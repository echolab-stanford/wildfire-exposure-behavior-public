if (!dir.exists(file.path(path_infiltration, "bootstraps"))) dir.create(file.path(path_infiltration, "bootstraps"))

#-------------------------------------------------------------------------------
# Figure 4 Panels a-b
# Written by: Sam Heft-Neal
# Requires large computer memory.
#-------------------------------------------------------------------------------
#### Read in and clean up briefly ####
# Read in data and rename pm25_out
data <- read_rds(file.path(path_purpleair, "analysis_data_clean_all.rds")) %>% mutate(pm25_out = pm25_out_mean)

# Add woy and doy so we have option to include them as FE 
data <- data %>% mutate(
  woy = lubridate::week(time_hours),
  dow = lubridate::wday(time_hours),
  mos = paste(month, year,sep ="-")
) %>% rename(hod = hour)

# Throw out station with insufficient number of obs. Here throwing out everything with less than 100 obs (ie nonmissing for in and out)
# Also throw out stations with no variance either in indoor or outdoor
count_obs <- data %>% group_by(ID_in) %>% summarise(n_obs = sum(!is.na(pm25_in) & !is.na(pm25_out)), var_in = sd(pm25_in, na.rm = T), var_out = sd(pm25_out, na.rm = T))
drop_ids <- count_obs %>% dplyr::filter(n_obs<200 | var_in < 1 | var_out <1 ) %>% dplyr::select(ID_in) %>% unlist()
data <- data %>% dplyr::filter(ID_in %in% drop_ids == F)

# Get ACS info
if (!file.exists(file.path(path_purpleair, "purpleAir_indoor_acs_medianIncome.rds"))) {
  states <- tigris::states() %>% st_as_sf()
  monloc <- data %>% dplyr::select(ID_in, Lon_in, Lat_in) %>% distinct() %>% dplyr::filter(!is.na(Lon_in))
  monloc_sf <- sfheaders::sf_point(monloc[,c("Lon_in","Lat_in")])
  monloc_sf$ID_in <- monloc$ID_in
  st_crs(monloc_sf)<-st_crs(states)
  monloc_sf <- st_join(monloc_sf, states) %>% dplyr::select(ID_in,state = NAME)
  
  states_list <- sort(unique(monloc_sf$state))
  monloc_sf <- monloc_sf %>% mutate(medincome = NA)
  monloc_df <- monloc_sf %>% as.data.frame() %>% dplyr::select(-geometry)
  
  options(tigris_use_cache = TRUE)
  
  for(i in 1:length(states_list)){
    statect <- tidycensus::get_acs(state = states_list[i], geography = "tract", variables = c(medincome = "B19013_001"), year = 2018, geometry = TRUE)
    monloc_df[(monloc_df$state==states_list[i]),"medincome"] <- as.data.frame(statect[as.numeric(st_within(monloc_sf[monloc_sf$state==states_list[i],], statect)),"estimate"])[,"estimate"]
  }
  
  states_list2 <- sort(unique(monloc_df$state[is.na(monloc_df$medincome)]))
  
  for(i in 1:length(states_list2)){
    statect <- tidycensus::get_acs(state = states_list2[i], geography = "tract", variables = c(medincome = "B19013_001"), year = 2016, geometry = TRUE)
    monloc_df[(monloc_df$state==states_list2[i] & is.na(monloc_df$medincome)),"medincome"] <- as.data.frame(statect[as.numeric(st_within(monloc_sf[monloc_sf$state==states_list2[i] & is.na(monloc_df$medincome),], statect)),"estimate"])[,"estimate"]*1.05
  }
  
  states_list3 <- sort(unique(monloc_df$state[is.na(monloc_df$medincome)]))
  
  for(i in 1:length(states_list3)){
    statect <- tidycensus::get_acs(state = states_list3[i], geography = "tract", variables = c(medincome = "B19013_001"), year = 2010, geometry = TRUE)
    monloc_df[(monloc_df$state==states_list3[i] & is.na(monloc_df$medincome)),"medincome"] <- as.data.frame(statect[as.numeric(st_within(monloc_sf[monloc_sf$state==states_list3[i] & is.na(monloc_df$medincome),], statect)),"estimate"])[,"estimate"]*1.1521
  }
  
  options(tigris_use_cache = FALSE)
  
  write_rds(monloc_df, file = file.path(path_purpleair, "purpleAir_indoor_acs_medianIncome.rds"))
}

income <- read_rds(file.path(path_purpleair, "purpleAir_indoor_acs_medianIncome.rds"))

# Bring in income data
data <- left_join(data, income) #%>% dplyr::filter(!is.na(income_median))

#-------------------------------------------------------------------------------
#### Run regressions ####
# Regression plan:
# (1) Run the distributed lag on a pooled model to see how many hours it takes for infiltration to occur.  
#     Fix that as # of lags.
# (2) Run all monitor specific regressions with them.  
# (3) Then we save sum of lags, and SE on that, and number of obs that contributed to regression. 
#     We can then thing of weighting NL/PA comparison by uncertainty in PA estimates.

## Define panel structure
# data <- data %>% dplyr::filter(!is.na(pm25_in) & !is.na(pm25_out))
pandat <- panel(data, panel.id = c("ID_in","time_hours"), duplicate.method = "first") 
rm(data) 

# Only want to include single family residences:
sfr <- read_rds(file.path(path_infiltration, "estimates", "purpleair_infiltration_estimates_by_model.rds")) %>% 
  rename(ID_in = id) %>% 
  dplyr::select(ID_in, building_type)
pandat <- left_join(pandat, sfr) %>% dplyr::filter(building_type == "sfr")

pandat$pm25_out[pandat$pm25_out < -5 ]<- NA
pandat$pm25_out[pandat$pm25_out > 1000 ]<- NA
pandat$pm25_in[pandat$pm25_in < -5 ]<- NA
pandat$pm25_in[pandat$pm25_in > 1000 ]<- NA
pandat$pm25_out_pc_mean[pandat$pm25_out_pc_mean < -5 ]<- NA
pandat$pm25_out_pc_mean[pandat$pm25_out_pc_mean > 1000 ]<- NA
pandat$pm25_in_pc[pandat$pm25_in_pc < -5 ]<- NA
pandat$pm25_in_pc[pandat$pm25_in_pc > 1000 ]<- NA

smoke <- read_rds(file.path(path_purpleair, "smoke_by_PAmonitor_density.rds")) %>% rename(day = dom)
pandat <- left_join(pandat, smoke)
pandat$heavy = as.numeric(pandat$density==27)

### [3] Estimate response across outdoor PM

# Define variables for estimating non-linearities:
pandat$income <- pandat$medincome
pandat$income2 <- pandat$medincome^2
pandat$income3 <- pandat$medincome^3
pandat$income4 <- pandat$medincome^4  

## Short-run PM
smoke_day <- read_rds(file.path(path_purpleair, "smoke_by_PAmonitor.rds")) %>% rename(day = dom)

pandat <- left_join(pandat, smoke_day)

pandat$pm25_out2 <- pandat$pm25_out^2
pandat$pm25_out3 <- pandat$pm25_out^3
pandat$pm25_out4 <- pandat$pm25_out^4

# x_pm <- 0:17
# bootsamp_pm <- matrix(nrow = 4, ncol = 100)

if (!file.exists(file.path(
  path_infiltration, "bootstraps", 
  "infiltration_smoke_pm_bootstrap_run_results_smokeday.rds"))) {
  var <- sort(unique(pandat$ID_in))
  
  bootsamp_pm <- list()
  
  for(i in 1:100){
    var_s <- sample(var, size = length(var), replace = T)
    
    data_s <- left_join(data.frame(ID_in = var_s), pandat[,c("pm25_in","pm25_out","pm25_out2","pm25_out3","pm25_out4","smoke_day","ID_in","dow","hod","mos","time_hours")])
    data_s <- panel(data_s, panel.id = c("ID_in","time_hours"), duplicate.method = "first")
    
    mod_s <- feols(pm25_in ~ l(pm25_out,0:3) + l(pm25_out2, 0:3) + l(pm25_out3, 0:3) + l(pm25_out4, 0:3) + (l(pm25_out,0:3) + l(pm25_out2, 0:3) + l(pm25_out3, 0:3) + l(pm25_out4, 0:3)):smoke_day | ID_in + dow + hod + mos, data = data_s )
    
    bootsamp_pm[[i]] <- coef(mod_s)
    print(i)
    
  }
  write_rds(bootsamp_pm, file = file.path(
    path_infiltration, "bootstraps", 
    "infiltration_smoke_pm_bootstrap_run_results_smokeday.rds"))
}

## [3] Income

# xi <- seq(22000,250000, 1000)

pandat$income_smoke <- pandat$income*pandat$heavy
if (!file.exists(file.path(
  path_infiltration, "bootstraps", 
  "infiltration_smoke_income_bootstrap_run_results_linear_smokeday.rds"))) {
  bootsamp_inc <- list()
  
  var <- sort(unique(pandat$ID_in))
  for(i in 1:100){
    var_s <- sample(var, size = length(var), replace = T)
    
    data_s <- left_join(data.frame(ID_in = var_s), pandat[,c("pm25_in","pm25_out","income","smoke_day","time_hours","ID_in","dow","hod","mos")])
    data_s <- panel(data_s, panel.id = c("ID_in","time_hours"), duplicate.method = "first")
    
    mod_s <- feols(pm25_in ~ l(pm25_out,0) + I(l(pm25_out,0)*income) + I(l(pm25_out, 0)*income*smoke_day) | ID_in + dow + hod + mos, data = data_s )
    
    bootsamp_inc[[i]] <- coef(mod_s)
    print(i)
    
  }
  write_rds(bootsamp_inc, file = file.path(
    path_infiltration, "bootstraps", 
    "infiltration_smoke_income_bootstrap_run_results_linear_smokeday.rds"))
}

#-------------------------------------------------------------------------------
#### Load data for figure ####
## Panel a - PM by smoke
bs_pm_list <- read_rds(file.path(
  path_infiltration, "bootstraps", 
  "infiltration_smoke_pm_bootstrap_run_results_smokeday.rds"))

xp1 <- (round(quantile(pandat$pm25_out[pandat$heavy==1], .01 , na.rm = T))):(round(quantile(pandat$pm25_out[pandat$heavy==1], .975 , na.rm = T)))
xp0 <- (round(quantile(pandat$pm25_out[pandat$heavy==0], .01 , na.rm = T))):(round(quantile(pandat$pm25_out[pandat$heavy==0], .99 , na.rm = T)))

get_y_pm <- function(x, coefs, smoke = 0){
  sum(coefs[1:4]) + 2*x*sum(coefs[5:8]) + 3*x^2*sum(coefs[9:12]) + 4*x^3*sum(coefs[13:16]) + 
    smoke*(
      sum(coefs[17:20]) + 2*x*sum(coefs[21:24]) + 3*x^2*sum(coefs[25:28]) + 4*x^3*sum(coefs[29:32])
    )  
}

bs_pm1 <- matrix(nrow = length(xp1), ncol = 100)
for(i in 1:100){
  bs_pm1[,i] <- get_y_pm(xp1, bs_pm_list[[i]], 1)
}

bs_pm0 <- matrix(nrow = length(xp0), ncol = 100)
for(i in 1:100){
  bs_pm0[,i] <- get_y_pm(xp0, bs_pm_list[[i]], 0)
}

yp1 <- apply(bs_pm1, 1, function(x){quantile(x, .5)})
yp1_low <- apply(bs_pm1, 1, function(x){quantile(x, .025)})
yp1_high <- apply(bs_pm1, 1, function(x){quantile(x, .975)})

yp0 <- apply(bs_pm0, 1, function(x){quantile(x, .5)})
yp0_low <- apply(bs_pm0, 1, function(x){quantile(x, .025)})
yp0_high <- apply(bs_pm0, 1, function(x){quantile(x, .975)})

histbin <- statar::xtile(pandat$pm25_out[pandat$heavy==1], cutpoints = seq(0,300,2))
hist_ht_p1 <- data.frame(xleft = seq(0,302,2)-1,xright = seq(0,302,2)+1, ht = as.numeric(table(histbin)))
hist_ht_p1[nrow(hist_ht_p1),"ht"]<-hist_ht_p1[nrow(hist_ht_p1),"ht"]/2

histbin <- statar::xtile(pandat$pm25_out[pandat$heavy==0], cutpoints = seq(0,74,2))
hist_ht_p0 <- data.frame(xleft = seq(0,76,2)-1,xright = seq(0,76,2)+1, ht = as.numeric(table(histbin)))

## Panel b - income by smoke (income doesn't really differ by smoke)
bs_inc_list <- read_rds(file.path(
  path_infiltration, "bootstraps", 
  "infiltration_smoke_income_bootstrap_run_results_linear_smokeday.rds"))
xi1 <- seq(37000,235000, 2000)
xi0 <- seq(37000,235000, 2000)

get_y_inc <- function(x, coefs, smoke = 0){
  coefs[1] + coefs[2]*x + coefs[3]*x*smoke 
}  

bs_i1 <- matrix(nrow = length(xi1), ncol = 100)
for(i in 1:100){
  bs_i1[,i] <- get_y_inc(xi1, bs_inc_list[[i]], 1)
}

bs_i0 <- matrix(nrow = length(xi0), ncol = 100)
for(i in 1:100){
  bs_i0[,i] <- get_y_inc(xi0, bs_inc_list[[i]], 0)
}

yi0 <- apply(bs_i0, 1, function(x){quantile(x, .5)})
yi0_low <- apply(bs_i0, 1, function(x){quantile(x, .025)})
yi0_high <- apply(bs_i0, 1, function(x){quantile(x, .975)})

yi1 <- apply(bs_i1, 1, function(x){quantile(x, .5)})
yi1_low <- apply(bs_i1, 1, function(x){quantile(x, .025)})
yi1_high <- apply(bs_i1, 1, function(x){quantile(x, .975)})

histbin2 <- statar::xtile(pandat$income[pandat$heavy==0], cutpoints = seq(50000,250000,5000)); histbin2 <- c(as.numeric(table(histbin2))[1:38], 0, as.numeric(table(histbin2))[39:41])
hist_inc0 <- data.frame(xleft = seq(50000,255000,5000)-2500,xright = seq(50000,255000,5000)+2500, ht = histbin2 )

plot_poly <- function(x,ylow, yhigh, col){
  polygon(x = c(x[1], x, x[length(x)], rev(x)),y = c(ylow[1], yhigh, ylow[length(ylow)],rev(ylow) ), border = NA, col = col)
}

#-------------------------------------------------------------------------------
#### Calculations reported in paper ####
# How many monitors in final regression?
if (!file.exists(file.path(path_purpleair, "final_monitor_list.rds"))) {
  final_monitor_list <- unique(pandat$ID_in)
  length(final_monitor_list)
  write_rds(final_monitor_list, file = file.path(path_purpleair, "final_monitor_list.rds")) # Write out to make sure elsewhere we are using same set of monitors
}

# Average infiltration from pooled model
pandat <- panel(pandat, panel.id = c("ID_in","time_hours"), duplicate.method = "first") # After a bunch of merges and stuff need to re-define panel structure for feols
pooled_model <- feols(pm25_in ~ l(pm25_out,0:6) | ID_in + dow + hod + mos, data = pandat)

# Estimate + 95% CI:
coef <- coefficients(pooled_model)
est <- sum(coef[grep(x = names(coef), pattern = "pm25_out")])
se <- summary(glht(pooled_model, linfct = paste("pm25_out +","`",paste0("l(pm25_out, ",1:6,")", collapse = "`+`"),"`", "=0",sep = "")))$test$sigma %>% as.numeric()
ci <- c(est + qnorm(0.025)*se, est + qnorm(0.975)*se)

#-------------------------------------------------------------------------------
#### Plot Top Row ####
poly_col0 <-add.alpha(wes_palette("Zissou1")[2], .6)
poly_col1 <-add.alpha(wes_palette("Zissou1")[3], .75)

pdf(file.path(path_figures, "figure04a-b.pdf"), width =12.5, height = 6)

par(mar = c(4,5,2,2))
par(mfrow = c(1,2))

## Panel a
plot(0,0,axes=F, xlab = "",ylab = "",pch = 16, cex=2, ylim = c(0, .3), col = NA, xlim =c(5,305))

# smoke == 0
plot_poly(xp0, yp0_low, yp0_high, poly_col0)
lines(xp0, yp0, lwd = 3)

# smoke == 1
plot_poly(xp1, yp1_low, yp1_high, poly_col1)
lines(xp1, yp1, lwd = 3)

axis(2, tick = T,las =2, cex.axis = 1.4)
axis(1, at = seq(0,300,50), cex.axis=1.4)
mtext(side = 1, text = "Outdoor PM2.5 (ug/m3)",line=3,cex=1.5)
mtext(side = 2, text = "Infiltration Rate",line=4,cex=1.5)

plotHist(hist_ht_p0, poly_col0, .75, 0.015, 0.12, max(hist_ht_p0$ht), add.alpha(poly_col0, .05) )
plotHist(hist_ht_p1, poly_col1, .75, 0,0.01, max(hist_ht_p1$ht), add.alpha(poly_col1, .05) )

## Panel b
plot(0, 0, axes=F, xlab = "",ylab = "",pch = 16, cex=2, ylim = c(0, .3), col = NA, xlim =c(30000,260000))

# smoke == 1
plot_poly(xi1, yi1_low, yi1_high, poly_col1)
lines(xi1,yi1, lwd = 3)

# smoke == 0
plot_poly(xi0, yi0_low, yi0_high, poly_col0)
lines(xi0,yi0, lwd = 3)

axis(2, tick = T,las =2, cex.axis = 1.4)
axis(1, tick = T, at = seq(50000,250000,50000),labels = seq(50000,250000,50000)/1000, cex.axis =1.4)
mtext(side = 1, text = "median household income ($1000)",line=3,cex=1.5)

plotHist(hist_inc0, 'gray80', .75, 0,0.05, max(hist_inc0$ht), add.alpha('white', .05) )

dev.off()

#-------------------------------------------------------------------------------
# Figure 4 Panel c
# Written by: Sam Heft-Neal
#-------------------------------------------------------------------------------
# Histogram of infiltration
inf_dist <- read_rds(file.path(path_infiltration, "estimates", "PA_monitor_level_infiltration_estimates_sfr_clean_pc.rds"))

# Set up data frame with info needed to make plot (I do barplots instead of histogram here just because I have more control)
bincounts <- statar::xtile(inf_dist$est_dl, cutpoints = seq(.025,.575,.025)) %>% table() %>% as.numeric() #create counts of data in each bin
bardata <- data.frame(xleft = seq(.025,.6,.025)-.0125,xright = seq(.025,.6,.025)+.0125, ht = bincounts) #data frame with cols for bar left, right, and height
bardata[nrow(bardata),"ht"]<-33 #fix wrong count in top bin

# Plot
pdf(file.path(path_figures, "figure04c.pdf"), width =11, height = 3)

par(mar =c(3,3,0,0))

plot(0, 0, axes=F, xlab = "",ylab = "",pch = 16, cex=2, ylim = c(0, 200), col = NA, xlim =c(0,0.6)) #empty plot

plotHist(bardata, 'gray80', .5, 0,max(bardata$ht),max(bardata$ht), add.alpha('gray50', .25)) #call user defined function for plotting hists at bottom of response curves 

axis(1, at = seq(0, 0.6, .05),cex.axis = 1.1)
axis(2, las = 2, cex.axis = 1.1)

mtext(side = 1, text = "Infiltration Rate",line=3,cex=1.1)
mtext(side = 2, text = "Number of Residences",line=4,cex=1.1)

dev.off()

#-------------------------------------------------------------------------------
# Figure 4 Panels d-f
# Written by: Sam Heft-Neal
# Requires large computer memory.
#-------------------------------------------------------------------------------
#### Read in and clean up briefly ####
# Read in data and rename pm25_out
data <- read_rds(file.path(path_purpleair, "analysis_data_clean_all.rds")) %>% rename(pm25_out = pm25_out_mean)

# Add woy and doy so we have option to include them as FE 
data <- data %>% mutate(
  woy = lubridate::week(time_hours),
  dow = lubridate::wday(time_hours),
  mos = paste(month, year,sep ="-")
) %>% rename(hod = hour)

# Throw out station with insufficient number of obs. Here throwing out everything with less than 100 obs (ie nonmissing for in and out)
# Also throw out stations with no variance either in indoor or outdoor
count_obs <- data %>% group_by(ID_in) %>% summarise(n_obs = sum(!is.na(pm25_in) & !is.na(pm25_out)), var_in = sd(pm25_in, na.rm = T), var_out = sd(pm25_out, na.rm = T))
drop_ids <- count_obs %>% dplyr::filter(n_obs<200 | var_in < 1 | var_out <1 ) %>% dplyr::select(ID_in) %>% unlist()
data <- data %>% dplyr::filter(ID_in %in% drop_ids == F)

#### Define panel structure ####
pandat <- panel(data, panel.id = c("ID_in","time_hours"), duplicate.method = "first") 

# Bring in monitor level info on state      
stlist <- read_rds(file.path(path_purpleair, "us_sensor_list.rds"))
stlist <- stlist[stlist$ID %in% pandat$ID_in,]
stlist <- stlist %>% dplyr::select(ID_in = ID, state)
pandat <- left_join(pandat, stlist)  

# Bring in monitor building classification so we can limit to single family residences (SFR)
sfr <- read_csv(file.path(path_purpleair, "locationTypeCategorizing", "locationTypeCategorized.csv")) %>% rename(building_type = type)  

# Subset to monitors in the bay area
stlist <- read_rds(file.path(path_purpleair, "us_sensor_list.rds")) %>% dplyr::filter(ID %in% unique(pandat$ID_in))

ca <- read_rds(file.path(path_boundaries, "ca_county_boundaries.rds"))
bay <- ca[ca@data$NAME_2 %in% c("Alameda","Contra Costa","Marin","San Francisco","San Mateo","Sonoma","Napa","Solano","Santa Cruz","Santa Clara"),]

station_loc <- SpatialPoints(stlist[,c("Lon","Lat")])
crs(station_loc)<-crs(bay)

overlap <- over(station_loc, bay)

bay_stations <- stlist[!is.na(overlap$PID),]

### SLOW #######################################################################
# For each monitor estimate infiltration leaving out this wildfire period    
if(!file.exists(file.path(path_infiltration, "estimates", "figure4_purpleair_infiltration_estimates.rds"))){      
  
  #run lagged dependent variable model omitting this wildfire period
  run_reg_ldv <- function(id){ 
    
    dat <- dplyr::filter(pandat, ID_in == id & (time_hours <= "2020-08-08" | time_hours >= "2020-09-26")) 
    dat <- panel(dat, panel.id = c("ID_in","time_hours"), duplicate.method = "first")
    fmla <- as.formula(paste("pm25_in ~ l(pm25_in, 1) + l(pm25_out, 0)  | hod + woy + dow + mos",sep=""))
    mod <- feols(fmla , data = dat)
    est <-  sum(coef(mod)["pm25_out"]*(coef(mod)["l(pm25_in, 1)"])^(0:1000)) 
    # se <- summary(glht(mod, linfct = paste("pm25_out +","`",paste0("l(pm25_out, ",1:n_lags,")", collapse = "`+`"),"`", "=0",sep = "")))$test$sigma %>% as.numeric()
    # est1 <- (coef(mod)[grep(x= names(coef(mod)), pattern = "Temp")])
    n <- summary(mod)$nobs
    return(data.frame(id = id, est_ldv = est,  N_ldv = n))          
  }
  
  stations <- bay_stations %>% dplyr::select(ID) %>% unlist() #only run for bay area monitors
  res <- Map(function(x) try(run_reg_ldv(x)), stations) #apply reg across monitors
  index <- lapply(res, function(x){nrow(x)>0}) %>% unlist(); table(index) #check that all regs worked
  
  # These monitors don't have enough obs to run reg (should automate dropping)
  res <- res[c(1:6,8:length(res))]
  res <- res[c(1:632,634:length(res))]
  res <- res[c(1:632,634:length(res))]
  res <- res[c(1:881,883:length(res))]
  res <- res[c(1:1247,1249:length(res))]
  res <- res[c(1:1571,1573:length(res))]
  
  
  res_ldv <- data.frame(data.table::rbindlist(res)) # Combine all results into single data frame
  
  write_rds(res_ldv, file = file.path(path_infiltration, "estimates", "figure4_purpleair_infiltration_estimates.rds"))
} else {
  res_ldv <- read_rds(file.path(path_infiltration, "estimates", "figure4_purpleair_infiltration_estimates.rds"))
}
################################################################################

# Prepare monitor-level infiltration estimates for plotting
res <- res_ldv %>% filter(N_ldv >= 720 & est_ldv > 0 & est_ldv < 1 & id %in% bay_stations$ID) %>% #limit to estimates that had at least 720 = 24 hrs * 30 days of data
  rename(est = est_ldv, N = N_ldv) %>% 
  left_join(sfr) %>% 
  dplyr::filter(building_type == "sfr") %>% # Limit to SFRs only 
  mutate(inf_group = 2, # Identify top and bottom quartiles of infiltration within Bay Area
         inf_group = replace(inf_group, est < quantile(est, .25), 1),
         inf_group = replace(inf_group, est > quantile(est, .75), 3)
  ) %>% 
  rename(ID_in = id)

# Subset hourly data to stations and time periods of interest for time-series plotting
pandat <- pandat %>% 
  dplyr::filter(ID_in %in% res$ID_in) %>%    # Limit data to monitors in bay
  left_join(res[,c("ID_in","inf_group")]) %>% 
  dplyr::filter(inf_group %in% c(1,3)) %>% 
  dplyr::filter(time_hours > "2020-08-08" & time_hours < "2020-09-26") # Limit to relevant time period

#-------------------------------------------------------------------------------
## Map of monitors by low/high infiltration
## Time series of indoor & outdoor divided into low/high infiltration
ids <- pandat %>% as.data.frame() %>% dplyr::select(ID_in, inf_group) %>% distinct()
ids1 <- ids %>% dplyr::filter(inf_group==1) # Identify low infiltration ids
ids3 <- ids %>% dplyr::filter(inf_group==3) # Identify high infiltration ids

# Calculate daily averages within each infiltration group
aves <- pandat %>% 
  group_by(inf_group, year, month, day) %>% 
  summarise(pm_in = mean(pm25_in, na.rm = T),
            pm_out = mean(pm25_out, na.rm = T)) %>% 
  mutate(time_hours = as.Date(paste(year,"-", month,"-", day, sep="")))

# Calculate daily average for each monitor
avesmon <- pandat %>% 
  group_by(ID_in, year, month, day, inf_group) %>% 
  summarise(pm_in = mean(pm25_in, na.rm = T),pm_out = mean(pm25_out, na.rm = T)) %>% 
  mutate(time_hours = as.Date(paste(year,"-", month,"-", day, sep=""))) %>% 
  drop_na(pm_in, pm_out)

# Separate data by infiltration group
aves1 <- aves %>% dplyr::filter(inf_group==1)
aves3 <- aves %>% dplyr::filter(inf_group==3)

avesmon1 <- avesmon %>% dplyr::filter(inf_group==1)
avesmon3 <- avesmon %>% dplyr::filter(inf_group==3)

# Create variable counting n obs so we can drop really small samples
avesmon1 <- avesmon1 %>% group_by(ID_in) %>% 
  summarise(n = sum(!is.na(pm_in) & !is.na(pm_out))) %>% 
  right_join(avesmon1) %>% 
  dplyr::filter(n>40)

avesmon3 <- avesmon3 %>% group_by(ID_in) %>% 
  summarise(n = sum(!is.na(pm_in)& !is.na(pm_out))) %>% 
  right_join(avesmon3) %>% 
  dplyr::filter(n>40)

# Colors and line weight
LWDa <- 2
COL1 <- 'navy'
COL2 <- 'red3'

# Plot
pdf(file = file.path(path_figures, "figure04d-f.pdf"),width =12.5, height = 7.5)  

layout(matrix(byrow = T,nrow = 4, ncol = 4, data=c(1,1,2,2,1,1,2,2,1,1,3,3,1,1,3,3) )  )

par(mar = c(6,6,6,6))

# Map panel
plot(bay, col = 'gray92')
plot(ca, add=T)

points(res$lon[res$inf_group==1], res$lat[res$inf_group==1], pch = 21, bg = add.alpha('navy', 0.5), col = add.alpha('white', .5),cex=1.25, lwd =.25)
points(res$lon[res$inf_group==3], res$lat[res$inf_group==3], pch = 21, bg = add.alpha('red3', 0.5), col = add.alpha('white', 0.5),cex=1.25, lwd =.25)

legend(x = "bottomleft",fill = c("navy","red3"),legend = c("Low","High"),title = "Household Infiltration",bty = "n",  cex = 2, border = c("navy","red3"))

mtext(side = 3, text = "Monitor Locations",adj = 0, cex=2)

# Outdoor and indoor time series 
# Outdoor
par(mar = c(0,4,5,0))
plot(aves1$time_hours, aves1$pm_out,type = "l", ylim = c(0,300),axes = F, xlab = "",ylab = "", col = NA)

lines(aves1$time_hours, aves1$pm_out, col= 'navy', lwd = 2.5,lty=1)
lines(aves3$time_hours, aves3$pm_out, col = 'red3', lwd = 2.5,lty=1)

for (i in 1:nrow(ids1)){lines(avesmon1$time_hours[avesmon1$ID_in==ids1[i,1]], avesmon1$pm_out[avesmon1$ID_in==ids1[i,1]], col = add.alpha('navy', .1))}
for (i in 1:nrow(ids3)){lines(avesmon3$time_hours[avesmon3$ID_in==ids3[i,1]], avesmon3$pm_out[avesmon3$ID_in==ids3[i,1]], col = add.alpha('red3', .1))}

axis(2, tick = T,las=2, at = seq(0,300,50))

mtext(adj = 0, text = "Outdoor PM2.5",cex=2,line=1)
mtext(side = 2, text = "ug/m3",cex=1.25, line=3.5)

# Indoor
par(mar = c(4,4,5,0))
plot(aves1$time_hours, aves1$pm_in,type = "l", ylim = c(0,150),axes = F, xlab = "",ylab = "", col = NA)

lines(aves1$time_hours, aves1$pm_in, col= 'navy', lwd = 2)
lines(aves3$time_hours, aves3$pm_in, col = 'red3', lwd = 2)

for (i in 1:nrow(ids1)){lines(avesmon1$time_hours[avesmon1$ID_in==ids1[i,1]], avesmon1$pm_in[avesmon1$ID_in==ids1[i,1]], col = add.alpha('navy', .1))}
for (i in 1:nrow(ids3)){lines(avesmon3$time_hours[avesmon3$ID_in==ids3[i,1]], avesmon3$pm_in[avesmon3$ID_in==ids3[i,1]], col = add.alpha('red3', .1))}

axis(2, tick = T,las=2, at = seq(0,150,50))
axis.Date(1, aves3$time_hours, at = seq(as.Date("2020/8/8"),as.Date("2020/9/28"),"weeks"))

mtext(adj = 0, text = "Indoor PM2.5",cex=2,line=1)
mtext(side = 1, text = 2020, cex=1.25,line=3.5)      
mtext(side = 2, text = "ug/m3",cex=1.25, line=3.5)

dev.off()

#### Calculate daily mean exposure ####
out_low <- avesmon1 %>% group_by(ID_in) %>% summarise(pm_out = mean(pm_out, na.rm = T)) %>% ungroup() %>% summarise(pm_out = mean(pm_out, na.rm = T)) %>% as.numeric()
out_high <- avesmon3 %>% group_by(ID_in) %>% summarise(pm_out = mean(pm_out, na.rm = T)) %>% ungroup() %>% summarise(pm_out = mean(pm_out, na.rm = T))%>% as.numeric()
in_low <- avesmon1 %>% group_by(ID_in) %>% summarise(pm_in = mean(pm_in, na.rm = T)) %>% ungroup() %>% summarise(pm_in = mean(pm_in, na.rm = T)) %>% as.numeric()
in_high <- avesmon3 %>% group_by(ID_in) %>% summarise(pm_in = mean(pm_in, na.rm = T)) %>% ungroup() %>% summarise(pm_in = mean(pm_in, na.rm = T)) %>% as.numeric()

out_low1 <- avesmon1 %>% group_by(ID_in) %>% summarise(pm_out = mean(pm_out, na.rm = T)) %>% arrange(pm_out)
out_high1 <- avesmon3 %>% group_by(ID_in) %>% summarise(pm_out = mean(pm_out, na.rm = T)) %>% arrange(pm_out)
in_low1 <- avesmon1 %>% group_by(ID_in) %>% summarise(pm_in = mean(pm_in, na.rm = T))%>% arrange(pm_in)
in_high1 <- avesmon3 %>% group_by(ID_in) %>% summarise(pm_in = mean(pm_in, na.rm = T)) %>% arrange(pm_in)

# Plot the right parts of panel
pdf(file.path(path_figures, "figure04e-f_right.pdf"),width = 1.5, height = 4)
par(mfrow = c(2,1))
par(mar = c(2,2,2,2))

plot(0, 0, col = NA, axes = F, xlab = "",ylab = "", xlim = c(0,1.75), ylim = c(0, 80))
segments(x0 = .25, x1 = 0.75, y0 = out_low1$pm_out, col =add.alpha('navy', .4), lwd = 0.5)
segments(x0 = .25, x1 = 0.75, y0 = out_low, col =add.alpha('navy', 1),lwd=2)
segments(x0 = 1, x1 = 1.5, y0 = out_high1$pm_out, col =add.alpha('red3', .4), lwd = 0.5)
segments(x0 = 1, x1 = 1.5, y0 = out_high, col =add.alpha('red3', 1),lwd=2)
axis(4, at = seq(0,80, 20), las = 2)

plot(0, 0, col = NA, axes = F, xlab = "",ylab = "", xlim = c(0,1.75), ylim = c(0, 40))
segments(x0 = .25, x1 = 0.75, y0 = in_low1$pm_in, col =add.alpha('navy', .4), lwd = 0.5)
segments(x0 = .25, x1 = 0.75, y0 = in_low, col =add.alpha('navy', 1),lwd=2)
segments(x0 = 1, x1 = 1.5, y0 = in_high1$pm_in, col =add.alpha('red3', .4), lwd = 0.5)
segments(x0 = 1, x1 = 1.5, y0 = in_high, col =add.alpha('red3', 1),lwd=2)
axis(4, at = seq(0,40, 10), las = 2)

dev.off()

#-------------------------------------------------------------------------------
# Text comparison
out_low <- rbind(avesmon1, avesmon3) %>% group_by(ID_in) %>% summarise(pm_out = mean(pm_out, na.rm = T)) %>% ungroup() %>% summarise(pm_out = mean(pm_out, na.rm = T)) %>% as.numeric()
out_high <- avesmon3 %>% group_by(ID_in) %>% summarise(pm_out = mean(pm_out, na.rm = T)) %>% ungroup() %>% summarise(pm_out = mean(pm_out, na.rm = T))%>% as.numeric()
in_low <- avesmon1 %>% group_by(ID_in) %>% summarise(pm_in = mean(pm_in, na.rm = T)) %>% ungroup() %>% summarise(pm_in = mean(pm_in, na.rm = T)) %>% as.numeric()
in_high <- avesmon3 %>% group_by(ID_in) %>% summarise(pm_in = mean(pm_in, na.rm = T)) %>% ungroup() %>% summarise(pm_in = mean(pm_in, na.rm = T)) %>% as.numeric()

out_low1 <- avesmon1 %>% group_by(ID_in) %>% summarise(pm_out = mean(pm_out, na.rm = T)) %>% arrange(pm_out)
out_high1 <- avesmon3 %>% group_by(ID_in) %>% summarise(pm_out = mean(pm_out, na.rm = T)) %>% arrange(pm_out)
in_low1 <- avesmon1 %>% group_by(ID_in) %>% summarise(pm_in = mean(pm_in, na.rm = T))%>% arrange(pm_in)
in_high1 <- avesmon3 %>% group_by(ID_in) %>% summarise(pm_in = mean(pm_in, na.rm = T)) %>% arrange(pm_in)

# Text comparison 2 - by PM bin
# avesmon defined above gives daily monitor level averages during the period
period_ave <- avesmon %>% group_by(ID_in, inf_group) %>% summarise(pm_in = mean(pm_in), pm_out = mean(pm_out))
