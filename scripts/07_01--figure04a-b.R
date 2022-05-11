##################################
## read in and clean up briefly ##
##################################


#read in data and rename pm25_out
data <- read_rds(file.path(path_infiltration, "analysis_data_clean_all.rds")) %>% mutate(pm25_out = pm25_out_mean)

#add woy and doy so we have option to include them as FE 
data <- data %>% mutate(
  woy = lubridate::week(time_hours),
  dow = lubridate::wday(time_hours),
  mos = paste(month, year,sep ="-")
) %>% rename(hod = hour)

#throw out station with insufficient number of obs. Here throwing out everything with less than 100 obs (ie nonmissing for in and out)
#also throw out stations with no variance either in indoor or outdoor
count_obs <- data %>% group_by(ID_in) %>% summarise(n_obs = sum(!is.na(pm25_in) & !is.na(pm25_out)), var_in = sd(pm25_in, na.rm = T), var_out = sd(pm25_out, na.rm = T))
drop_ids <- count_obs %>% dplyr::filter(n_obs<200 | var_in < 1 | var_out <1 ) %>% dplyr::select(ID_in) %>% unlist()
data <- data %>% dplyr::filter(ID_in %in% drop_ids == F)

# #get acs info
# states <- tigris::states() %>% st_as_sf()
# monloc <- data %>% dplyr::select(ID_in, Lon_in, Lat_in) %>% distinct() %>% dplyr::filter(!is.na(Lon_in))
# monloc_sf <- sfheaders::sf_point(monloc[,c("Lon_in","Lat_in")])
# monloc_sf$ID_in <- monloc$ID_in
# st_crs(monloc_sf)<-st_crs(states)
# monloc_sf <- st_join(monloc_sf, states) %>% dplyr::select(ID_in,state = NAME)
# 
# states_list <- sort(unique(monloc_sf$state))
# monloc_sf <- monloc_sf %>% mutate(medincome = NA)
# monloc_df <- monloc_sf %>% as.data.frame() %>% dplyr::select(-geometry)
# 
# library(tidycensus)
# options(tigris_use_cache = TRUE)
# 
# 
# for(i in 1:length(states_list)){
#   
#   statect <- tidycensus::get_acs(state = states_list[i], geography = "tract", variables = c(medincome = "B19013_001"), year = 2018, geometry = TRUE)
#   
#   monloc_df[(monloc_df$state==states_list[i]),"medincome"] <- as.data.frame(statect[as.numeric(st_within(monloc_sf[monloc_sf$state==states_list[i],], statect)),"estimate"])[,"estimate"]
#   
# }
# 
# 
# 
# states_list2 <- sort(unique(monloc_df$state[is.na(monloc_df$medincome)]))
# 
# 
# 
# for(i in 1:length(states_list2)){
#   
#   statect <- tidycensus::get_acs(state = states_list2[i], geography = "tract", variables = c(medincome = "B19013_001"), year = 2016, geometry = TRUE)
#   
#   monloc_df[(monloc_df$state==states_list2[i] & is.na(monloc_df$medincome)),"medincome"] <- as.data.frame(statect[as.numeric(st_within(monloc_sf[monloc_sf$state==states_list2[i] & is.na(monloc_df$medincome),], statect)),"estimate"])[,"estimate"]*1.05
#   
# }
# 
# 
# 
# states_list3 <- sort(unique(monloc_df$state[is.na(monloc_df$medincome)]))
# 
# 
# 
# for(i in 1:length(states_list3)){
#   
#   statect <- tidycensus::get_acs(state = states_list3[i], geography = "tract", variables = c(medincome = "B19013_001"), year = 2010, geometry = TRUE)
#   
#   monloc_df[(monloc_df$state==states_list3[i] & is.na(monloc_df$medincome)),"medincome"] <- as.data.frame(statect[as.numeric(st_within(monloc_sf[monloc_sf$state==states_list3[i] & is.na(monloc_df$medincome),], statect)),"estimate"])[,"estimate"]*1.1521
#   
# }
# 
# 
# write_rds(monloc_df, file = file.path(path_infiltration, "data/purpleAir_indoor_acs_medianIncome.rds"))
# 


income <- read_rds(file.path(path_infiltration, "purpleAir_indoor_acs_medianIncome.rds"))



#bring in income data
data <- left_join(data, income) #%>% dplyr::filter(!is.na(income_median))


##################################
## Run regressions              ##
##################################

## Regression plan:
#(1) run the distributed lag on a pooled model to see how many hours it takes for infiltration to occur.  
#fix that as # of lags, and 
#(2) run all monitor specific regressions with them.  
#(3) then we save sum of lags, and SE on that, and number of obs that contributed to regression.  we can then thing of weighting NL/PA comparison by uncertainty in PA estimates.



### define panel structure

# data <- data %>% dplyr::filter(!is.na(pm25_in) & !is.na(pm25_out))
pandat <- panel(data, panel.id = c("ID_in","time_hours"), duplicate.method = "first") 
rm(data) 


#only want to include single family residences:
sfr <- read_rds(file.path(path_infiltration, "purpleair_infiltration_estimates_by_model.rds")) %>% rename(ID_in = id) %>% dplyr::select(ID_in, building_type)
pandat <- left_join(pandat, sfr) %>% dplyr::filter(building_type == "sfr")


pandat$pm25_out[pandat$pm25_out < -5 ]<- NA
pandat$pm25_out[pandat$pm25_out > 1000 ]<- NA
pandat$pm25_in[pandat$pm25_in < -5 ]<- NA
pandat$pm25_in[pandat$pm25_in > 1000 ]<- NA
pandat$pm25_out_pc_mean[pandat$pm25_out_pc_mean < -5 ]<- NA
pandat$pm25_out_pc_mean[pandat$pm25_out_pc_mean > 1000 ]<- NA
pandat$pm25_in_pc[pandat$pm25_in_pc < -5 ]<- NA
pandat$pm25_in_pc[pandat$pm25_in_pc > 1000 ]<- NA



smoke <- read_rds(file.path(path_infiltration, "smoke_by_PAmonitor_density.rds")) %>% rename(day = dom)
pandat <- left_join(pandat, smoke)
pandat$heavy = as.numeric(pandat$density==27)

### [3] Estimate response across outdoor PM

#define variables for estimating non-linearities:

pandat$income <- pandat$medincome
pandat$income2 <- pandat$medincome^2
pandat$income3 <- pandat$medincome^3
pandat$income4 <- pandat$medincome^4  



###  short-run PM ###    
smoke_day <- read_rds(file.path(path_infiltration, "smoke_by_PAmonitor.rds")) %>% rename(day = dom)

pandat <- left_join(pandat, smoke_day)

pandat$pm25_out2 <- pandat$pm25_out^2
pandat$pm25_out3 <- pandat$pm25_out^3
pandat$pm25_out4 <- pandat$pm25_out^4

# x_pm <- 0:17
#bootsamp_pm <- matrix(nrow = 4, ncol = 100)
#var <- sort(unique(pandat$ID_in))

# bootsamp_pm <- list()
# 
# for(i in 1:100){
#   var_s <- sample(var, size = length(var), replace = T)
# 
#   data_s <- left_join(data.frame(ID_in = var_s), pandat[,c("pm25_in","pm25_out","pm25_out2","pm25_out3","pm25_out4","heavy","ID_in","dow","hod","mos","time_hours")])
#   data_s <- panel(data_s, panel.id = c("ID_in","time_hours"), duplicate.method = "first") 
#   
#   mod_s <- feols(pm25_in ~ l(pm25_out,0:3) + l(pm25_out2, 0:3) + l(pm25_out3, 0:3) + l(pm25_out4, 0:3) + (l(pm25_out,0:3) + l(pm25_out2, 0:3) + l(pm25_out3, 0:3) + l(pm25_out4, 0:3)):heavy | ID_in + dow + hod + mos, data = data_s )
# 
#   bootsamp_pm[[i]] <- coef(mod_s)
#   print(i)
# 
# }
# write_rds(bootsamp_pm, file = "data/infiltration_smoke_pm_bootstrap_run_results.rds")


### [3] income ######

xi <- seq(22000,250000, 1000)


# bootsamp_inc <- list()
# pandat$income_smoke <- pandat$income*pandat$heavy
# 
# var <- sort(unique(pandat$ID_in))
# for(i in 1:100){
# var_s <- sample(var, size = length(var), replace = T)
# 
# data_s <- left_join(data.frame(ID_in = var_s), pandat[,c("pm25_in","pm25_out","income","heavy","time_hours","ID_in","dow","hod","mos")])
# data_s <- panel(data_s, panel.id = c("ID_in","time_hours"), duplicate.method = "first") 
# 
# mod_s <- feols(pm25_in ~ l(pm25_out,0) + I(l(pm25_out,0)*income) + I(l(pm25_out, 0)*income*heavy) | ID_in + dow + hod + mos, data = data_s )
# 
# 
# bootsamp_inc[[i]] <- coef(mod_s)
# print(i)
# 
# }
# 
# 
# write_rds(bootsamp_inc, file = file.path(path_infiltration, "infiltration_smoke_income_bootstrap_run_results_linear.rds"))
# 
# bootsamp_pm <- list()
# 
# for(i in 1:100){
#   var_s <- sample(var, size = length(var), replace = T)
#   
#   data_s <- left_join(data.frame(ID_in = var_s), pandat[,c("pm25_in","pm25_out","pm25_out2","pm25_out3","pm25_out4","heavy","ID_in","dow","hod","mos","time_hours")])
#   data_s <- panel(data_s, panel.id = c("ID_in","time_hours"), duplicate.method = "first") 
#   
#   mod_s <- feols(pm25_in ~ l(pm25_out,0:3) + l(pm25_out2, 0:3) + l(pm25_out3, 0:3) + l(pm25_out4, 0:3) + (l(pm25_out,0:3) + l(pm25_out2, 0:3) + l(pm25_out3, 0:3) + l(pm25_out4, 0:3)):heavy | ID_in + dow + hod + mos, data = data_s )
#   
#   bootsamp_pm[[i]] <- coef(mod_s)
#   print(i)
#   
# }
# write_rds(bootsamp_pm, file = "data/infiltration_smoke_pm_bootstrap_run_results.rds")
# 

#####################################################################################################################      
#####################################################################################################################
#####################################################################################################################

## load data for figure

#panel a - PM by smoke

bs_pm_list <- read_rds(file.path(path"~/Documents/GitHub/wildfire_home_leakage/data/infiltration_smoke_pm_bootstrap_run_results.rds")
                       xp1 <- (round(quantile(pandat$pm25_out[pandat$heavy==1], .01 , na.rm = T))):300#(round(quantile(pandat$pm25_out[pandat$heavy==1], .975 , na.rm = T)))
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
                       
                       
                       
                       #panel b - income by smoke (income doesn't really differ by smoke)
                       bs_inc_list <- read_rds("~/Documents/GitHub/wildfire_home_leakage/data/infiltration_smoke_income_bootstrap_run_results_linear.rds")
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
                       
                       
                       histbin2 <- statar::xtile(pandat$income[pandat$heavy==0], cutpoints = seq(50000,250000,5000)); histbin2 <- c(as.numeric(table(histbin2))[1:41], 0, as.numeric(table(histbin2))[43:45])
                       hist_inc0 <- data.frame(xleft = seq(50000,255000,5000)-2500,xright = seq(50000,255000,5000)+2500, ht = histbin2 )
                       hist_inc0 <- hist_inc0[1:44,]
                       
                       histbin2 <- statar::xtile(pandat$income[pandat$heavy==1], cutpoints = seq(50000,250000,5000)); histbin2 <- c(as.numeric(table(histbin2))[1:41], 0, as.numeric(table(histbin2))[43:45])
                       hist_inc1 <- data.frame(xleft = seq(50000,255000,5000)-2500,xright = seq(50000,255000,5000)+2500, ht = histbin2 )
                       hist_inc1 <- hist_inc1[1:44,]
                       
                       
                       
                       
                       
                       
                       plot_poly <- function(x,ylow, yhigh, col){
                         polygon(x = c(x[1], x, x[length(x)], rev(x)),y = c(ylow[1], yhigh, ylow[length(ylow)],rev(ylow) ), border = NA, col = col)
                       }
                       
                       
                       
                       
                       ##########################################################################################################################################     
                       ### calculations reported in paper ###
                       
                       #how many monitors in final regression?
                       final_monitor_list <- unique(pandat$ID_in)
                       length(final_monitor_list)
                       write_rds(final_monitor_list, file = "data/final_monitor_list.rds") #write out to make sure elsewhere we are using same set of monitors
                       
                       #average infiltration from pooled model
                       pandat <- panel(pandat, panel.id = c("ID_in","time_hours"), duplicate.method = "first") #after a bunch of merges and stuff need to re-define panel structure for feols
                       pooled_model <- feols(pm25_in ~ l(pm25_out,0:6) | ID_in + dow + hod + mos, data = pandat)
                       #estimate + 95% CI:
                       coef <- coefficients(pooled_model)
                       est <- sum(coef[grep(x = names(coef), pattern = "pm25_out")])
                       se <- summary(glht(pooled_model, linfct = paste("pm25_out +","`",paste0("l(pm25_out, ",1:6,")", collapse = "`+`"),"`", "=0",sep = "")))$test$sigma %>% as.numeric()
                       ci <- c(est + qnorm(0.025)*se, est + qnorm(0.975)*se)
                       
                       
                       ##########################################################################################################################################     
                       
                       ### Plot Top Row ###
                       
                       poly_col0 <-add.alpha(wes_palette("Zissou1")[2], .6)
                       poly_col1 <-add.alpha(wes_palette("Zissou1")[3], .75)
                       
                       pdf("figures/fig4-top-row.pdf", width =12.5, height = 6)
                       
                       
                       par(mar = c(4,5,2,2))
                       par(mfrow = c(1,2))
                       
                       
                       ###  panel a  ###
                       
                       plot(0,0,axes=F, xlab = "",ylab = "",pch = 16, cex=2, ylim = c(0, .3), col = NA, xlim =c(5,305))
                       
                       #smoke == 0
                       plot_poly(xp0, yp0_low, yp0_high, poly_col0)
                       lines(xp0, yp0, lwd = 3)
                       
                       #smoke == 1
                       plot_poly(xp1, yp1_low, yp1_high, poly_col1)
                       lines(xp1, yp1, lwd = 3)
                       
                       axis(2, tick = T,las =2, cex.axis = 1.4)
                       axis(1, at = seq(0,300,50), cex.axis=1.4)
                       mtext(side = 1, text = "Outdoor PM2.5 (ug/m3)",line=3,cex=1.5)
                       mtext(side = 2, text = "Infiltration Rate",line=4,cex=1.5)
                       
                       plotHist(hist_ht_p0, poly_col0, .75, 0.015, 0.12, max(hist_ht_p0$ht), add.alpha(poly_col0, .05) )
                       plotHist(hist_ht_p1, poly_col1, .75, 0,0.01, max(hist_ht_p1$ht), add.alpha(poly_col1, .05) )
                       
                       
                       
                       
                       ###  panel b  ###
                       
                       plot(0, 0, axes=F, xlab = "",ylab = "",pch = 16, cex=2, ylim = c(0, .3), col = NA, xlim =c(30000,260000))
                       
                       #smoke == 1
                       plot_poly(xi1, yi1_low, yi1_high, poly_col1)
                       lines(xi1,yi1, lwd = 3)
                       
                       #smoke == 0
                       plot_poly(xi0, yi0_low, yi0_high, poly_col0)
                       lines(xi0,yi0, lwd = 3)
                       
                       axis(2, tick = T,las =2, cex.axis = 1.4)
                       axis(1, tick = T, at = seq(50000,250000,50000),labels = seq(50000,250000,50000)/1000, cex.axis =1.4)
                       mtext(side = 1, text = "median household income ($1000)",line=3,cex=1.5)
                       
                       plotHist(hist_inc0, 'gray80', .75, 0,0.05, max(hist_inc0$ht), add.alpha('white', .05) )
                       
                       
                       
                       dev.off()
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       
