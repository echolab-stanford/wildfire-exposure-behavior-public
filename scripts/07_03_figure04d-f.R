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
