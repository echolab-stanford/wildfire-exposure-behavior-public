source("~/Documents/GitHub/wildfire_home_leakage/work/02_purpleAir/00_loadFunctions.R")
source("~/Documents/GitHub/wildfire_home_leakage/work/02_purpleAir/00_loadPackages.R")
library(splines)

  ##################################
  ## read in and clean up briefly ##
  ##################################
data <- read_rds("~/Documents/GitHub/wildfire_home_leakage/data/analysis_data_clean_all.rds") %>% mutate(pm25_out = pm25_out_mean)

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

income <- read_rds("~/Documents/GitHub/purple-air-infiltration/data/purpleAir_indoor_acs_medianIncome.rds")



#bring in income data
data <- left_join(data, income) #%>% dplyr::filter(!is.na(income_median))

pandat <- panel(data, panel.id = c("ID_in","time_hours"), duplicate.method = "first") 
rm(data) 

#only want to include single family residences:
sfr <- read_rds("~/Documents/GitHub/wildfire_home_leakage/data/clean/purpleair_infiltration_estimates_by_model.rds") %>% rename(ID_in = id) %>% dplyr::select(ID_in, building_type)
pandat <- left_join(pandat, sfr) %>% dplyr::filter(building_type == "sfr")


pandat$pm25_out[pandat$pm25_out < -5 ]<- NA
pandat$pm25_out[pandat$pm25_out > 1000 ]<- NA
pandat$pm25_in[pandat$pm25_in < -5 ]<- NA
pandat$pm25_in[pandat$pm25_in > 1000 ]<- NA
pandat$pm25_out_pc_mean[pandat$pm25_out_pc_mean < -5 ]<- NA
pandat$pm25_out_pc_mean[pandat$pm25_out_pc_mean > 1000 ]<- NA
pandat$pm25_in_pc[pandat$pm25_in_pc < -5 ]<- NA
pandat$pm25_in_pc[pandat$pm25_in_pc > 1000 ]<- NA



smoke <- read_rds("~/BurkeLab Dropbox/Projects/pollution_infiltration/data/intermediate_files/smoke_by_PAmonitor_density.rds") %>% rename(day = dom)
pandat <- left_join(pandat, smoke)
pandat$heavy = as.numeric(pandat$density==27)


###  bring in smoke ###    
smoke_day <- read_rds("~/BurkeLab Dropbox/Projects/pollution_infiltration/data/intermediate_files/smoke_by_PAmonitor.rds") %>% rename(day = dom)

pandat <- left_join(pandat, smoke_day)

prism <- read_rds("~/Documents/GitHub/wildfire_home_leakage/data/purpleAir/hourly_prism.rds")
grid <- raster("~/BurkeLab Dropbox/Projects/pollution_infiltration/data/prism_grid/PRISM_tmin_early_4kmD2_20201001_bil/PRISM_tmin_early_4kmD2_20201001_bil.bil") %>% raster()
latlon <- pandat %>% group_by(ID_in) %>% summarise(Lon_in2 = mean(Lon_in, na.rm = T), Lat_in2 = mean(Lat_in, na.rm = T))
pandat <- left_join(pandat, latlon)

pandat$cell <- cellFromXY(grid, pandat[,c("Lon_in2","Lat_in2")])

pandat <- left_join(pandat, prism)

pandat <- pandat %>% rename(temperature_out = Temp, precip_out = ppt)

#####################################################################################################################      
#####################################################################################################################
####################################
      
### [2] write function for running reg with correct # of lags and map it over vector of station ids
      
    #make sure we have reight measures
   # pandat$pm25_in <- pandat$pm25_in_pc
   # pandat$pm25_out <- pandat$pm25_out_pc_mean
   # pandat$pm25_in <- pandat$pm25_corrected_in
   # pandat$pm25_out <- pandat$pm25_out_mean


    pandat$raining <- as.numeric(pandat$precip_out>0 & !is.na(pandat$precip_out))
    pandat$raining[is.na(pandat$precip_out)]<-NA
    pandat$night <- as.numeric(pandat$hod %in% c(22:24,0:6) )
    
     
    pooled_model_tmp_in <- feols(pm25_in ~ l(pm25_out,0)*temperature_in | ID_in + dow + hod + mos, data = pandat)
    pooled_model_tmp_out <- feols(pm25_in ~ l(pm25_out,0)*temperature_out | ID_in + dow + hod + mos, data = pandat)
    pooled_model_rain <- feols(pm25_in ~ l(pm25_out,0) +  l(pm25_out,0):raining | ID_in + dow + hod + mos, data = pandat)
    pooled_model_night <- feols(pm25_in ~ l(pm25_out,0) + l(pm25_out,0):night | ID_in + dow + hod + mos, data = pandat)
    
    
    pandat_full <- pandat; rm(pandat)

    run_reg_nolag <- function(id){
      
      dat <- dplyr::filter(pandat, ID_in == id) 
      dat <- panel(dat, panel.id = c("ID_in","time_hours"), duplicate.method = "first")
      fmla <- as.formula(paste("pm25_in ~  l(pm25_out, 0)  | hod + woy + dow + mos",sep=""))
      mod <- feols(fmla , data = dat)
      est <-  coef(mod)["pm25_out"] 
      se <- se(mod)["pm25_out"]
      n <- summary(mod)$nobs
      return(data.frame(id = id, est_nl = est,  se_nl = se, N_nl = n))          
    }
    
    
    
    stations <- sort(unique(pandat$ID_in)) #list of stations
    res <- Map(function(x) try(run_reg_nolag(x)), stations)
    index <- lapply(res, function(x){nrow(x)==0 | is.null(nrow(x))}); index <- lapply(index, function(x){length(x)>0}) %>% unlist(); table(index)
    res_nl <- data.frame(data.table::rbindlist(res[which(index==T)]))    
    
    res_nl <- res_nl %>% rename(ID_in = id)
    res_nl <- left_join(res_nl, income)
    di <- read_rds("~/BurkeLab Dropbox/Projects/purple-air-infiltration/predict_PA_inf_CL_ACS/PC/posterior/dat_pa_inf_cl_acs_chars_avg.rds") %>% rename(ID_in = id)
    di <- di %>% filter(cutoff == "nn")
    di <- di %>% dplyr::select(ID_in, est_dl, se_dl, est_nl_full = est_nl, se_nl_full = se_nl, starts_with("race"),poverty_below, income_median, cdd, hdd, pop_density, height, bedrooms, baths, area, yrbuilt, stories, housing_index)
    res_nl <- left_join(res_nl, di)
    res_night <- res_nl
    
    
    pandat <-  pandat_full %>% dplyr::filter(raining == 1)
    stations <- sort(unique(pandat$ID_in)) #list of stations
    res <- Map(function(x) try(run_reg_nolag(x)), stations)
    index <- lapply(res, function(x){nrow(x)==0 | is.null(nrow(x))}); index <- lapply(index, function(x){length(x)>0}) %>% unlist(); table(index)
    res_nl <- data.frame(data.table::rbindlist(res[which(index==T)]))    
    
    res_nl <- res_nl %>% rename(ID_in = id)
    res_nl <- left_join(res_nl, income)
    di <- read_rds("~/BurkeLab Dropbox/Projects/purple-air-infiltration/predict_PA_inf_CL_ACS/PC/posterior/dat_pa_inf_cl_acs_chars_avg.rds") %>% rename(ID_in = id)
    di <- di %>% filter(cutoff == "nn")
    di <- di %>% dplyr::select(ID_in, est_dl, se_dl, est_nl_full = est_nl, se_nl_full = se_nl, starts_with("race"),poverty_below, income_median, cdd, hdd, pop_density, height, bedrooms, baths, area, yrbuilt, stories, housing_index)
    res_nl <- left_join(res_nl, di)
    res_rain <- res_nl
    
    pandat <-  pandat_full %>% dplyr::filter(temperature_out < 10 & pm25_out < 30)
    stations <- sort(unique(pandat$ID_in)) #list of stations
    res <- Map(function(x) try(run_reg_nolag(x)), stations)
    index <- lapply(res, function(x){nrow(x)==0 | is.null(nrow(x))}); index <- lapply(index, function(x){length(x)>0}) %>% unlist(); table(index)
    res_nl <- data.frame(data.table::rbindlist(res[which(index==T)]))    
    res_nl <- res_nl %>% rename(ID_in = id)
    res_nl <- left_join(res_nl, income)
    di <- read_rds("~/BurkeLab Dropbox/Projects/purple-air-infiltration/predict_PA_inf_CL_ACS/PC/posterior/dat_pa_inf_cl_acs_chars_avg.rds") %>% rename(ID_in = id)
    di <- di %>% filter(cutoff == "nn")
    di <- di %>% dplyr::select(ID_in, est_dl, se_dl, est_nl_full = est_nl, se_nl_full = se_nl, starts_with("race"),poverty_below, income_median, cdd, hdd, pop_density, height, bedrooms, baths, area, yrbuilt, stories, housing_index)
    res_nl <- left_join(res_nl, di)
    res_lowtime <- res_nl
    
    
    
    summary(lm(est_nl_full ~ medincome, data = res_rain))
    summary(lm(est_nl ~ medincome, data = res_rain))
    summary(lm(est_nl ~ medincome, data = res_night))
    summary(lm(est_nl ~ medincome, data = res_lowtime))
    
    mean(res_lowtime$est_nl, na.rm = T)
    mean(res_rain$est_nl, na.rm = T)
    mean(res_night$est_nl, na.rm = T)
    mean(res_night$est_nl_full, na.rm = T)
    
    
    
    save(res_night,res_rain,res_lowtime, file = "~/Desktop/infiltration_by_circ.RData")
  
    res_full <- res_rain %>% dplyr::select(-est_nl, -se_nl) %>% rename(est_nl = est_nl_full, se_nl = se_nl_full)
    
    res <- list(res_full,res_rain,res_night,res_lowtime) %>% lapply( function(x){quantile(x$est_nl, c(0.25, .5, .75), na.rm = T)}) 
    
    
    
    
    
    
    rdat <- data.frame(type = c("full","rain","night","low-time"),  low = NA,est = NA,high = NA)
    for(i in 1:4){rdat[i,c("low","est","high")]<-as.numeric(res[[i]])}
    # lets just double check full results used elsewhere in paper similar to full sample results re-estimated here
    inf <- readRDS("~/Documents/GitHub/wildfire_home_leakage/data/purpleAir/PA_monitor_level_infiltration_estimates_sfr_clean_pc.rds")
    quantile(inf$est_nl, c(0.25, .5, .75), na.rm = T)
    rdat[rdat$type=="full",c("low","est","high")]

    
        
    idat <-  data.frame(type = c("full","rain","night","low-time"),  low = NA,est = NA,high = NA)
    
    mod1 <- lm(est_nl ~ medincome, data = res_full)
    mod2 <- lm(est_nl ~ medincome, data = res_rain)
    mod3 <- lm(est_nl ~ medincome, data = res_night)
    mod4 <- lm(est_nl ~ medincome, data = res_lowtime)
    mod <- list(mod1, mod2, mod3, mod4)
    
    
    dat <- list(res_full, res_rain, res_night, res_lowtime)
    

    res <- list()
    for(i in 1:4){

    res[[i]] <- summary(lm(est_nl ~ medincome, data= dat[[i]]) )

    }
    
    rdat2 <- data.frame(type = rdat$type, low = NA, est = NA, high = NA)
    
    for(i in 1:4){rdat2[i,c("low","est","high")] <- c(
      coef(res[[i]])[2] + qnorm(0.05)*se(res[[i]])[2],
      coef(res[[i]])[2],
      coef(res[[i]])[2] + qnorm(0.95)*se(res[[i]])[2]
      
      )
    }
    
    rdat2[,2:4]<-rdat2[,2:4]*50000
    rdat2$low_per <- rdat2$low/rdat$est
    rdat2$est_per <- rdat2$est/rdat$est
    rdat2$high_per <- rdat2$high/rdat$est
    
    
    
    
    mod1 <- lm(est_nl ~ as.factor(yrbuilt>2008), data = res_full)
    mod2 <- lm(est_nl ~ as.factor(yrbuilt>2008), data = res_rain)
    mod3 <- lm(est_nl ~ as.factor(yrbuilt>2008), data = res_night)
    mod4 <- lm(est_nl ~ as.factor(yrbuilt>2008), data = res_lowtime)
    mod <- list(mod1, mod2, mod3, mod4)
    
    
    rdat3 <- data.frame(type = rdat$type, low = NA, est = NA, high = NA)
    
    for(i in 1:4){rdat3[i,c("low","est","high")] <- c(
      coef(mod[[i]])[2] + qnorm(0.05)*se(mod[[i]])[2],
      coef(mod[[i]])[2],
      coef(mod[[i]])[2] + qnorm(0.95)*se(mod[[i]])[2]
      
    )
    }
    
    rdat3$low_per <- rdat3$low/rdat$est
    rdat3$est_per <- rdat3$est/rdat$est
    rdat3$high_per <- rdat3$high/rdat$est
    
    
    
    pdf(file = "figures/figSX-low-behavior-income-housing.pdf", width = 12, height = 4)
    par(mfrow = c(1,3))
    plot(1:4, rdat$est,xlim = c(0.5, 4.5),ylim = c(0, .3),axes = F, xlab = "",ylab = "",col = NA)
    segments(x0 = 1:4, y0 = rdat$low, y1 = rdat$high, col = 'gray75', lwd = 1.25)
    points(1:4, rdat$est, pch = 21, cex =3, col ='black',bg ='white',lwd=1)
    #abline(h=0, col = 'red',lty=2)
    axis(2, las=2,tick = T, at = seq(0, .3, .1),cex.axis=1.25)
    mtext(side = 3, text = "Inter-quartile range of infiltration estimates",cex=1,adj =0)
    text(x = 1:4, y = rdat$low - 0.02, labels = c("Full model","Raining","Night","Low-behavior"),cex=1.1)
    mtext(side = 2, text = "Infiltration", line=3,cex=1)
    
    plot(1:4, rdat2$est_per,xlim = c(0.5, 4.5), ylim = c(-1, .5),axes = F, xlab = "",ylab = "",col = NA)
    abline(h=0, col = 'red',lty=2)
    segments(x0 = 1:4, y0 = rdat2$low_per*2, y1 = rdat2$high_per*2, col = 'gray25', lwd = 1.25)
    points(1:4, rdat2$est_per*2, pch = 21, cex =3, col ='black',bg ='gray75',lwd=1)
   # axis(2, las=2,tick = T, at = seq(-.3, .1, .1), labels =  seq(-30,10,10))
    mtext(side = 3, text = "% change in infiltration per + $100K income",cex=1,adj =0)
    text(x = 1:4, y = rdat2$low_per*2 - 0.05, labels = c("Full model","Raining","Night","Low-behavior"),cex=1.1)
    mtext(side = 2, text = "% change", line=3,cex=1)
    axis(2, las=2,tick = T, at = seq(-1, .5, .25), labels =  seq(-100,50,25),cex.axis=1.25)
    
    
    
    plot(1:4, rdat3$est_per,xlim = c(0.5, 4.5), ylim = c(-1, 0.5),axes = F, xlab = "",ylab = "",col = NA)
    abline(h=0, col = 'red',lty=2)
    axis(2, las=2,tick = T, at = seq(-1, .5, .25), labels =  seq(-100,50,25),cex.axis=1.25)
    segments(x0 = 1:4, y0 = rdat3$low_per, y1 = rdat3$high_per, col = 'gray25', lwd = 1.25)
    points(1:4, rdat3$est_per, pch = 21, cex =3, col ='black',bg ='gray75',lwd=1)
    mtext(side = 3, text = "% change in infiltration if home built after 2008",cex=1,adj =0)
    text(x = 1:4, y = rdat3$low_per - .05, labels = c("Full model","Raining","Night","Low-behavior"),cex=1.1)
    mtext(side = 2, text = "% change", line=3,cex=1)
    
    
    dev.off()
    
    
    
    
    
