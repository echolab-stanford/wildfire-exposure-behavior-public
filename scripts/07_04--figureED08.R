##################################
## read in and clean up briefly ##
##################################


#read in data and rename pm25_out
data <- read_rds("~/Documents/GitHub/wildfire_home_leakage/data/analysis_data_clean_all.rds") %>% mutate(pm25_out = pm25_out_mean)

#add woy and doy so we have option to include them as FE 
data <- data %>% mutate(
  woy = lubridate::week(time_hours),
  dow = lubridate::wday(time_hours),
  mos = paste(month, year,sep ="-")
) %>% rename(hod = hour)

count_obs <- data %>% group_by(ID_in) %>% summarise(n_obs = sum(!is.na(pm25_in) & !is.na(pm25_out)), var_in = sd(pm25_in, na.rm = T), var_out = sd(pm25_out, na.rm = T))
drop_ids <- count_obs %>% dplyr::filter(n_obs<200 | var_in < 1 | var_out <1 ) %>% dplyr::select(ID_in) %>% unlist()
data <- data %>% dplyr::filter(ID_in %in% drop_ids == F)

pandat <- panel(data, panel.id = c("ID_in","time_hours"), duplicate.method = "first") 
rm(data) 




io <- pandat %>% dplyr::filter(pm25_out > 0 & !is.na(pm25_out) & pm25_out >= pm25_in) %>% mutate(io_ratio = pm25_in/pm25_out) %>% group_by(ID_in) %>% summarise(io_ratio = mean(io_ratio, na.rm = T))
dido <- read_rds("~/Documents/GitHub/wildfire_home_leakage/data/purpleAir/PA_monitor_level_infiltration_estimates_sfr_clean_pc.rds") %>% filter(est_dl > 0 & est_dl <1 & !is.na(est_dl))
io <- io %>% rename(id = ID_in)

dat <- left_join(io, dido) %>% drop_na(io_ratio, est_dl)




m <- summary(lm(est_dl ~ io_ratio, data= dat))



pdf(file ="~/Documents/GitHub/purple-air-infiltration/figures/figSX_io_vs_dido.pdf",width = 6, height = 6)
plot(dat$io_ratio, dat$est_dl, xlim = c(0,1), ylim= c(0,1), pch = 21, bg = add.alpha('gray', 0.5), col ='gray30',axes = F, xlab = "",ylab = "")
abline(a = coef(m)[1], b = coef(m)[2], col = 'red')
abline(v = mean(dat$io_ratio), col = 'blue',lty=2)
abline(h = mean(dat$est_dl, na.rm = T), col = 'blue',lty=2)
axis(2, tick = T, las =2,cex.axis=1.5)
axis(1, tick = T,cex.axis=1.5)
mtext(side = 1, text = "I/O Ratio",cex=2,line=3)
mtext(side = 2, text = "dI/dO",cex=2,line=3)
text(x = 0.9, y = 0.9, labels = paste("slope =",round(coef(m)[2],2)),cex=2)
text(x = 0.9, y = 0.85, labels = paste("R2 =",round(m$adj.r.squared,2)),cex=2)
dev.off()



######################################################################################################################################################################################


daily <- pandat %>% group_by(ID_in, year, month, day) %>% summarise(daily_pm = mean(pm25_out, na.rm = T))
daily$pm_bin <- statar::xtile(daily$daily_pm, cutpoints = c(5,seq(10,30,10),50,100))
pandat <- left_join(pandat, daily)


io_list <- list()
for(i in 6:7){
  
  subdat <- pandat[pandat$pm_bin==i,]
  io_list[[i]] <- subdat %>% dplyr::filter(pm25_out > 0 & !is.na(pm25_out) & pm25_out >= pm25_in) %>% mutate(io_ratio = pm25_in/pm25_out) %>% group_by(ID_in) %>% summarise(io_ratio = mean(io_ratio, na.rm = T))
}

dido_list <- list()



for(i in 6:7){
  
  subdat <- pandat[pandat$pm_bin==i,]
  mlist <- sort(unique(subdat$ID_in))
  mlist <- mlist[mlist%in%dido$id]
  dido_list[[i]] <- data.frame(ID_in = mlist, dido = NA)
  
  
  for(j in 1:length(mlist)){
  subdat2 <- subdat %>% filter(ID_in == mlist[j])
   try( dido_list[[i]]$dido[j]<- as.numeric(coef(feols(pm25_in ~  l(pm25_out, 0)  | hod + woy + dow + mos, data = subdat2))[1]))
    }
  print(i)
}


combo <- list()

for(i in 1:7){combo[[i]] <- left_join(io_list[[i]], dido_list[[i]])}

for(i in 1:7){combo[[i]]$bin = i}
combo <- data.frame(data.table::rbindlist(combo))
combo <- combo %>% drop_na(io_ratio, dido) %>% filter(io_ratio > 0 & io_ratio < 1 & dido > 0 & dido < 1)

png(filename = file.path(path_github, "figures/raw/figureED08.png"), width = 1400, height = 750)
par(mfrow = c(2,4))
for(i in 1:7){
  plot(combo$io_ratio[combo$bin==i], combo$dido[combo$bin==i], xlim= c(0,1), ylim= c(0,1), col = 'gray', pch = 16, xlab = "I/O ratio", ylab = "dI/dO")
  m <- summary(lm(dido ~ io_ratio, data= combo[combo$bin==i,]))
  abline(a = coef(m)[1], b = coef(m)[2], col = 'red')
  text(x = 0.8, y = 0.98,label = paste("slope=",round(coef(m)[2],2),sep=""),cex = 1.5)
  text(x = 0.8, y = 0.9,label = paste("R2=",round(m$adj.r.squared,2),sep=""),cex = 1.5)
  text(x = 0.2, y = 0.978,label = paste("R2=",round(m$adj.r.squared,2),sep=""),cex = 1.5)
  mtext(side = 3, text = paste("Ave daily PM2.5 = ",c("<5","5-10","10-20","20-30","30-50","50-100",">100")[i],"ug/m3",sep=''),line=1, adj = 0, cex = 1.5)
}
dev.off()


###############



# sfr <- read_csv("data/purpleAir/locationTypeCategorizing/locationTypeCategorized.csv") %>% rename(building_type = type) %>% dplyr::filter(building_type=="sfr") 
# io <- io %>% dplyr::filter(ID_in %in% sfr$id)
# 
# io <- io %>% filter(!is.infinite(io$io_ratio) & io_ratio < 1)
# 
# di <- read_rds("~/BurkeLab Dropbox/Projects/purple-air-infiltration/predict_PA_inf_CL_ACS/PC/posterior/dat_pa_inf_cl_acs_chars_avg.rds")
# 
# di <- di %>% filter(cutoff == "nn")
# 
# io <- io %>% rename(id = ID_in)
# io <- left_join(io, di)
# io <- io%>% filter(!is.na(lon))
# 
# 
# 
# summary(lm(io_ratio ~ yrbuilt, data= io))
# 
# 
# summary(lm(io_ratio ~ factor(yrbuilt>=2000), data= io))
# summary(lm(io_ratio ~ factor(yrbuilt>=2008), data= io))
# 
# io$inf_dl <- io$est_dl
# 
# 
# summary(lm(inf_dl ~ factor(yrbuilt>=2000), data= io))
# summary(lm(inf_dl ~ factor(yrbuilt>=2008), data= io))
# 
# 
# 
# 
# 
