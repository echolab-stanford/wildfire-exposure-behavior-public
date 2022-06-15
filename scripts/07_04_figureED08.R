#-------------------------------------------------------------------------------
# Figure ED8
# Written by: Sam Heft-Neal
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

count_obs <- data %>% 
  group_by(ID_in) %>% 
  summarise(n_obs = sum(!is.na(pm25_in) & !is.na(pm25_out)), 
            var_in = sd(pm25_in, na.rm = T), 
            var_out = sd(pm25_out, na.rm = T))
drop_ids <- count_obs %>% 
  dplyr::filter(n_obs<200 | var_in < 1 | var_out <1 ) %>% 
  dplyr::select(ID_in) %>% 
  unlist()
data <- data %>% dplyr::filter(ID_in %in% drop_ids == F)

pandat <- panel(data, panel.id = c("ID_in","time_hours"), duplicate.method = "first") 
rm(data) 

io <- pandat %>% 
  dplyr::filter(pm25_out > 0 & !is.na(pm25_out) & pm25_out >= pm25_in) %>% 
  mutate(io_ratio = pm25_in/pm25_out) %>% 
  group_by(ID_in) %>% 
  summarise(io_ratio = mean(io_ratio, na.rm = T))
dido <- read_rds(file.path(path_infiltration, "estimates", "PA_monitor_level_infiltration_estimates_sfr_clean_pc.rds")) %>% 
  filter(est_dl > 0 & est_dl <1 & !is.na(est_dl))
io <- io %>% rename(id = ID_in)

dat <- left_join(io, dido) %>% drop_na(io_ratio, est_dl)

m <- summary(lm(est_dl ~ io_ratio, data= dat))

#-------------------------------------------------------------------------------
# Plot
pdf(file = file.path(path_figures, "figureED08.pdf"),width = 6, height = 6)
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
