#-------------------------------------------------------------------------------
# Compare Infiltration Estimates
# Written by: Sam Heft-Neal
#-------------------------------------------------------------------------------
# Read in infiltration estimates
v1 <- read_rds(file.path(path_infiltration, "estimates", "PA_monitor_level_infiltration_estimates_sfr_clean_pc.rds")) %>% 
      rename(est_dl1 = est_dl, est_ldv1 = est_ldv, est_nl1 = est_nl, est_al1 = est_al) %>% 
      dplyr::select(id, starts_with("est"), state, lon, lat)
v2 <- read_rds(file.path(path_infiltration, "estimates", "PA_monitor_level_infiltration_estimates_sfr_clean.rds")) %>% 
      rename(est_dl2 = est_dl, est_ldv2 = est_ldv, est_nl2 = est_nl, est_al2 = est_al) %>% 
      dplyr::select(id, starts_with("est"))
v3 <- read_rds(file.path(path_infiltration, "estimates", "PA_monitor_level_infiltration_estimates_sfr_clean_uncorrected.rds")) %>% 
   rename(est_dl3 = est_dl, est_ldv3 = est_ldv, est_nl3 = est_nl, est_al3 = est_al) %>% 
   dplyr::select(id, starts_with("est"))

# Join infiltration estimates
dat <- v1 %>% 
   left_join(v2) %>% 
   left_join(v3)
dat <- dat %>% 
   dplyr::select(id, starts_with("est"))
names(dat)[2:ncol(dat)] <-paste(1:4, rep(c("a","b","c"), each = 4), sep ="")

# Plot correlations
pdf(file = file.path(path_figures, "figureED06.pdf"), width = 8, height = 8)
corrplot::corrplot.mixed(cor(dat[,2:ncol(dat)], use = 'complete.obs'), 
               lower = "number", upper = "square", tl.col ='black')
dev.off()
