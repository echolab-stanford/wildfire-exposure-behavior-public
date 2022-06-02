#-------------------------------------------------------------------------------
# Split Data into Train/Test
# Written by: Jessica Li
#-------------------------------------------------------------------------------
# Read in matched data
dat_matched <- readRDS(file.path(path_infiltration, pm_path, post_path, "dat_pa_inf_cl_acs_chars_avg.rds"))

# Split train/test
set.seed(5377)
dat_test <- dat_matched %>% 
  group_by(cutoff, physio_province) %>% 
  slice_sample(prop = 0.25) %>% 
  ungroup()
dat_train <- dat_matched %>% setdiff(dat_test)

# Save data
saveRDS(dat_train, file.path(path_infiltration, pm_path, post_path, "dat_train.rds"))
saveRDS(dat_test, file.path(path_infiltration, pm_path, post_path, "dat_test.rds"))
