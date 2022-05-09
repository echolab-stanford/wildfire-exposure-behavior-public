#-------------------------------------------------------------------------------
# Standardize Sample
# Written by: Jessica Li
# Last edited by: Jessica Li
#-------------------------------------------------------------------------------
# Read in PurpleAir data frames
pm_paths <- c("PA/", "EPA/", "PC/")
post_paths <- c("not_posterior/", "posterior/")
la <- length(pm_paths)
lo <- length(post_paths)
dat_pas <- vector("list", la*lo)
foreach(i = 1:la) %:% foreach(j = 1:lo) %do% {
  # N = 1511 (PA, not posterior)
  # N = 1511 (PA, posterior)
  # N = 1505 (EPA, not posterior)
  # N = 1505 (EPA, posterior)
  # N = 1501 (PC, not posterior)
  # N = 1501 (PC, posterior)
  df <- readRDS(file.path(path_infiltration, pm_paths[i], post_paths[j], "dat_pa_inf.rds"))
  print(paste(pm_paths[i], post_paths[j], nrow(df)))
  dat_pas[[i + 3*(j - 1)]] <- df
  saveRDS(df, file.path(path_infiltration, pm_paths[i], post_paths[j], "dat_pa_inf_nonstandard.rds"))
}

# Get set of IDs common to all sets
id_keep <- lapply(dat_pas, select, "id") %>% reduce(inner_join) %>% pull(id)

# Keep only observations with common ID and save
for (i in 1:length(dat_pas)) dat_pas[[i]] <- dat_pas[[i]] %>% filter(id %in% id_keep)
foreach(i = 1:la) %:% foreach(j = 1:lo) %do% {
  # N = 1484 (PA/EPA/PC, not posterior/posterior)
  saveRDS(dat_pas[[i + 3*(j - 1)]], file.path(path_infiltration, pm_paths[i], post_paths[j], "dat_pa_inf.rds"))
}
