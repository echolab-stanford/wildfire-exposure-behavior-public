#-------------------------------------------------------------------------------
# Match PurpleAir and EPA PM2.5
# Written by: Jessica Li
# 
# For each PurpleAir monitor, match the nearest EPA station within a fixed
# distance and at the same date-time.
#-------------------------------------------------------------------------------
# Read in PA and EPA data
dat_pa <- readRDS(file.path(path_purpleair, "dat_pa_pm25.rds"))
dat_epa <- readRDS(file.path(path_epa, "dat_epa_pm25.rds"))

# Get date-times
dts <- unique(dat_pa$date_time)

# Match nearest neighbors - this takes ~1 hour
registerDoParallel(num_cores)
start_time <- get_start_time()
dat_matched <- foreach(dts_i = split_chunks(dts, num_cores), .combine = bind_rows) %dopar% {
  dat_pa_i <- dat_pa %>% filter(date_time %in% dts_i)
  dat_epa_i <- dat_epa %>% filter(date_time %in% dts_i)
  
  foreach(dt = dts_i, .combine = bind_rows) %do% {
    # Subset PA and EPA data for this date-time
    dat_pa_dt <- dat_pa_i %>% filter(date_time == dt)
    dat_epa_dt <- dat_epa_i %>% filter(date_time == dt)
    
    # Find nearest EPA location to PA monitor
    nn <- get.knnx(dat_epa_dt[c("lon", "lat")], 
                   dat_pa_dt[c("lon", "lat")], 
                   k = 1)
    
    # Put matched PA and EPA data together
    dat_epa_dt <- dat_epa_dt[nn$nn.index,] %>%
      rename(scs_id_epa = scs_id, lon_epa = lon, lat_epa = lat, pm25_epa = pm25) %>%
      mutate(dist_deg = nn$nn.dist) %>%
      select(-date_time)
    match_i <- bind_cols(dat_pa_dt, dat_epa_dt) %>%
      rename(id_pa = id, lon_pa = lon, lat_pa = lat, pm25_pa = pm25) %>%
      mutate(dist_m = pointDistance(data.frame(lon_pa, lat_pa),
                                    data.frame(lon_epa, lat_epa),
                                    lonlat = TRUE))
  }
}
print_time(start_time)
stopImplicitCluster()
saveRDS(dat_matched, file.path(path_epa, "dat_pa_epa_pm25_nn.rds"))
