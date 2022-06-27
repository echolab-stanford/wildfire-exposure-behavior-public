#-------------------------------------------------------------------------------
# Prepare PurpleAir and EPA Data
# Written by: Jessica Li
# 
# For PurpleAir data:
# 1. Drop suspicious observations based on repeated consecutive PM2.5 values
# 2. Drop observations missing PM2.5
# 3. Drop monitors with too few observations
# For EPA data:
# 1. Drop observations with PM2.5 < -5
# 2. Set observations with PM2.5 in [-5, 0) to 0
# 3. Drop stations with too few observations
# Drop PurpleAir and EPA date_times that do not overlap
#-------------------------------------------------------------------------------
# Set minimum number of observations a monitor should have
min_obs <- 24 * 30

# Set maximum number of repeated PM values a monitor can have
max_reps <- 24 * 3 - 1

#-------------------------------------------------------------------------------
if (!file.exists(file.path(path_purpleair, "dat_pa_pm25.rds")) | 
    !file.exists(file.path(path_epa, "dat_epa_pm25.rds"))) {
  
  #### PA ####
  # Read in PA data
  dat_pa <- readRDS(file.path(path_purpleair, "outdoor_monitor_data_clean.rds")) %>%
    select(ID_out, time_hours, year, month, day, hour, Lon_out, Lat_out, pm25_out) %>%
    rename(id = ID_out, date_time = time_hours, lon = Lon_out, lat = Lat_out, pm25 = pm25_out)
  
  # Drop suspicious PA monitors
  dat_pa <- dat_pa %>% 
    filter(!((id  == 10608) & (year == 2020) & (month %in% 8:10)),
           !((id == 5236) & (year == 2020)))
  
  # Drop missing PM2.5 values in PA data
  dat_pa <- dat_pa %>% drop_na(pm25)
  
  # Drop PA stations with too few observations
  dat_pa <- dat_pa %>%
    group_by(id) %>%
    filter(n() >= min_obs) %>%
    ungroup()
  
  #-------------------------------------------------------------------------------
  #### EPA ####
  # Figure out which years PA and EPA data might overlap
  years_pa <- unique(dat_pa$year)
  years_epa <- list.files(file.path(path_epa, "hourly_epa")) %>%
    substr(14, 17) %>%
    as.numeric()
  years <- max(min(years_pa), min(years_epa)):min(max(years_pa), max(years_epa))
  
  # Read in EPA data
  dat_epa <- file.path(path_epa, "hourly_epa", paste0("hourly_88101_", years, ".csv")) %>%
    map_dfr(read.csv) %>%
    select(State.Code, County.Code, Site.Num, Date.GMT, Time.GMT, 
           Longitude, Latitude, Sample.Measurement) %>%
    rename(state = State.Code, county = County.Code, site = Site.Num,
           date = Date.GMT, time = Time.GMT, lon = Longitude, lat = Latitude,
           pm25 = Sample.Measurement) %>%
    mutate(date_time = paste(date, time) %>% 
             as.POSIXct(tz = "GMT", format = "%Y-%m-%d %H:%M") %>%
             with_tz("UTC"),
           scs_id = paste(state, county, site, sep = "-")) %>%
    select(-date, -time)
  
  # Drop EPA PM < -5 and set EPA PM in [-5, 0) to 0
  dat_epa <- dat_epa %>% 
    filter(pm25 >= -5) %>%
    mutate(pm25 = ifelse(pm25 < 0, 0, pm25))
  
  # Drop EPA stations with too few observations
  dat_epa <- dat_epa %>%
    group_by(scs_id) %>%
    filter(n() >= min_obs) %>%
    ungroup()
  
  #-------------------------------------------------------------------------------
  # Discard date-times where PA and EPA data don't overlap
  dt_pa <- unique(dat_pa$date_time)
  dt_epa <- unique(dat_epa$date_time)
  dat_pa <- dat_pa %>% filter(date_time %in% dt_epa)
  dat_epa <- dat_epa %>% filter(date_time %in% dt_pa)
  
  # Save data
  saveRDS(dat_pa, file.path(path_purpleair, "dat_pa_pm25.rds"))
  saveRDS(dat_epa, file.path(path_epa, "dat_epa_pm25.rds"))
}

#-------------------------------------------------------------------------------
# Match PurpleAir and EPA PM2.5
# Written by: Jessica Li
# 
# For each PurpleAir monitor, match the nearest EPA station within a fixed
# distance and at the same date-time.
#-------------------------------------------------------------------------------
if (!file.exists(file.path(path_epa, "dat_pa_epa_pm25_nn.rds"))) {
  
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
}

#-------------------------------------------------------------------------------
# Compare PurpleAir and EPA PM2.5
# Written by: Sam Heft-Neal
# Requires large computer memory.
#-------------------------------------------------------------------------------
# Set maximum distance (m) for matching
within_m <- 1000

# Set minimum number of observations a monitor should have
min_obs <- 24 * 30

# Comes from matching PurpleAir and EPA PM2.5 data
dat_matched <- readRDS(file.path(path_epa, "dat_pa_epa_pm25_nn.rds"))

# Now we want to use update (ie corrected) outdoor PA PM in this comparison so 
# take this data and join with corrected hourly PA
pa_corrected <- readRDS(file.path(path_purpleair, "outdoor_monitor_data_clean_part1.rds"))

pa_corrected <- pa_corrected[pa_corrected$ID_out %in% dat_matched$id_pa,]
pa_corrected<-pa_corrected %>% rename(id_pa = ID_out, date_time = time_hours)
pa_corrected<-pa_corrected %>% rename(pm25_corrected_out = pm25_out)
pa_corrected <- pa_corrected[,c("id_pa","date_time","pm25_uncorrected_out","pm25_corrected_out")]

dat_matched2 <- left_join(dat_matched, pa_corrected)


pa_corrected1 <- readRDS(file.path(path_purpleair, "outdoor_monitor_data_clean.rds"))
pa_corrected1 <- pa_corrected1[pa_corrected1$ID_out %in% dat_matched$id_pa,]
pa_corrected1<-pa_corrected1 %>% rename(id_pa = ID_out, date_time = time_hours)
pa_corrected1 <- pa_corrected1[,c("id_pa","date_time","pm25_out_pc")]

dat_matched2 <- left_join(dat_matched2, pa_corrected1)

rm(pa_corrected, pa_corrected1)
gc()

# Discard monitors with too few observations
dat_matched <- dat_matched2 %>% 
  filter(dist_m <= within_m) %>%
  group_by(id_pa) %>%
  filter(n() >= min_obs) %>%
  ungroup() %>%
  group_by(scs_id_epa) %>%
  filter(n() >= min_obs) %>%
  ungroup()

rm(dat_matched2)

# Define month of sample
dat_matched <- dat_matched %>%  mutate(month_sample = format(date_time, "%Y-%m"))

# Fit regressions
md1 <- feols(pm25_epa ~ pm25_uncorrected_out | 
               (id_pa) + (scs_id_epa) + 
               (month_sample) + (hour), 
             data = dat_matched)

md2 <- feols(pm25_epa ~ pm25_corrected_out | 
               (id_pa) + (scs_id_epa) + 
               (month_sample) + (hour), 
             data = dat_matched)

md3 <- feols(pm25_epa ~ pm25_out_pc | 
               (id_pa) + (scs_id_epa) + 
               (month_sample) + (hour), 
             data = dat_matched)

# Regression results
summary(md1)
summary(md2)
summary(md3)

r2(md1)
r2(md2)
r2(md3)

# Save
etable(md1, md2, md3, 
       digits = "r3",
       digits.stats = 2,
       fitstat = c("r2", "n"),
       dict = c(pm25_uncorrected_out = "Purple Air PM$_{2.5}$",
                pm25_corrected_out = "Purple Air PM$_{2.5}$",
                pm25_out_pc = "Purple Air PM$_{2.5}$",
                scs_id_epa = "EPA monitor",
                id_pa = "Purple Air monitor",
                hour = "hour-of-day",
                month_sample = "month-of-sample"),
       file = file.path(path_tables, "tableS12.tex"),
       style.tex = style.tex(depvar.title = "",
                             model.title = "\\textbf{Purple Air PM2.5 Construction:}",
                             line.top = "",
                             line.bottom = "\\bottomrule",
                             var.title = "",
                             fixef.title = "",
                             fixef.prefix = "FE: ",
                             fixef.where = "stats",
                             stats.title = "",
                             tablefoot = F),
       depvar = F)
