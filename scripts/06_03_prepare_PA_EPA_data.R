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
