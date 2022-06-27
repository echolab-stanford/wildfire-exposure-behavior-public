#-------------------------------------------------------------------------------
# Build EPA Station-Day Panel and Calculate Smoke PM at County Level
# Written by: Anne Driscoll
#-------------------------------------------------------------------------------
if (!file.exists(file.path(path_smokePM, "panel_station_pm_smoke_day.RDS"))) {
  # Load EPA PM2.5 data
  epa = readRDS(file.path(path_epa, "epa_station_level_pm25_data.rds"))
  
  # Create a panel of all date-ID combos
  dates = seq(as.Date("2006-01-01"), as.Date("2020-12-31"), by="days")
  panel = expand.grid(unique(epa$id), dates)
  names(panel) = c("id", "date")
  
  # Get EPA ready to merge to panel 
  epa$date = as.Date(paste0(epa$year, "-", epa$month, "-", epa$day))
  start_date = as.Date("2005-12-31")
  epa = epa %>% 
    filter(date > start_date) %>%
    group_by(id, date) %>%
    summarise(pm25 = mean(pm25, na.rm=T))
  
  # Merge to panel
  epa = merge(epa, panel, by=c("id", "date"), all=T)
  
  # Figure out the grid_id for each EPA ID
  grid = readRDS(file.path(path_boundaries, "grid.RDS"))
  epa_ll = readOGR(file.path(path_epa, "epa_station_locations"), "epa_station_locations")
  epa_ll = spTransform(epa_ll, crs(grid))
  knn_ids = get.knnx(coordinates(grid), coordinates(epa_ll), k=1)$nn.index
  matches = data.frame(id = epa_ll$id, grid_id = grid$ID[knn_ids])
  
  # Add the NA columns to fill in loop
  epa = epa %>%
    mutate(smoke_day=as.numeric(NA), light=as.numeric(NA), medium=as.numeric(NA), 
           dense=as.numeric(NA), total_smoke=as.numeric(NA),
           precipitation=as.numeric(NA), temperature=as.numeric(NA))
  
  # Get smoke days
  smoke = readRDS(file.path(path_smoke, "sparse_smoke_grid.RDS")) %>% 
    mutate(date = as.Date(date, format = "%Y%m%d"),
           smoke_day = 1) %>% 
    rename(grid_id = id, total_smoke = total)
  
  years = 2006:2020
  for (i in 1:length(years)) {
    year = years[i]
    
    precipitation = list.files(file.path(path_era5, "total_precipitation", "grid_precipitation"), 
                               pattern = as.character(year), full.names = T) %>% 
      map_dfr(readRDS) %>% rename(precipitation = total_precipitation)
    temperature = list.files(file.path(path_era5, "2m_temperature", "grid_temperature"), 
                             pattern = as.character(year), full.names = T) %>% 
      map_dfr(readRDS) %>% rename(temperature = `2m_temperature`)
    
    cur = epa[year(epa$date) == year, 1:3]
    cur = cur %>% 
      left_join(matches, by = "id") %>% 
      left_join(smoke, by = c("grid_id", "date")) %>% 
      replace_na(list(smoke_day = 0)) %>% 
      left_join(precipitation, by = c("grid_id" = "id_grid", "date")) %>% 
      left_join(temperature, by = c("grid_id" = "id_grid", "date")) %>% 
      select(id, date, pm25, smoke_day, light, medium, dense, total_smoke, 
             precipitation, temperature)
    
    epa[year(epa$date) == year, ] = cur
    print(year)
  }
  
  saveRDS(epa, file.path(path_smokePM, "panel_station_pm_smoke_day.RDS"))
} else {
  epa = readRDS(file.path(path_smokePM, "panel_station_pm_smoke_day.RDS"))
}

#-------------------------------------------------------------------------------
#### Get the EPA panel connected to county geo ####
if (!file.exists(file.path(path_smokePM, "panel_station_pm_smoke_day_w_county.RDS"))) {
  
  # Get the counties
  counties = readRDS(file.path(path_boundaries, "counties.RDS"))
  epa_ll = spTransform(epa_ll, crs(counties))
  over_c = over(epa_ll, counties)
  matches = data.frame(id = epa_ll$id, 
                       county = over_c$GEOID, 
                       state = over_c$STATEFP) 
  
  # For monitors that didn't match directly, take nearest county (unless far)
  knn_ids = get.knnx(coordinates(counties), 
                     coordinates(epa_ll[is.na(matches$county), ]), k=1)
  knn_ids$nn.index[knn_ids$nn.dist > 1] = NA
  knn_ids = knn_ids$nn.index
  
  w = is.na(matches$county)
  matches[is.na(matches$county), ] = data.frame(id = epa_ll$id[w], 
                                                county = counties$GEOID[knn_ids], 
                                                state = counties$STATEFP[knn_ids])
  
  epa_c = merge(epa, matches, by = "id", all.x=T)
  saveRDS(epa_c, file.path(path_smokePM, "panel_station_pm_smoke_day_w_county.RDS"))
} else {
  # Load EPA panel merged with county
  epa_c = readRDS(file.path(path_smokePM, "panel_station_pm_smoke_day_w_county.RDS"))
}

#-------------------------------------------------------------------------------
#### Aggregate to county level and add county data ####
if (!file.exists(file.path(path_smokePM, "panel_county_pm_smoke_day.RDS"))) {
  
  # Calculate background PM2.5
  # Code originally from Marissa Childs
  pm_medians = epa_c %>% 
    mutate(month = month(date), year = year(date)) %>%
    group_by(county, month, year) %>% 
    summarise(pm25 = list(pm25[which(!is.na(pm25) & smoke_day == 0)])) %>% # Grab the non NA and non-smoke day PM obs
    rowwise() %>%
    mutate(nobs = length(pm25)) %>% # For each station-month year, how many obs meet criteria?
    ungroup %>%
    arrange(county, month, year) %>%
    group_by(county, month) %>%
    # Add leads and lags
    mutate(pm25_lead = lead(pm25, n = 1, default = list(NA)), # Add NAs if there is no previous year which we can ignore while taking medians
           pm25_lag = lag(pm25, n = 1, default = list(NA)), # Add NAs if there is no future year which we can ignore while taking medians
           nobs_lead = lead(nobs, n = 1, default = 0), 
           nobs_lag = lag(nobs, n = 1, default = 0))  %>% 
    ungroup %>%
    rowwise %>%
    mutate(pm25_3yr = list(c(pm25, pm25_lag, pm25_lead)), # Combine pm from current, lag, and lead
           nobs_3yr = nobs + nobs_lead + nobs_lag) %>% # Add up the obs
    rowwise() %>%
    mutate(pm25_med_3yr = median(unlist(pm25_3yr), na.rm = T)) %>%
    select(county, year, month, pm25_med_3yr)
  
  # Aggregate to county-day level and calculate smoke PM2.5
  epa_c %<>% 
    group_by(county, date) %>%
    summarise(n=n(), 
              nas=sum(is.na(pm25)), 
              pm25=mean(pm25, na.rm=T), 
              smoke_day=max(smoke_day),
              precipitation=mean(precipitation),
              temperature=mean(temperature)) %>%
    mutate(month=month(date), 
           year=year(date)) %>%
    merge(pm_medians, all.x=T, by=c("county", "year", "month")) %>%
    mutate(smokePM=pmax(if_else(smoke_day == 1, pm25 - pm25_med_3yr, 0), 0))
  
  # Get Census data to merge
  county_income = get_acs("county", survey="acs5", 
                          variables=c("B19013_001", "B01003_001"), 
                          year=2019) %>% 
    select(GEOID, variable, estimate) %>%
    spread(variable, estimate) %>%
    rename(county=GEOID, median_household_income=B19013_001, 
           population=B01003_001)
  
  # Merge the county level panel
  epa_c = merge(epa_c, county_income, by="county", all.x=T)
  epa_c = epa_c %>% select(county, date, n, nas, pm25, pm25_med_3yr, smokePM, 
                           smoke_day, temperature, precipitation, 
                           median_household_income, population) %>%
    filter(!is.na(epa_c$county))
  saveRDS(epa_c, file.path(path_smokePM, "panel_county_pm_smoke_day.RDS"))
}
