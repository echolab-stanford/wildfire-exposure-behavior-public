#-------------------------------------------------------------------------------
# Correlate Smoke PM Time Series Across Pairs of Stations
# Written by: Jessica Li
# 
# Running this script may require a large amount of computing resources. The use
# of high-performance computing is recommended.
#-------------------------------------------------------------------------------
# Stations should be within some km distance of each other
max_distance = 500
start_date = "20060101"
end_date = "20201231"

# Read in smokePM values at station-days
smokePM = readRDS(file.path(path_smokePM, "smokePM_forMarshall.rds")) %>% 
  filter(date >= ymd(start_date),
         date <= ymd(end_date)) %>% 
  drop_na(smokePM) %>% 
  select(epa_station_id, lon, lat, date, smokePM)

# Find pairs of stations within max_distance of each other
station_ll = smokePM %>% 
  select(epa_station_id, lon, lat) %>% 
  distinct()
station_ll = SpatialPointsDataFrame(station_ll %>% select(lon, lat),
                                    station_ll %>% select(epa_station_id),
                                    proj4string = CRS("+proj=longlat"))
distances = pointDistance(station_ll, lonlat = T, allpairs = T)/1000
station_pairs = which(distances < max_distance, arr.ind = T) %>% 
  as.data.frame() %>% 
  filter(row != col) %>% 
  mutate(epa_station_id_1 = station_ll$epa_station_id[row],
         epa_station_id_2 = station_ll$epa_station_id[col],
         distance_km = distances[cbind(row, col)]) %>% 
  select(epa_station_id_1, epa_station_id_2, distance_km)

# Correlate smokePM values
start_time = get_start_time()
res = mclapply(1:nrow(station_pairs), function(p) {
  print(p)
  station_pair = station_pairs[p, ]
  vals = station_pair %>% 
    left_join(smokePM %>% select(epa_station_id_1 = epa_station_id, date, smokePM_1 = smokePM), 
              by = c("epa_station_id_1")) %>% 
    inner_join(smokePM %>% select(epa_station_id_2 = epa_station_id, date, smokePM_2 = smokePM), 
               by = c("epa_station_id_2", "date"))
  if (nrow(vals) == 0) return(NULL)
  df = data.frame(epa_station_id_1 = station_pair$epa_station_id_1, 
                  epa_station_id_2 = station_pair$epa_station_id_2, 
                  distance_km = station_pair$distance_km,
                  R2 = summary(lm(smokePM_2 ~ smokePM_1, vals))$r.squared,
                  N = nrow(vals))
  return(df)
}, mc.cores = num_cores)
print_time(start_time)
res = bind_rows(res)
saveRDS(res, file.path(path_smokePM, "station_pair_smokePM_correlation.rds"))
