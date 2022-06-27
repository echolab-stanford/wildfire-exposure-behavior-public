#-------------------------------------------------------------------------------
# Plot Counties in Analysis
# Written by: Anne Driscoll
#-------------------------------------------------------------------------------
# Load counties
counties = readRDS(file.path(path_boundaries, "all_national_counties.RDS"))
counties = counties[!counties$STATEFP %in% str_pad(c(2, 15, 57:78), 2, "left", 0), ]

# Get EPA stations
epa = readRDS(file.path(path_epa, "epa_station_level_pm25_data.rds"))
epa = epa %>% filter(year %in% 2016:2020)
epa_ll =  epa[!duplicated(epa$id), c("lon", "lat", "id")]
epa_ll = SpatialPointsDataFrame(epa_ll[, c("lon", "lat")], data=epa_ll)
crs(epa_ll) = "+proj=longlat +datum=WGS84"
epa_ll = spTransform(epa_ll, crs(counties))

# Get counties with EPA stations
o = over(epa_ll, counties)
counties_inc = unique(o$GEOID)

# Plot
pdf(file.path(path_figures, "figureED01a.pdf"), 
    width=8, height=5)
plot(counties)
plot(add=T, counties[counties$GEOID %in% counties_inc, ], col="red")
dev.off()

#-------------------------------------------------------------------------------
# Calculate population in these counties
pop = raster(file.path(path_population, "gpw_v4_population_count_rev11_2015_2pt5_min.tif"))
counties = spTransform(counties, crs(pop))
counties$pop = exact_extract(pop, counties, 
                             fun = function(x, w) {sum(x*w, na.rm=T)})
sum(counties$pop[counties$GEOID %in% counties_inc])/sum(counties$pop)

# x = load_variables(2019, "acs5")
pop = get_acs(geography="county", variables=paste0("B02001_00", 2:8), 
              year=2019) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from=variable, values_from=estimate) %>%
  rename(white=B02001_002, black=B02001_003, native=B02001_004, 
         asian=B02001_005, pacific=B02001_006, other=B02001_007, two=B02001_008)

colSums(pop[, 2:8])
colSums(pop[pop$GEOID %in% counties_inc, 2:8])

colSums(pop[pop$GEOID %in% counties_inc, 2:8])/colSums(pop[, 2:8])

#-------------------------------------------------------------------------------
# Compute Average County Width
# Written by: Jessica Li
#-------------------------------------------------------------------------------
# Get EPA stations
epa = readRDS(file.path(path_epa, "epa_station_level_pm25_data.rds"))
epa = epa %>% filter(year %in% 2016:2020)
epa_ll =  epa[!duplicated(epa$id), c("lon", "lat", "id")]
epa_ll = SpatialPointsDataFrame(epa_ll[, c("lon", "lat")], data=epa_ll)
crs(epa_ll) = "+proj=longlat +datum=WGS84"

# Get counties include in analysis
counties = readRDS(file.path(path_boundaries, "all_national_counties.RDS"))
counties = counties[!counties$STATEFP %in% str_pad(c(2, 15, 57:78), 2, "left", 0), ]
epa_ll = spTransform(epa_ll, crs(counties))
o = over(epa_ll, counties)
counties_inc = unique(o$GEOID)
counties_inc = counties[counties$GEOID %in% counties_inc,]

if (!file.exists(file.path(path_boundaries, "counties_included.rds"))) {
  saveRDS(counties_inc, file.path(path_boundaries, "counties_included.rds"))
}

# Calculate width
county_area_sqkm = area(counties_inc)/1000/1000
county_width_km = sqrt(county_area_sqkm)
county_avg_width_km = mean(county_width_km)
county_avg_width_km

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

if (!file.exists(file.path(path_smokePM, "station_pair_smokePM_correlation.rds"))) {
  
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
}

#-------------------------------------------------------------------------------
# Figure ED 1 Panel b
# Written by: Marshall Burke
#-------------------------------------------------------------------------------
# Load correlations
dt <- read_rds(file.path(path_smokePM, "station_pair_smokePM_correlation.rds"))
dt <- dt %>% filter(N>=1000) %>% mutate(r = sqrt(R2))
dt <- mutate(dt,bin=floor(distance_km/10)*10+5)
meds <- dt %>% group_by(bin) %>% summarise(r_med = median(r))
dt <- left_join(dt,meds)

# Plot
pdf(file=file.path(path_figures, "figureED01b.pdf"),width=6,height=5)
ggplot(dt, aes(x=distance_km, y=r) ) +
  geom_bin2d(bins = 50) +
  scale_fill_continuous(type = "viridis") +
  xlab("pairwise distance (km)") + ylab("r (pearson)") + 
  #  geom_smooth(color="black")  +
  geom_line(aes(x=bin,y=r_med),size=1.2) +
  theme_minimal()
dev.off()

#-------------------------------------------------------------------------------
# Assemble Figure ED 1
# Written by: Jessica Li
#-------------------------------------------------------------------------------
# Load panels
panel_a = image_read_pdf(file.path(path_figures, "figureED01a.pdf"))
panel_b = image_read_pdf(file.path(path_figures, "figureED01b.pdf"))

# Trim and scale
panel_a = image_trim(panel_a)
panel_a = image_scale(panel_a, "x1500")
panel_a = image_border(panel_a, "none", "x50")
panel_a = image_annotate(panel_a, "a", font = "Arial", size = 12, weight = 700)

panel_b = image_trim(panel_b)
panel_b = image_scale(panel_b, "x1500")
panel_b = image_border(panel_b, "none", "x50")
panel_b = image_annotate(panel_b, "b", font = "Arial", size = 12, weight = 700)

# Combine
fig = image_append(c(panel_a, panel_b))

# Save
image_write(fig, path = file.path(path_figures, "figureED01.pdf"), format = "pdf")
