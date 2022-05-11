counties = readRDS(file.path(path_dropbox, "all_national_counties.RDS"))
counties = counties[!counties$STATEFP %in% c("02", 15, 57:78), ]

epa = readRDS(file.path(path_dropbox, "epa_station_level_pm25_data.rds"))
epa = epa %>% filter(year %in% 2016:2020)
epa_ll =  epa[!duplicated(epa$id), c("lon", "lat", "id")]
epa_ll = SpatialPointsDataFrame(epa_ll[, c("lon", "lat")], data=epa_ll)
crs(epa_ll) = "+proj=longlat +datum=WGS84"
epa_ll = spTransform(epa_ll, crs(counties))

o = over(epa_ll, counties)
counties_inc = unique(o$GEOID)

pdf(file.path(path_github, "figures/figureED01a.pdf"), 
     width=8, height=5)
plot(counties)
plot(add=T, counties[counties$GEOID %in% counties_inc, ], col="red")
dev.off()

pop = raster(file.path(path_dropbox, "gpw_v4_population_count_rev11_2015_2pt5_min.tif"))
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
