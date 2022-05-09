path_fire = file.path(path_dropbox, "fire")
path_fire_processed = file.path(path_fire, "processed")
path_fire_grid = file.path(path_fire_processed, "grid_dist_fire")

#-------------------------------------------------------------------------------
# Get County Distance to Fire
# Written by: Anne Driscoll
# Last edited by: Jessica Li
# 
# Get population weighted distance to nearest fire point for all county-days.
#-------------------------------------------------------------------------------
# read in data
crs_m = "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
years = 2006:2020

grid_pop = readRDS(file.path(path_dropbox, "grid_population.RDS"))
grid = readOGR(file.path(path_dropbox, "10km_grid"), "10km_grid")
grid = spTransform(grid, crs_m)

counties = readRDS(file.path(path_dropbox, "all_national_counties.RDS"))
counties = counties[counties$INTPTLAT > 24.7 & counties$INTPTLAT < 49.3 &
                      counties$INTPTLON > -125.2 & counties$INTPTLON < -66.4, ]
counties = spTransform(counties, crs_m)


# get the county to grid mapping
crosswalk = over(counties, grid, returnList=T)
for (i in 1:length(crosswalk)) {
  cur = crosswalk[[i]]
  if (nrow(cur) == 0) {next}
  cur$county = counties$GEOID[i]
  crosswalk[[i]] = cur
}
w = sapply(crosswalk, nrow) > 0
crosswalk = rbindlist(crosswalk[w])
crosswalk = merge(crosswalk, grid_pop, by.x="ID", by.y="id", all.x=T)
crosswalk = crosswalk %>% select(county, ID, pop)


for (i in 1:length(years)) {
  
  year = years[i]
  
  dists = readRDS(file.path(path_fire_grid, paste0("dist_to_fire_",
                                                   year, ".RDS")))
  dists = merge(dists, crosswalk, by.x="id", by.y="ID",
                all=T, allow.cartesian=T)
  dists = dists %>%
    group_by(county, date) %>%
    summarise(km_dist = wtd.mean(km_dist, weights=pop))
  
  
  saveRDS(dists, file.path(path_fire_processed, paste0("county_pop_weighted_dist_to_fire_",
                                                       year, ".RDS")))
  
}
