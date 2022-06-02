#-------------------------------------------------------------------------------
# Compute Average County Width
# Written by: Jessica Li
#-------------------------------------------------------------------------------
# Get counties include in analysis
counties = readRDS(file.path(path_dropbox, "all_national_counties.RDS"))
counties = counties[!counties$STATEFP %in% c("02", 15, 57:78), ]
epa_ll = readOGR(file.path(path_dropbox, "epa_station_locations"), "epa_station_locations")
epa_ll = spTransform(epa_ll, crs(counties))
o = over(epa_ll, counties)
counties_inc = unique(o$GEOID)
counties_inc = counties[counties$GEOID %in% counties_inc,]

saveRDS(counties_inc, file.path(path_dropbox, "counties_included.rds"))

# Calculate width
county_area_sqkm = area(counties_inc)/1000/1000
county_width_km = sqrt(county_area_sqkm)
county_avg_width_km = mean(county_width_km)
county_avg_width_km
