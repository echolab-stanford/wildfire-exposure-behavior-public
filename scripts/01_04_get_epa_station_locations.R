#-------------------------------------------------------------------------------
# Get EPA Station Locations
# Written by: Anne Driscoll
#-------------------------------------------------------------------------------
# Read in EPA PM2.5 data
epa = readRDS(file.path(path_epa, "epa_station_level_pm25_data.rds"))

# Get coordinates
epa_ll =  epa[!duplicated(epa$id), c("lon", "lat", "id")]

# Make spatial
epa_ll = SpatialPointsDataFrame(epa_ll[, c("lon", "lat")], data=epa_ll)
crs(epa_ll) = "+proj=longlat +datum=WGS84"

# Save
if (!dir.exists(file.path(path_epa, "epa_station_locations"))) dir.create(file.path(path_epa, "epa_station_locations"))
writeOGR(epa_ll, 
         path_epa_ll, 
         "epa_station_locations",
         driver="ESRI Shapefile")
