#-------------------------------------------------------------------------------
# Get EPA Station Locations
# Written by: Anne Driscoll
# Last edited by: Jessica Li
#-------------------------------------------------------------------------------
# Read in EPA PM2.5 data
epa = readRDS(file.path(path_dropbox, "epa_station_level_pm25_data.rds"))

# Get coordinates
epa_ll =  epa[!duplicated(epa$id), c("lon", "lat", "id")]

# Make spatial
epa_ll = SpatialPointsDataFrame(epa_ll[, c("lon", "lat")], data=epa_ll)
crs(epa_ll) = "+proj=longlat +datum=WGS84"

# Save
path_epa_ll = file.path(path_dropbox, "epa_station_locations")
if (!dir.exists(path_epa_ll)) dir.create(path_epa_ll)
writeOGR(epa_ll, 
         path_epa_ll, 
         "epa_station_locations",
         driver="ESRI Shapefile")

# states = readOGR(file.path(path_dropbox, "tl_2019_us_state"), "tl_2019_us_state")
# states = states[states$STATEFP == "06", ]
# crs(epa_ll) = crs(states)
# which_ca = over(epa_ll, states)
# epa_ll_ca = epa_ll[!is.na(which_ca$STATEFP), ]
# writeOGR(epa_ll_ca, 
#          file.path(path_dropbox, "CA_epa_station_locations"), 
#          "CA_epa_station_locations",
#          driver="ESRI Shapefile")
