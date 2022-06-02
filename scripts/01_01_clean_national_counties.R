#-------------------------------------------------------------------------------
# Clean National Counties
# Written by: Anne Driscoll
#-------------------------------------------------------------------------------
#### All national counties ####
# Read in TIGER line file
counties = readOGR(file.path(path_dropbox, "tl_2019_us_county"), "tl_2019_us_county")

# Simplify
c = gSimplify(counties, 0.008, topologyPreserve=F)
c = SpatialPolygonsDataFrame(c, counties@data)
c = clgeo_Clean(c)

# Convert lat/lon to numeric
c$INTPTLAT = as.numeric(as.character(c$INTPTLAT))
c$INTPTLON = as.numeric(as.character(c$INTPTLON))

# Save
counties_file = file.path(path_dropbox, "all_national_counties.rds")
if (!file.exists(counties_file)) saveRDS(c, counties_file)

#-------------------------------------------------------------------------------
#### CONUS counties ####
# As used in Burke et al 2021 "The changing risk and burden of wildfire in the United States"
# Read in TIGER line file
counties = readOGR(file.path(path_dropbox, "tl_2019_us_county"), "tl_2019_us_county")

# Convert lat/lon to numeric
counties$AWATER = as.numeric(as.character(counties$AWATER))
counties$ALAND = as.numeric(as.character(counties$ALAND))

# Limit to CONUS
counties = counties[!counties$STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"), ]

# Simplify
crs_using = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # "+init=epsg:4238"
counties = spTransform(counties, CRS(crs_using))
counties_simp = gSimplify(counties, 0.05, topologyPreserve=T)
counties_simp = SpatialPolygonsDataFrame(counties_simp, counties@data)

# Save
counties_file = file.path(path_dropbox, "counties.RDS")
if (!file.exists(counties_file)) saveRDS(counties_simp, counties_file)
