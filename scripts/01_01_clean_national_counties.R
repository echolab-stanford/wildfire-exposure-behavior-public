#-------------------------------------------------------------------------------
# Clean National Counties
# Written by: Anne Driscoll
# Last edited by: Jessica Li
#-------------------------------------------------------------------------------
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
saveRDS(c, file.path(path_dropbox, "all_national_counties.rds"))

#-------------------------------------------------------------------------------
# Copied from wildfire-map-public
# The Census TIGER line file is not provided in the repo - if you'd like to 
# download it to recreate from base data it can be downloaded at:
# https://www2.census.gov/geo/tiger/TIGER2019/COUNTY/
counties = readOGR(file.path(path_dropbox, "tl_2019_us_county"), "tl_2019_us_county")
counties$AWATER = as.numeric(as.character(counties$AWATER))
counties$ALAND = as.numeric(as.character(counties$ALAND))
counties = counties[!counties$STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"), ]
crs_using = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #"+init=epsg:4238" #
counties = spTransform(counties, CRS(crs_using))
counties_simp = gSimplify(counties, 0.05, topologyPreserve=T)
counties_simp = SpatialPolygonsDataFrame(counties_simp, counties@data)
# saveRDS(counties_simp, file.path(path_dropbox, "counties.RDS"))
