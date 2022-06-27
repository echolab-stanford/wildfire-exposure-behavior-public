#-------------------------------------------------------------------------------
# Clean National Counties
# Written by: Anne Driscoll
#-------------------------------------------------------------------------------
#### All national counties ####
# Read in TIGER line file
counties = readOGR(file.path(path_boundaries, "tl_2019_us_county"), "tl_2019_us_county")

# Simplify
c = gSimplify(counties, 0.008, topologyPreserve=F)
c = SpatialPolygonsDataFrame(c, counties@data)
c = clgeo_Clean(c)

# Convert lat/lon to numeric
c$INTPTLAT = as.numeric(as.character(c$INTPTLAT))
c$INTPTLON = as.numeric(as.character(c$INTPTLON))

# Save
if (!file.exists(file.path(path_boundaries, "all_national_counties.rds"))) {
  saveRDS(c, file.path(path_boundaries, "all_national_counties.rds"))
}

#-------------------------------------------------------------------------------
#### CONUS counties ####
# As used in Burke et al 2021 "The changing risk and burden of wildfire in the United States"
# Read in TIGER line file
counties = readOGR(file.path(path_boundaries, "tl_2019_us_county"), "tl_2019_us_county")

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
if (!file.exists(file.path(path_boundaries, "counties.RDS"))) {
  saveRDS(counties_simp, file.path(path_boundaries, "counties.RDS"))
}

#-------------------------------------------------------------------------------
# Create 10km Grid
# Written by: Anne Driscoll
# 
# Create a 10 km grid across the contiguous US.
#-------------------------------------------------------------------------------
# Set projections and grid resolution
proj = "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs"
county_proj = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
res = 10000

# Create grid
counties = readRDS(file.path(path_boundaries, "all_national_counties.rds"))
counties = counties[!counties$STATEFP %in% 
                      c("02", "15", "60", "66", "69", "72", "78"), ]
counties = spTransform(counties, proj)
c = gBuffer(counties, width = res)
grid = CreateGrid(counties, resolution=res, returnclass="sp")
grid = spTransform(grid, proj)

# Remove grid cells that don't overlap a state
keep = over(grid, c)
keep = which(!is.na(keep))
data_ll = grid[keep, ]
if (!file.exists(file.path(path_boundaries, "grid.RDS"))) {
  saveRDS(data_ll, file.path(path_boundaries, "grid.RDS"))
}

# Make into an actual grid rather than points
data_ll = spTransform(data_ll, proj)
data_ll = gBuffer(data_ll, byid=T, width=res/2, capStyle="SQUARE")
data_ll = st_as_sf(data_ll)
if (!dir.exists(file.path(path_boundaries, "10km_grid"))) {
  dir.create(file.path(path_boundaries, "10km_grid"))
  st_write(obj=data_ll, dsn=file.path(path_boundaries, "10km_grid"),
           layer="10km_grid", driver="ESRI Shapefile", append=F)
}
