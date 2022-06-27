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
