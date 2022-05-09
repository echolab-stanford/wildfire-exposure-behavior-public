#-------------------------------------------------------------------------------
# Create 10 km Grid
# Written by: Anne Driscoll
# Last edited by: Jessica Li
# 
# Create a 10 km grid across the contiguous US.
#-------------------------------------------------------------------------------
proj = "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs"
county_proj = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
res = 10000

# Create grid
counties = readRDS(file.path(path_dropbox, "all_national_counties.rds"))
counties = counties[!counties$STATEFP %in% 
                      c("02", "15", "60", "66", "69", "72", "78"), ]
counties = spTransform(counties, proj)
c = gBuffer(counties, width = res/2)
grid = CreateGrid(counties, resolution=res, returnclass="sp")

# Remove grid cells that don't overlap a state
keep = over(grid, counties)
keep = which(!is.na(keep$STATEFP))
data_ll = grid[keep, ]
saveRDS(data_ll, file.path(path_github, "data/grid.RDS"))

# Make into an actual grid rather than points
# data_ll = spTransform(data_ll, county_proj)
data_ll = gBuffer(data_ll, byid=T, width=res/2, capStyle="SQUARE") #slow
data_ll = st_as_sf(data_ll)
st_write(obj=data_ll, dsn=paste0(path_dropbox, "10km_grid"),
         layer="10km_grid", driver="ESRI Shapefile", append=F)
