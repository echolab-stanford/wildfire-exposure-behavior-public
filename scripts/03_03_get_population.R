#-------------------------------------------------------------------------------
# Get Population Over 10km Grid
# Written by: Anne Driscoll
#-------------------------------------------------------------------------------
# Read in data needed
pop = raster(file.path(path_population, "gpw_v4_population_count_rev11_2010_2pt5_min.tif"))

# Define grid as shapes rather than points
grid = readRDS(file.path(path_boundaries, "grid.RDS"))
grid = gBuffer(grid, byid=T, width=5000, capStyle="SQUARE")
grid = spTransform(grid, crs(pop))

# Extract
pop = crop(pop, extent(grid))
grid_pop = exact_extract(pop, grid, 
                          fun=function(val, wts){sum(val, na.rm=T)})
data_out = data.frame(id=grid$ID, pop=grid_pop)
saveRDS(data_out, file.path(path_population, "grid_population.RDS"))
