#-------------------------------------------------------------------------------
# Get Population Over 10km Grid
# Written by: Anne Driscoll
# Last edited by: Jessica Li
#-------------------------------------------------------------------------------
# read in data needed
pop = raster(file.path(path_dropbox, "gpw_v4_population_count_rev11_2010_2pt5_min.tif"))

# define grid as shapes rather than points
grid = readRDS(file.path(path_github, "data/grid.RDS"))
grid = gBuffer(grid, byid=T, width=5000, capStyle="SQUARE") #slow
grid = spTransform(grid, crs(pop))

# extract
pop = crop(pop, extent(grid))
grid_pop = exact_extract(pop, grid, 
                          fun=function(val, wts){sum(val, na.rm=T)})
data_out = data.frame(id=grid$ID, pop=grid_pop)
saveRDS(data_out, file.path(path_dropbox, "grid_population.RDS"))
