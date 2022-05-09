num_cores <- 8
path_era5 = file.path(path_dropbox, "ERA5")
path_era5_precipitation = file.path(path_era5, "total_precipitation")
path_era5_precipitation_grid = file.path(path_era5_precipitation, "grid_precipitation")
if (!dir.exists(path_era5_precipitation_grid)) dir.create(path_era5_precipitation_grid)

#-------------------------------------------------------------------------------
# Get Precipitation from ERA5
# Written by: Jessica Li
# Lasted edited by: Jessica Li
#-------------------------------------------------------------------------------
# Load precipitation data
dat_precip0 <- brick(file.path(path_era5_precipitation, "era5_2006-2020_daily_precip.nc"))

# Read in project grid as shape in same CRS
project_grid <- readRDS(file.path(path_github, "data/grid.RDS"))
project_grid <- spTransform(project_grid, crs(dat_precip0))

# Match project grid cell to overlapping ERA5 grid cell
project_grid$cell_precip <- cellFromXY(dat_precip0[[1]], coordinates(project_grid))

#-------------------------------------------------------------------------------
grid_years <- 2006:2020
grid_dates <- names(dat_precip0)
nc <- ncell(project_grid)/nlayers(project_grid)

registerDoParallel(num_cores)
for (grid_year in grid_years) {
  # Work on one year at a time
  print(paste("Working on:", grid_year, Sys.time()))
  dat_precip <- dat_precip0 %>% subset(grep(grid_year, grid_dates, value = TRUE))
  
  # Get daily precipitation in each project grid cell
  nl <- nlayers(dat_precip)
  start_time <- get_start_time()
  precipitation <- foreach(i = 1:nl, .combine = c) %dopar% dat_precip[[i]][project_grid$cell_precip]
  print_time(start_time)
  
  precipitation <- data.frame(id_grid = project_grid$ID, 
                              date = rep(names(dat_precip), each = nc),
                              precipitation = precipitation) %>% 
    mutate(date = as.Date(date, format = "X%Y.%m.%d", tz = "UTC"))
  
  # Save daily grid precipitation values
  saveRDS(precipitation, file.path(path_era5_precipitation_grid, paste0("grid_precipitation_", grid_year, ".rds")))
}
stopImplicitCluster()
