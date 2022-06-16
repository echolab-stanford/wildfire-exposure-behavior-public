# use_condaenv("r-reticulate")
if (!py_module_available("cdsapi")) py_install("cdsapi", pip = TRUE)
if (!py_module_available("tenacity")) py_install("tenacity", pip = TRUE)
source_python(file.path(path_supplementary, "define_CDS_API_request.py"))

#-------------------------------------------------------------------------------
# Retrieve ERA5(-Land) Daily Aggregates
# Written by: Jessica Li
# 
# Download ERA5(-Land) daily aggregates using the "Daily statistics calculated 
# from ERA5 data" application 
# (https://cds.climate.copernicus.eu/cdsapp#!/software/app-c3s-daily-era5-statistics?tab=app)
# accessed via the CDS Python API.
#-------------------------------------------------------------------------------
#### Set time zone and area ####
# year and month must be passed in as integers, e.g. 2020L, 3L
years <- 2006:2020
months <- 1:12

# Contiguous US time zones range UTC-04:00 (EDT) to UTC-08:00 (PST), so we use
# UTC-06:00 (CST/MDT)
time_zone <- "UTC-06:00"

# Contiguous US
north <- 49.92
south <- 24.43
east <- -66.62
west <- -125.5

#-------------------------------------------------------------------------------
#### Retrieve data ####
#-------------------------------------------------------------------------------
# Define function for generic retrieval
# Also logs output and messages
retrieve_era5 <- function(variable, statistic, dataset, path_log = "~/Desktop/") {
  if (variable == "total_precipitation") stopifnot(time_zone == "UTC+00:00")
  time_zone_dir <- gsub(":", "", time_zone)
  
  stopifnot(dataset %in% c("global", "land"))
  dataset_dir <- str_to_title(dataset)
  area <- list(lat = c(south, north), lon = c(west, east))
  statistic_run <- statistic
  
  # Full dataset name
  dataset <- ifelse(dataset == "global", "reanalysis-era5-single-levels", "reanalysis-era5-land")
  
  # Start logging output and messages to console
  warn_option <- getOption("warn")
  options(warn = 1)
  log_file_name <- file.path(path_log, sprintf("%s_%s_%s.log", dataset, variable, statistic))
  try(log_file <- file(log_file_name, open="at")) # Open file for appending in text mode
  sink(log_file, type="output", append = TRUE, split = TRUE)
  sink(log_file, type="message", append = TRUE)
  
  for (year in years) {
    for (month in months) {
      start_time <- get_start_time(paste("Started:", variable, year, month))
      retrieve_era5_daily(
        dataset = dataset,
        variable = variable,
        statistic = statistic_run,
        year = year,
        month = month,
        time_zone = time_zone,
        area = area,
        folder = file.path(path_era5, variable)
      )

      run_time <- print_time(start_time)
      
      # Wait a bit before next retrieval if current retrieval ran fast/from cache
      if (run_time < minutes(1)) Sys.sleep(10)
    }
  }
  # Stop logging
  sink(type = "output")
  sink(type = "message")
  close(log_file)
  options(warn = warn_option)
}

# Run retrieval
# Takes 15-90 minutes per month
# 2-meter temperature
retrieve_era5("2m_temperature", "daily_mean", "land")

# Total precipitation
# ERA5-Land total_precipitation is cumulative
time_zone <- "UTC+00:00"
retrieve_era5("total_precipitation", "daily_maximum", "land")



#-------------------------------------------------------------------------------
# Get ERA5 Variables Over 10 km Grid
# Written by: Jessica Li
#-------------------------------------------------------------------------------
# Read in project grid
project_grid0 <- readRDS(file.path(path_boundaries, "grid.RDS"))

# Define function for getting an ERA5 variable over 10 km grid
get_over_grid <- function(dataset, variable, statistic, time_zone, folder, path_era5 = file.path(path_era5)) {
  # Confirm dataset
  stopifnot(dataset %in% c("global", "land"))
  dataset_dir <- str_to_title(dataset)
  time_zone_dir <- gsub(":", "", time_zone)
  
  # Load ERA5 data
  path_in <- file.path(path_era5, variable)
  file_names <- list.files(path_in, full.names = TRUE, pattern = "\\.nc$")
  dat_variable0 <- file_names %>% lapply(stack) %>% stack()
  
  # Get project grid in same CRS
  project_grid <- spTransform(project_grid0, crs(dat_variable0))
  
  # Match project grid cell to overlapping ERA5 grid cell
  project_grid$cell_era5 <- cellFromXY(dat_variable0[[1]], coordinates(project_grid))
  
  # Find nearest neighbor non-NA ERA5 grid cell
  cell_era5_nna <- which(!is.na(values(dat_variable0[[1]])))
  nn <- get.knnx(coordinates(dat_variable0)[cell_era5_nna,],
                 coordinates(project_grid),
                 k = 1)
  cell_era5_na <- which(is.na(values(dat_variable0[[1]])))
  cell_proj_na <- project_grid$cell_era5 %in% cell_era5_na
  
  # Match to NN non-NA ERA5 grid cell in 1 degree if overlap value is NA
  project_grid$cell_era5 <- ifelse(cell_proj_na & nn$nn.dist <= 1,
                                   cell_era5_nna[nn$nn.index],
                                   project_grid$cell_era5)
  
  # Theoretically NN-ing all project grid cells to non-NA ERA5 grid cell should be 
  # equivalent if not for cutoff distance
  # project_grid$cell_era5 <- cell_era5_nna[nn$nn.index]
  
  grid_year_months <- file_names %>% str_sub(-10, -4) %>% unique() %>% sort()
  wind_speed <- variable == "wind_speed"
  if (wind_speed) grid_year_months <- file_names %>% str_sub(-7, -4) %>% unique() %>% sort()
  grid_dates <- names(dat_variable0)
  
  for (grid_year_month in grid_year_months) {
    # Work on one month at a time
    print(paste("Working on:", variable, statistic, grid_year_month, Sys.time()))
    dat_variable <- dat_variable0 %>% subset(grep(gsub("_", "\\.", grid_year_month), 
                                                  grid_dates, value = TRUE))

    # Get daily value in each project grid cell
    start_time <- get_start_time()
    era5_values <- dat_variable[project_grid$cell_era5]
    print_time(start_time)
    
    # Reshape for merging later
    gridded_values <- data.frame(id_grid = project_grid$ID,
                                 era5_values) %>% 
      pivot_longer(cols = -id_grid,
                   names_to = "date",
                   values_to = variable) %>% 
      mutate(date = as.Date(date, format = "X%Y.%m.%d", tz = "UTC"))
    
    # Save daily gridded values
    saveRDS(gridded_values, 
            file.path(
              path_era5, variable, folder,
              paste0(paste("grid", variable, statistic, grid_year_month, sep = "_"), ".rds")
            ))
  }
}

# Get variables over project grid
get_over_grid("land", "2m_temperature", "daily_mean", "UTC-06:00", "grid_temperature")
get_over_grid("land", "total_precipitation", "daily_maximum", "UTC+00:00", "grid_precipitation")
