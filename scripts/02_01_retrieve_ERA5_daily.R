# use_condaenv("r-reticulate")
if (!py_module_available("cdsapi")) py_install("cdsapi", pip = TRUE)
if (!py_module_available("tenacity")) py_install("tenacity", pip = TRUE)
source_python(file.path(path_github, "scripts/define_CDS_API_request.py"))

#-------------------------------------------------------------------------------
# Retrieve ERA5(-Land) Daily Aggregates
# Written by: Jessica Li
# Last edited by: Jessica Li
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

# Output directory
path_out <- file.path(path_dropbox, "ERA5/")

#-------------------------------------------------------------------------------
#### Retrieve data ####
#-------------------------------------------------------------------------------
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
  try(log_file <- file(log_file_name, open="at")) # open file for appending in text mode
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
        folder = file.path(path_out, dataset_dir, variable, "USA", "raw", time_zone_dir,
                           paste0(statistic, "_of_1-hourly"))
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

# Set temporary working directory
path_temp <- paste0(path_out, "temp/")
if (!dir.exists(path_temp)) dir.create(path_temp)
setwd(path_temp)

#-------------------------------------------------------------------------------
#### ERA5-Land ####
# Takes 15-90 minutes per month

# 2-meter temperature
retrieve_era5("2m_temperature", "daily_mean", "land")

# Total precipitation
# ERA5-Land total_precipitation is cumulative
time_zone <- "UTC+00:00"
retrieve_era5("total_precipitation", "daily_maximum", "land")

# Remove temporary working directory
setwd(path_github)
unlink(path_temp, recursive = TRUE)
