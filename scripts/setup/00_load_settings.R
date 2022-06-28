#-------------------------------------------------------------------------------
# Provide your US Census API Key
key <- readRDS("../../Registrations/US Census/us_census_data_api_key.rds")
census_api_key(key)

# Set the number of cores to use in parallel computing
num_cores = 6

# Choose whether to run tuning
run_tuning = F

#-------------------------------------------------------------------------------
# Set to location of Dropbox and GitHub folders
path_dropbox = "~/BurkeLab Dropbox/Projects/wildfire-exposure-behavior/4 TEST/"
path_github = "~/Documents/GitHub/wildfire-exposure-behavior-public/"

# File paths based on root folders above
path_atus = file.path(path_dropbox, "ATUS")
path_purpleair = file.path(path_dropbox, "PurpleAir")
path_fire = file.path(path_dropbox, "fire")
path_era5 = file.path(path_dropbox, "ERA5")
path_safegraph = file.path(path_dropbox, "SafeGraph")
path_infiltration = file.path(path_dropbox, "infiltration")
path_twitter = file.path(path_dropbox, "Twitter")
path_gtrends = file.path(path_dropbox, "Google Trends")
path_smoke = file.path(path_dropbox, "smoke")
path_beh_bootstraps = file.path(path_dropbox, "behavioral_bootstraps")
path_epa = file.path(path_dropbox, "EPA")
path_population = file.path(path_dropbox, "population")
path_prism = file.path(path_dropbox, "PRISM")
path_boundaries = file.path(path_dropbox, "boundaries")
path_smokePM = file.path(path_dropbox, "smokePM")

path_tables = file.path(path_github, "tables", "raw")
path_figures = file.path(path_github, "figures", "raw")
path_setup = file.path(path_github, "scripts", "setup")
path_main = file.path(path_github, "scripts", "main")
path_supplementary = file.path(path_github, "scripts", "supplementary")

#-------------------------------------------------------------------------------
# Choose method of estimating PM2.5 concentrations
# Options are: "PA"  (PurpleAir concentrations without correction)
#              "EPA" (PurpleAir concentrations with EPA correction)
#              "PC"  (PurpleAir particle count-derived concentrations)
pm_method <- "PC"

# Choose lag structure in estimating household-specific infiltration
# Options are: "dl"  (distributed lag)
#              "ldv" (lagged dependent variable)
#              "al"  (all lags)
#              "nl"  (no lag)
lag_structure <- "dl"

# Choose if using posterior infiltration estimates
# Options are: FALSE (prior)
#              TRUE  (posterior)
posterior <- TRUE

# File paths based on choices above
if (pm_method == "PA") {
  pa_file <- file.path(path_infiltration, "estimates", "purpleair_infiltration_estimates_by_model.rds")
} else if (pm_method == "EPA") {
  pa_file <- file.path(path_infiltration, "estimates", "PA_monitor_level_infiltration_estimates_sfr_clean.rds")
} else if (pm_method == "PC") {
  pa_file <- file.path(path_infiltration, "estimates", "PA_monitor_level_infiltration_estimates_sfr_clean_pc.rds")
}
post_file <- paste0(file_path_sans_ext(pa_file), "_posterior.rds")
post_path <- "not_posterior/"

if (posterior) {
  pa_file <- post_file
  post_path <- "posterior/"
}
pm_path <- paste0(pm_method, "/")
if (!dir.exists(file.path(path_infiltration, "heterogeneity", pm_path, post_path))) {
  dir.create(file.path(path_infiltration, "heterogeneity", pm_path, post_path), recursive = T)
}

pa_files = c(
  file.path(path_infiltration, "estimates", "purpleair_infiltration_estimates_by_model.rds"),
  file.path(path_infiltration, "estimates", "PA_monitor_level_infiltration_estimates_sfr_clean.rds"),
  file.path(path_infiltration, "estimates", "PA_monitor_level_infiltration_estimates_sfr_clean_pc.rds")
)
post_files = paste0(file_path_sans_ext(pa_files), "_posterior.rds") %>% 
  setNames(pa_files)
pa_files = pa_files %>% setNames(post_files)
pm_paths = c("PA", "EPA", "PC")
post_paths = c("not_posterior", "posterior")
