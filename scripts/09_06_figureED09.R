#-------------------------------------------------------------------------------
# Plot Map of Indoor and Outdoor PM2.5
# Written by: Jessica Li
# 
# Plot a map of PM2.5 at indoor and outdoor PurpleAir monitors in the Bay Area 
# at a particular date-time.
#-------------------------------------------------------------------------------
# Choose subset of data based on time
chosen_year = 2020
chosen_months = 8:9

# Choose subset of data based on space
north = 37.47
south = 37.31
east = -121.97
west = -122.32

# Choose specific date-time
local_datetime = "2020-08-19 13"
local_tz = "America/Los_Angeles"
datetime = ymd_h(local_datetime, tz = local_tz)
datetime = with_tz(datetime, "UTC")

if (!dir.exists(path_purpleair)) dir.create(path_purpleair)

# Subset outdoor based on time
# Run commented out lines using HPC because file too large to load in memory
outdoor_file = file.path(path_dropbox, "outdoor_monitor_data_clean_part1.rds")
outdoor_subset_file = paste0(file_path_sans_ext(basename(outdoor_file)), "_", 
                             chosen_year, "_", paste(str_pad(chosen_months, 2, "left", 0), 
                                                     collapse = "-"), ".rds")
outdoor_subset_file = file.path(path_purpleair, outdoor_subset_file)
# outdoor = readRDS(outdoor_file) %>%
#   filter(year == chosen_year,
#          month %in% chosen_months) %>%
#   select(ID_out, Lon_out, Lat_out, time_hours, pm25_out) %>%
#   mutate(
#     # bottom code at 0
#     pm25_out = pmax(pm25_out, 0),
#     # top code any values 500-1000 at 500
#     pm25_out = ifelse(pm25_out >= 500 & pm25_out <= 1000, 500, pm25_out),
#     # set any values > 1000 to NA
#     pm25_out = ifelse(pm25_out > 1000, NA, pm25_out)
#   ) %>%
#   # Drop missing hourly observations
#   drop_na(pm25_out)
# saveRDS(outdoor, outdoor_subset_file)
outdoor = readRDS(outdoor_subset_file)

# Subset indoor data based on time
indoor_file = file.path(path_dropbox, "indoor_monitor_data_clean.rds")
indoor_subset_file = paste0(file_path_sans_ext(basename(indoor_file)), "_",
                            chosen_year, "_", paste(str_pad(chosen_months, 2, "left", 0), 
                                                    collapse = "-"), ".rds")
indoor_subset_file = file.path(path_purpleair, indoor_subset_file)
# indoor = readRDS(indoor_file) %>%
#   filter(year == chosen_year,
#          month %in% chosen_months) %>%
#   select(ID_in, Lon_in, Lat_in, time_hours, pm25_in = pm25_corrected_in) %>%
#   mutate(
#     # bottom code at 0
#     pm25_in = pmax(pm25_in, 0),
#     # top code any values 500-1000 at 500
#     pm25_in = ifelse(pm25_in >= 500 & pm25_in <= 1000, 500, pm25_in),
#     # set any values > 1000 to NA
#     pm25_in = ifelse(pm25_in > 1000, NA, pm25_in)
#   ) %>%
#   # Drop missing hourly observations
#   drop_na(pm25_in)
# saveRDS(indoor, indoor_subset_file)
indoor = readRDS(indoor_subset_file)

# Zoom into snapshot area and specific date-time
outdoor = outdoor %>% 
  filter(time_hours == datetime,
         Lon_out <= east, 
         Lon_out >= west,
         Lat_out <= north, 
         Lat_out >= south) %>% 
  mutate(aqi = con2aqi("pm25", pm25_out),
         aqi_range = cut(aqi, c(0, 50, 100, 150, 200, 300, 500), 
                         include.lowest = T),
         aqi_category = mapvalues(
           aqi_range,
           c("[0,50]", "(50,100]", "(100,150]", 
             "(150,200]", "(200,300]", "(300,500]"),
           c("Good", "Moderate", "Unhealthy for Sensitive Groups", 
             "Unhealthy", "Very unhealthy", "Hazardous")))
indoor = indoor %>% 
  filter(time_hours == datetime,
         Lon_in <= east, 
         Lon_in >= west,
         Lat_in <= north, 
         Lat_in >= south) %>% 
  mutate(aqi = con2aqi("pm25", pm25_in),
         aqi_range = cut(aqi, c(0, 50, 100, 150, 200, 300, 500), 
                         include.lowest = T),
         aqi_category = mapvalues(
           aqi_range,
           c("[0,50]", "(50,100]", "(100,150]", 
             "(150,200]", "(200,300]", "(300,500]"),
           c("Good", "Moderate", "Unhealthy for Sensitive Groups", 
             "Unhealthy", "Very unhealthy", "Hazardous")))

# Build base map
points_margin = 0.01
base_map = openmap(c(north + points_margin, west - points_margin), 
                   c(south - points_margin, east + points_margin), 
                   type = "osm")
# Project to WGS84
base_map = openproj(base_map)
base_map = OpenStreetMap::autoplot.OpenStreetMap(base_map)

# Plot AQI at monitors
aqi_green = "#00e400"
aqi_yellow = "#ffff00"
aqi_orange = "#ff7e00"
aqi_red = "#ff0000"
aqi_purple = "#8f3f97"
aqi_maroon = "#7e0023"
aqi_colors = c(Good = aqi_green, 
               Moderate = aqi_yellow, 
               `Unhealthy for Sensitive Groups` = aqi_orange, 
               Unhealthy = aqi_red, 
               `Very unhealthy` = aqi_purple, 
               Hazardous = aqi_maroon)

p_outdoor = base_map + 
  geom_point(data = outdoor, 
             mapping = aes(Lon_out, Lat_out, color = aqi_category),
             size = 7) + 
  geom_text(data = outdoor,
            mapping = aes(Lon_out, Lat_out, label = aqi),
            size = 2,
            check_overlap = T) +
  scale_color_manual(values = aqi_colors) + 
  theme_void() + 
  labs(color = "AQI Category")

p_indoor = base_map + 
  geom_point(data = indoor,
             mapping = aes(Lon_in, Lat_in, color = aqi_category),
             size = 7) + 
  geom_text(data = indoor,
            mapping = aes(Lon_in, Lat_in, label = aqi),
            size = 2,
            check_overlap = T) +
  scale_color_manual(values = aqi_colors) + 
  theme_void() + 
  labs(color = "AQI Category")

l = get_legend(p_outdoor)
ggsave(file.path(path_github, "figures/raw/figureED09_legend.pdf"),
       plot = l, width = 3, height = 3, units = "in")

p_outdoor = p_outdoor + theme(legend.position = "none")
ggsave(file.path(path_github, "figures/raw/figureED09a.pdf"),
       plot = p_outdoor, width = 10, height = 5, units = "in")

p_indoor = p_indoor + theme(legend.position = "none")
ggsave(file.path(path_github, "figures/raw/figureED09b.pdf"),
       plot = p_indoor, width = 10, height = 5, units = "in")
