#-------------------------------------------------------------------------------
# Prepare PurpleAir, CoreLogic, and ACS Data
# Written by: Jessica Li
#-------------------------------------------------------------------------------
# Read in PA data
bad_est <- function(est) return(est < 0 | est > 1)

# N = 2455 (PA, not posterior)
# N = 2455 (PA, posterior)
# N = 1509 (EPA, not posterior)
# N = 1509 (EPA, posterior)
# N = 1520 (PC, not posterior)
# N = 1520 (PC, posterior)
dat_pa <- readRDS(pa_file)

# N = 1534 (PA, not posterior)
# N = 1534 (PA, posterior)
# N = 1509 (EPA, not posterior)
# N = 1509 (EPA, posterior)
# N = 1520 (PC, not posterior)
# N = 1520 (PC, posterior)
if (pm_method %in% c("PA", "EPA")) dat_pa <- dat_pa %>% filter(building_type == "sfr")

# N = 1511 (PA, not posterior)
# N = 1511 (PA, posterior)
# N = 1505 (EPA, not posterior)
# N = 1505 (EPA, posterior)
# N = 1501 (PC, not posterior)
# N = 1501 (PC, posterior)
dat_pa <- dat_pa %>% 
  mutate(est_dl = ifelse(bad_est(est_dl), NA, est_dl),
         est_ldv = ifelse(bad_est(est_ldv), NA, est_ldv),
         est_nl = ifelse(bad_est(est_nl), NA, est_nl),
         est_al = ifelse(bad_est(est_al), NA, est_al)) %>% 
  # Get only observations with infiltration estimates in [0, 1]
  drop_na(est_dl, est_ldv) %>% 
  select(id, lon, lat, state, est_dl, se_dl, est_ldv, se_ldv, est_nl, se_nl, est_al, se_al)

#-------------------------------------------------------------------------------
# Read in ACS data
conus <- setdiff(states()$STUSPS, c("AK", "AS", "GU", "HI", "MP", "PR", "VI"))
vars_acs_all <- load_variables(year = 2019, dataset = "acs5", cache = TRUE)
# vars_race <- paste0("B02001_00", 2:7)
vars_race <- paste0("B03002_00", 3:8) # non-HL race variables
vars_acs <- c("B01003_001", vars_race, "B03001_003", "B17017_001", "B17017_002",
              "B19013_001", "B22003_001", "B22003_002", "B25008_002", "B25008_003")
dat_acs <- get_acs(geography = "tract",
                   variables = vars_acs,
                   output = "wide",
                   year = 2019,
                   survey = "acs5",
                   state = conus) %>%
  select(GEOID, matches("^B.*E$"))
names(dat_acs)[-1] <- c("pop_total", "race_white", "race_black", "race_aian", 
                        "race_asian", "race_nhpi", "race_other", "ethn_hl",
                        "poverty_univ", "poverty_below", "income_median",
                        "food_stamps_univ", "food_stamps_received",
                        "occ_owner", "occ_renter")
dat_acs <- dat_acs %>%
  mutate(race_white = ifelse(pop_total, race_white/pop_total, 0),
         race_black = ifelse(pop_total, race_black/pop_total, 0),
         race_aian = ifelse(pop_total, race_aian/pop_total, 0),
         race_asian = ifelse(pop_total, race_asian/pop_total, 0),
         race_nhpi = ifelse(pop_total, race_nhpi/pop_total, 0),
         race_other = ifelse(pop_total, race_other/pop_total, 0),
         ethn_hl = ifelse(pop_total, ethn_hl/pop_total, 0),
         poverty_below = ifelse(poverty_univ, poverty_below/poverty_univ, 0),
         food_stamps_received = ifelse(food_stamps_univ, food_stamps_received/food_stamps_univ, 0),
         occ_renter = ifelse(occ_owner + occ_renter, occ_renter/(occ_owner + occ_renter), 0)) %>%
  select(-poverty_univ, -food_stamps_univ, -occ_owner)

#-------------------------------------------------------------------------------
# Read in cooling/heating degree days data
files <- list.files(file.path(path_dropbox, "temperature/"), 
                    pattern = "^tract_level", full.names = TRUE)
years <- as.numeric(substr(files, 69, 72))
variable <- substr(files, 65, 67)
degree_days <- files %>% lapply(read.csv)
degree_days <- mapply(mutate, year = years, vbl = variable, degree_days, SIMPLIFY = FALSE) %>% 
  bind_rows() %>% 
  pivot_wider(names_from = vbl,
              values_from = mean) %>% 
  group_by(GEOID) %>% 
  summarize(cdd = mean(cdd, na.rm = TRUE),
            hdd = mean(hdd, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(GEOID = str_pad(GEOID, 11, "left", 0))

# Merge into ACS data
dat_acs <- merge(dat_acs, degree_days, by = "GEOID")

#-------------------------------------------------------------------------------
# Read in physiographic province data
physio <- readOGR(file.path(path_dropbox, "physio_shp/physio.shp"))

# Combine boundaries since not all are covered
# Provinces are only combined with those adjacent to them and in the same division
province_from <- c("LOWER CALIFORNIAN", "COLUMBIA PLATEAU", "NORTHERN ROCKY MOUNTAINS",
                   "VALLEY AND RIDGE", "ST. LAWRENCE VALLEY", "ADIRONDACK", "NEW ENGLAND")
province_to <- c("PACIFIC BORDER", "BASIN AND RANGE", "MIDDLE ROCKY MOUNTAINS",
                 "BLUE RIDGE", "APPALACHIAN PLATEAUS", "APPALACHIAN PLATEAUS", 
                 "PIEDMONT")
physio$PROVINCE <- mapvalues(physio$PROVINCE, province_from, province_to)
physio <- physio[!is.na(physio$DIVISION),]

#-------------------------------------------------------------------------------
# Save data sets
saveRDS(dat_pa, file.path(path_infiltration, pm_path, post_path, "dat_pa_inf.rds"))
saveRDS(dat_acs, file.path(path_dropbox, "dat_acs_chars.rds"))
saveRDS(physio, file.path(path_dropbox, "physio.rds"))
