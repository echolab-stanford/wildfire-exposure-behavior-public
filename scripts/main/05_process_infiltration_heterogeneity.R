#-------------------------------------------------------------------------------
# Get Posterior Infiltration Estimates
# Written by: Jessica Li, Marissa Childs
# 
# Be sure to run with posterior set to FALSE in settings.
#-------------------------------------------------------------------------------
for (post_f in post_files) {
  if (!file.exists(post_f)) {
    
    stopifnot(!posterior)
    
    # Read in infiltration estimates
    dat_inf_full <- readRDS(pa_files[post_f])
    
    #-------------------------------------------------------------------------------
    # Distributed lag model
    dat_inf <- dat_inf_full %>%
      mutate(est_dl = ifelse(est_dl < 0 | est_dl > 1, NA, est_dl)) %>% 
      drop_na(est_dl, se_dl)
    if (pm_method %in% c("PA", "EPA")) dat_inf <- dat_inf %>% filter(building_type == "sfr")
    
    # Set up named data list
    stan_data <- list(
      J = nrow(dat_inf),
      tau_hat = dat_inf$est_dl,
      se_hat = dat_inf$se_dl
    )
    
    # Fit model
    set.seed(222)
    stan_fit <- stan(
      # Stan program
      file = file.path(path_supplementary, "define_bayesian_hierarchical_model.stan"),
      data = stan_data,
      chains = 4,             # number of Markov chains
      warmup = 1000,          # number of warm-up iterations per chain
      iter = 2000,            # total number of iterations per chain
      cores = 2,              # number of cores 
      refresh = 0             # no progress shown
    )
    
    # Get posteriors' means
    post_inf <- summary(stan_fit, pars = "theta")$summary[, c("mean", "se_mean")] %>% 
      as.data.frame() %>% 
      rename(post_est_dl = mean, post_se_dl = se_mean)
    dat_inf <- dat_inf %>% 
      mutate(est_dl = post_inf$post_est_dl,
             se_dl = post_inf$post_se_dl)
    dat_inf_dl <- dat_inf %>% select(id, est_dl, se_dl)
    
    #-------------------------------------------------------------------------------
    # No lag model
    dat_inf <- dat_inf_full %>% 
      mutate(est_nl = ifelse(est_nl < 0 | est_nl > 1, NA, est_nl)) %>% 
      drop_na(est_nl, se_nl)
    if (pm_method %in% c("PA", "EPA")) dat_inf <- dat_inf %>% filter(building_type == "sfr")
    
    # Set up named data list
    stan_data <- list(
      J = nrow(dat_inf),
      tau_hat = dat_inf$est_nl,
      se_hat = dat_inf$se_nl
    )
    
    # Fit model
    set.seed(222)
    stan_fit <- stan(
      # Stan program
      file = file.path(path_supplementary, "define_bayesian_hierarchical_model.stan"),
      data = stan_data,
      chains = 4,             # number of Markov chains
      warmup = 1000,          # number of warm-up iterations per chain
      iter = 2000,            # total number of iterations per chain
      cores = 2,              # number of cores 
      refresh = 0             # no progress shown
    )
    
    # Get posteriors' means
    post_inf <- summary(stan_fit, pars = "theta")$summary[, c("mean", "se_mean")] %>% 
      as.data.frame() %>% 
      rename(post_est_nl = mean, post_se_nl = se_mean)
    dat_inf <- dat_inf %>% 
      mutate(est_nl = post_inf$post_est_nl,
             se_nl = post_inf$post_se_nl)
    dat_inf_nl <- dat_inf %>% select(id, est_nl, se_nl)
    
    #-------------------------------------------------------------------------------
    # Join posterior estimates and SEs to original data frame
    o <- names(dat_inf_full)
    dat_inf <- dat_inf_full %>% select(-est_dl, -se_dl, -est_nl, -se_nl)
    dat_inf <- list(dat_inf, dat_inf_dl, dat_inf_nl) %>% 
      reduce(left_join) %>% 
      select(o)
    
    # Save updated data
    saveRDS(dat_inf, post_f)
  }
}

#-------------------------------------------------------------------------------
# Prepare PurpleAir, CoreLogic, and ACS Data
# Written by: Jessica Li
#-------------------------------------------------------------------------------
for (pm_p in pm_paths) {
  for (post_p in post_paths) {
    if (!file.exists(file.path(path_infiltration, "heterogeneity", pm_p, post_p, "dat_pa_inf.rds"))) {
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
      
      saveRDS(dat_pa, file.path(path_infiltration, "heterogeneity", pm_p, post_p, "dat_pa_inf.rds"))
    }
  }
}

#-------------------------------------------------------------------------------
if (!file.exists(file.path(path_infiltration, "heterogeneity", "dat_acs_chars.rds"))) {
  
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
  files <- list.files(file.path(path_prism, "temperature"), 
                      pattern = "^tract_level", full.names = TRUE)
  years <- as.numeric(substr(basename(files), 17, 20))
  variable <- substr(basename(files), 13, 15)
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
  
  saveRDS(dat_acs, file.path(path_infiltration, "heterogeneity", "dat_acs_chars.rds"))
}

#-------------------------------------------------------------------------------
if (!file.exists(file.path(path_boundaries, "physio.rds"))) {
  
  # Read in physiographic province data
  physio <- readOGR(file.path(path_boundaries, "physio_shp", "physio.shp"))
  
  # Combine boundaries since not all are covered
  # Provinces are only combined with those adjacent to them and in the same division
  province_from <- c("LOWER CALIFORNIAN", "COLUMBIA PLATEAU", "NORTHERN ROCKY MOUNTAINS",
                     "VALLEY AND RIDGE", "ST. LAWRENCE VALLEY", "ADIRONDACK", "NEW ENGLAND")
  province_to <- c("PACIFIC BORDER", "BASIN AND RANGE", "MIDDLE ROCKY MOUNTAINS",
                   "BLUE RIDGE", "APPALACHIAN PLATEAUS", "APPALACHIAN PLATEAUS", 
                   "PIEDMONT")
  physio$PROVINCE <- mapvalues(physio$PROVINCE, province_from, province_to)
  physio <- physio[!is.na(physio$DIVISION),]
  saveRDS(physio, file.path(path_boundaries, "physio.rds"))
}

#-------------------------------------------------------------------------------
# Standardize Sample
# Written by: Jessica Li
#-------------------------------------------------------------------------------
la <- length(pm_paths)
lo <- length(post_paths)

if (!all(file.exists(file.path(path_infiltration, "heterogeneity", 
                               rep(pm_paths, lo), rep(post_paths, la), 
                               "dat_pa_inf_nonstandard.rds")))) {
  
  # Read in PurpleAir data frames
  dat_pas <- vector("list", la*lo)
  foreach(i = 1:la) %:% foreach(j = 1:lo) %do% {
    # N = 1511 (PA, not posterior)
    # N = 1511 (PA, posterior)
    # N = 1505 (EPA, not posterior)
    # N = 1505 (EPA, posterior)
    # N = 1501 (PC, not posterior)
    # N = 1501 (PC, posterior)
    df <- readRDS(file.path(path_infiltration, "heterogeneity", pm_paths[i], post_paths[j], "dat_pa_inf.rds"))
    print(paste(pm_paths[i], post_paths[j], nrow(df)))
    dat_pas[[i + 3*(j - 1)]] <- df
    saveRDS(df, file.path(path_infiltration, "heterogeneity", pm_paths[i], post_paths[j], "dat_pa_inf_nonstandard.rds"))
  }
  
  # Get set of IDs common to all sets
  id_keep <- lapply(dat_pas, select, "id") %>% reduce(inner_join) %>% pull(id)
  
  # Keep only observations with common ID and save
  for (i in 1:length(dat_pas)) dat_pas[[i]] <- dat_pas[[i]] %>% filter(id %in% id_keep)
  foreach(i = 1:la) %:% foreach(j = 1:lo) %do% {
    # N = 1484 (PA/EPA/PC, not posterior/posterior)
    saveRDS(dat_pas[[i + 3*(j - 1)]], file.path(path_infiltration, "heterogeneity", pm_paths[i], post_paths[j], "dat_pa_inf.rds"))
  }
}

#-------------------------------------------------------------------------------
# Match PurpleAir Infiltration Estimates to Characteristics
# Written by: Jessica Li
#-------------------------------------------------------------------------------
if (!file.exists(file.path(path_infiltration, "heterogeneity", pm_path, post_path, "dat_pa_inf_cl_acs_chars_avg.rds"))) {
  
  # Read in PA and ACS data
  dat_pa <- readRDS(file.path(path_infiltration, "heterogeneity", pm_path, post_path, "dat_pa_inf.rds"))
  dat_acs <- readRDS(file.path(path_infiltration, "heterogeneity", "dat_acs_chars.rds"))
  
  #-------------------------------------------------------------------------------
  #### Match PA monitors to Census tracts ####
  # Get PA points
  points_pa <- SpatialPoints(dat_pa[c("lon", "lat")])
  
  # Get Census tract polygons for continental US
  conus <- sort(setdiff(states()$STUSPS, c("AK", "AS", "GU", "HI", "MP", "PR", "VI")))
  if (!file.exists(file.path(path_boundaries, "tracts_conus.rds"))) {
    tracts_conus <- vector("list", length(conus))
    for (i in 1:length(conus)) {
      tracts_conus[[i]] <- tracts(conus[i], class = "sp", year = 2019)
    }
    tracts_conus <- bind(tracts_conus)
    tracts_conus <- SpatialPolygonsDataFrame(SpatialPolygons(tracts_conus@polygons), tracts_conus@data)
    saveRDS(tracts_conus, file.path(path_boundaries, "tracts_conus.rds"))
  } else {
    tracts_conus <- readRDS(file.path(path_boundaries, "tracts_conus.rds"))
  }
  
  # Overlay to match
  matches_acs <- bind_cols(dat_pa["id"], over(points_pa, tracts_conus)) %>%
    select(id, GEOID, ALAND)
  
  # Match to nearest Census tract not missing median_income
  # Affects 2 PA monitors in Pennsylvania located in but near the edge of 2 tracts
  # covering a park and a cemetery; likely lonlat misplaced by a bit and should
  # belong among the surrounding residences
  missing_income_ids <- dat_acs %>% filter(is.na(income_median)) %>% pull(GEOID)
  missing_income_pa <- matches_acs$GEOID %in% missing_income_ids
  missing_income_tracts <- tracts_conus$GEOID %in% missing_income_ids
  nna <- apply(gDistance(points_pa[missing_income_pa, ],
                         tracts_conus[!missing_income_tracts, ],
                         byid = TRUE), 2, which.min)
  matches_acs[missing_income_pa, "GEOID"] <- tracts_conus$GEOID[!missing_income_tracts][nna]
  matches_acs[missing_income_pa, "ALAND"] <- tracts_conus$ALAND[!missing_income_tracts][nna]
  
  # Merge in ACS data by Census tract
  # N = 1484
  matches_acs <- left_join(matches_acs, dat_acs, by = "GEOID") %>%
    mutate(pop_density = pop_total/as.numeric(ALAND)) %>%
    select(-GEOID, -ALAND, -pop_total)
  nrow(matches_acs)
  rm(tracts_conus)
  
  #-------------------------------------------------------------------------------
  #### Match PA monitors to physiographic provinces ####
  # Read in physiographic province data
  physio <- readRDS(file.path(path_boundaries, "physio.rds"))
  physio <- SpatialPolygonsDataFrame(SpatialPolygons(physio@polygons), physio@data)
  
  # Match by overlaying and finding nearest polygon where no overlap found
  # N = 1484
  matches_physio <- bind_cols(dat_pa["id"], over(points_pa, physio)) %>%
    mutate(physio_province = 
             ifelse(is.na(PROVINCE), 
                    PROVINCE[apply(gDistance(points_pa, physio, byid = TRUE), 
                                   2, which.min)], 
                    PROVINCE) %>% 
             factor()) %>% 
    select(id, physio_province)
  nrow(matches_physio)
  
  #-------------------------------------------------------------------------------
  # Read in averaged CL data
  matches_cl_ctf = readRDS(file.path(path_infiltration, "heterogeneity", "CoreLogic", "matches_cl_ctf.rds"))
  matches_cl_nn = readRDS(file.path(path_infiltration, "heterogeneity", "CoreLogic", "matches_cl_nn.rds"))
  
  # Join matches together
  matches_cl_ctf <- left_join(matches_cl_ctf, matches_cl_nn[c("id", "yrbuilt_fac")], by = "id") %>% 
    mutate(cutoff = as.character(cutoff))
  dat_matched <- list(dat_pa, matches_acs, matches_physio, 
                      bind_rows(matches_cl_nn, matches_cl_ctf))
  dat_matched <- Reduce(inner_join, dat_matched) %>% 
    mutate(total_val = total_val/1000,
           income_median = income_median/1000) %>% 
    arrange(id, cutoff)
  
  #-------------------------------------------------------------------------------
  # Standardize sample across cutoffs
  nc <- length(unique(dat_matched$cutoff))
  ids <- dat_matched %>% 
    count(id) %>% 
    filter(n == nc) %>% 
    pull(id)
  
  # N = 1383
  dat_matched <- dat_matched %>% filter(id %in% ids)
  count(dat_matched, cutoff)
  
  #-------------------------------------------------------------------------------
  # Merge in mean outdoor PM2.5 at each indoor monitor
  dat_outdoor <- readRDS(paste0(path_purpleair, "purpleAir_meanOutdoorPM_by_indoorMonitor.rds"))
  dat_merged <- left_join(dat_matched, dat_outdoor, by = c("id" = "ID_in"))
  
  #-------------------------------------------------------------------------------
  # Generate housing and livelihood indices
  housing <- dat_merged %>% 
    group_by(cutoff) %>% 
    select(total_val, stories, baths, bedrooms, height, area) %>% 
    mutate(across(everything(), scale)) %>% 
    ungroup() %>% 
    select(-cutoff)
  housing <- rowMeans(housing)
  livelihood <- dat_merged %>% 
    group_by(cutoff) %>% 
    select(poverty_below, food_stamps_received, occ_renter, income_median) %>% 
    mutate(income_median = -income_median,
           across(everything(), scale)) %>% 
    ungroup() %>% 
    select(-cutoff)
  livelihood <- rowMeans(livelihood)
  
  # Merge into matched data
  dat_merged <- dat_merged %>% 
    mutate(housing_index = housing,
           livelihood_index = livelihood)
  
  #-------------------------------------------------------------------------------
  # Save matched data
  saveRDS(dat_merged, file.path(path_infiltration, "heterogeneity", pm_path, post_path, "dat_pa_inf_cl_acs_chars_avg.rds"))
}

#-------------------------------------------------------------------------------
# Split Data into Train/Test
# Written by: Jessica Li
#-------------------------------------------------------------------------------
# Read in matched data
dat_matched <- readRDS(file.path(path_infiltration, "heterogeneity", pm_path, post_path, "dat_pa_inf_cl_acs_chars_avg.rds"))

# Split train/test
set.seed(5377)
dat_test <- dat_matched %>% 
  group_by(cutoff, physio_province) %>% 
  slice_sample(prop = 0.25) %>% 
  ungroup()
dat_train <- dat_matched %>% setdiff(dat_test)

# Save data
saveRDS(dat_train, file.path(path_infiltration, "heterogeneity", pm_path, post_path, "dat_train.rds"))
saveRDS(dat_test, file.path(path_infiltration, "heterogeneity", pm_path, post_path, "dat_test.rds"))
