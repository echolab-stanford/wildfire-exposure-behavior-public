num_cores <- 4

#-------------------------------------------------------------------------------
# Match PurpleAir Infiltration Estimates to ACS Characteristics
# Written by: Jessica Li
# Last edited by: Jessica Li
#-------------------------------------------------------------------------------
# Read in PA and ACS data
dat_pa <- readRDS(file.path(path_infiltration, pm_path, post_path, "dat_pa_inf.rds"))
dat_acs <- readRDS(file.path(path_dropbox, "dat_acs_chars.rds"))

#-------------------------------------------------------------------------------
#### Match PA monitors to Census tracts ####
# Get PA points
points_pa <- SpatialPoints(dat_pa[c("lon", "lat")])

# Get Census tract polygons for continental US
conus <- sort(setdiff(states()$STUSPS, c("AK", "AS", "GU", "HI", "MP", "PR", "VI")))
tracts_conus <- vector("list", length(conus))
for (i in 1:length(conus)) tracts_conus[[i]] <- tracts(conus[i], class = "sp", year = 2019)
tracts_conus <- bind(tracts_conus)
tracts_conus <- SpatialPolygonsDataFrame(SpatialPolygons(tracts_conus@polygons), tracts_conus@data)
# saveRDS(tracts_conus, "~/Desktop/tracts_conus.rds")
# tracts_conus <- readRDS("~/Desktop/tracts_conus.rds")

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
physio <- readRDS(file.path(path_dropbox, "physio.rds"))
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
# Join matches together
dat_matched <- list(dat_pa, matches_acs, matches_physio)
dat_matched <- Reduce(inner_join, dat_matched) %>% 
  mutate(total_val = total_val/1000,
         income_median = income_median/1000) %>% 
  arrange(id)

#-------------------------------------------------------------------------------
# Merge in mean outdoor PM2.5 at each indoor monitor
dat_outdoor <- readRDS(file.path(path_dropbox, "purpleAir_meanOutdoorPM_by_indoorMonitor.rds"))
dat_merged <- left_join(dat_matched, dat_outdoor, by = c("id" = "ID_in"))

#-------------------------------------------------------------------------------
# Save matched data
saveRDS(dat_merged, file.path(path_infiltration, pm_path, post_path, "dat_pa_inf_acs_chars.rds"))
