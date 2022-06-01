#-------------------------------------------------------------------------------
# Compare PurpleAir and EPA PM2.5
# Written by: Sam Heft-Neal
# Last edited by: Jessica Li
#-------------------------------------------------------------------------------
# Set maximum distance (m) for matching
within_m <- 1000

# Set minimum number of observations a monitor should have
min_obs <- 24 * 30

# Comes from matching PurpleAir and EPA PM2.5 data
dat_matched <- readRDS(file.path(path_dropbox, "dat_pa_epa_pm25_nn.rds"))

# Now we want to use update (ie corrected) outdoor PA PM in this comparison so 
# take this data and join with corrected hourly PA
pa_corrected <- readRDS(file.path(path_dropbox, "outdoor_monitor_data_clean_part1.rds"))

pa_corrected <- pa_corrected[pa_corrected$ID_out %in% dat_matched$id_pa,]
pa_corrected<-pa_corrected %>% rename(id_pa = ID_out, date_time = time_hours)
pa_corrected<-pa_corrected %>% rename(pm25_corrected_out = pm25_out)
pa_corrected <- pa_corrected[,c("id_pa","date_time","pm25_uncorrected_out","pm25_corrected_out")]

dat_matched2 <- left_join(dat_matched, pa_corrected)


pa_corrected1 <- readRDS(file.path(path_dropbox, "outdoor_monitor_data_clean.rds"))
pa_corrected1 <- pa_corrected1[pa_corrected1$ID_out %in% dat_matched$id_pa,]
pa_corrected1<-pa_corrected1 %>% rename(id_pa = ID_out, date_time = time_hours)
pa_corrected1 <- pa_corrected1[,c("id_pa","date_time","pm25_out_pc")]

dat_matched2 <- left_join(dat_matched2, pa_corrected1)

rm(pa_corrected, pa_corrected1)
gc()

# Discard monitors with too few observations
dat_matched <- dat_matched2 %>% 
  filter(dist_m <= within_m) %>%
  group_by(id_pa) %>%
  filter(n() >= min_obs) %>%
  ungroup() %>%
  group_by(scs_id_epa) %>%
  filter(n() >= min_obs) %>%
  ungroup()

rm(dat_matched2)

# Define month of sample
dat_matched <- dat_matched %>%  mutate(month_sample = format(date_time, "%Y-%m"))

# Fit regressions
md1 <- feols(pm25_epa ~ pm25_uncorrected_out | 
               (id_pa) + (scs_id_epa) + 
               (month_sample) + (hour), 
             data = dat_matched)

md2 <- feols(pm25_epa ~ pm25_corrected_out | 
               (id_pa) + (scs_id_epa) + 
               (month_sample) + (hour), 
             data = dat_matched)

md3 <- feols(pm25_epa ~ pm25_out_pc | 
               (id_pa) + (scs_id_epa) + 
               (month_sample) + (hour), 
             data = dat_matched)

# Regression results
summary(md1)
summary(md2)
summary(md3)

r2(md1)
r2(md2)
r2(md3)

# Save
etable(md1, md2, md3, 
       digits = "r3",
       digits.stats = 2,
       fitstat = c("r2", "n"),
       dict = c(pm25_uncorrected_out = "Purple Air PM$_{2.5}$",
                pm25_corrected_out = "Purple Air PM$_{2.5}$",
                pm25_out_pc = "Purple Air PM$_{2.5}$",
                scs_id_epa = "EPA monitor",
                id_pa = "Purple Air monitor",
                hour = "hour-of-day",
                month_sample = "month-of-sample"),
       file = file.path(path_github, "tables/S12.tex"),
       style.tex = style.tex(depvar.title = "",
                             model.title = "\\textbf{Purple Air PM2.5 Construction:}",
                             line.top = "",
                             line.bottom = "\\bottomrule",
                             var.title = "",
                             fixef.title = "",
                             fixef.prefix = "FE: ",
                             fixef.where = "stats",
                             stats.title = "",
                             tablefoot = F),
       depvar = F)
