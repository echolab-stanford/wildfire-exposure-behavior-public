#-------------------------------------------------------------------------------
# Robustness Tables S1-S9
# Written by: Marshall Burke
#-------------------------------------------------------------------------------
options("modelsummary_format_numeric_latex" = "plain")

# Load main datasets and merge with distance to fire
dt <- read_rds(file.path(path_dropbox, "panel_county_pm_smoke_day.RDS"))
dt$fips <- as.character(dt$county)
dt <- dt %>% 
  mutate(wday = wday(date), 
         month=month(date), 
         year=year(date), 
         fipsmonth=paste(fips,month,sep="_"), 
         fipsmonthyear=paste(fips,year,month,sep="_"),
         dayofmonth=day(date), 
         state=substr(county,1,2)) %>%
  arrange(fips,date)
df <- read_fst(file.path(path_dropbox, "fire", "processed", "county_pop_weighted_dist_to_fire_2006_2020.fst"))
dt <- left_join(dt,df)

# Avg smoke PM exposure pre 2016
avgsmokepm <- dt %>% 
  filter(year<2016) %>% 
  group_by(fips) %>% 
  summarise(avgsmokepm=mean(smokePM,na.rm=T), 
            avgpm = mean(pm25,na.rm=T))
dt <- left_join(dt,avgsmokepm)

# Region mapping
regions <- read_rds(file.path(path_dropbox, 'stateFIPS_epaREGION_crosswalk.rds')) %>% 
  select(`State Code`,`EPA Region`) %>% 
  rename(state=`State Code`, epa_region=`EPA Region`) %>%
  distinct()
dt <- left_join(dt,regions)
safe <- read_rds(file.path(path_safegraph, 'safegraph_completely_home_ALL.rds'))
safe <- safe %>% rename(date=Date,fips=County_FIPS)
safe <- left_join(safe,dt,by=c("fips","date"))
safe_dt <- as.data.table(safe, ~ fips+date)
lgs=1:7
anscols = paste0("smokePM_lag",lgs)
safe_dt[, (anscols) := shift(.SD, 1:7, fill=NA, "lag"), .SDcols="smokePM"]
safe_dt <- as.data.frame(safe_dt)
safe_dt <- safe_dt %>% mutate(smokePM_lastweek = (smokePM + smokePM_lag1 + smokePM_lag2 + smokePM_lag3 + smokePM_lag4 + smokePM_lag5 + smokePM_lag6 + smokePM_lag7)/8)

# Twitter
twit <- read_fst(file.path(path_twitter, "county-sentiment.fst"))
twit$date <- as.Date(twit$date)
dt <- left_join(dt,twit,by=c("fips","date"))

# Google Trends
goog <- read_rds(file.path(path_gtrends, "google_trends_smoke_DMA_normalized_with_covariates.RDS"))
goog <- goog %>% 
  mutate(month=month(date), 
         year=year(date)) %>% 
  mutate(dmamonth = paste(dma,month,sep="_"), 
         dmamonthyear=paste(dma,year,month,sep="_"))
goog_panel <- read_rds(file.path(path_dropbox, 'panel_dma_pm_smoke_day_weekly.RDS'))
avgsmokepm <- goog_panel %>% 
  mutate(year=year(week)) %>% 
  filter(year<2016) %>% 
  group_by(dma) %>% 
  summarise(avgsmokepm=mean(smokePM,na.rm=T), 
            avgpm = mean(pm25,na.rm=T))
goog <- left_join(goog,avgsmokepm)
gf <- read_rds(file.path(path_dropbox, "fire", "processed", "dma_weekly_dist_to_fire_cluster.RDS"))
gf <- gf %>% rename(date=week)
goog <- left_join(goog,gf)

#-------------------------------------------------------------------------------
# Table S5
dt$smokePMscale = dt$smokePM/1000  # Rescaling so coefficients are legible. remember to note in table
df1 <- filter(dt,km_dist>50)
df2 <- filter(dt,km_dist<=50)
models <- list()
models[["baseline"]] <- feols(sent ~ smokePMscale  | fipsmonth + date, data=dt, weights = dt$population)
models[["temp/prec"]] <- feols(sent ~ smokePMscale + temperature + precipitation | fipsmonth + date, data=dt, weights = dt$population)
models[["distance > 50"]] <- feols(sent ~ smokePMscale | fipsmonth + date, data=df1, weights = df1$population)
models[["distance < 50"]] <- feols(sent ~ smokePMscale | fipsmonth + date, data=df2, weights = df2$population)
models[["FE1"]] <- feols(sent ~ smokePMscale | fipsmonth + date^state, data=dt, weights = dt$population)
models[["FE2"]] <- feols(sent ~ smokePMscale | fipsmonthyear + date, data=dt, weights = dt$population)
modelsummary(models,stars=F, statistic=c("({std.error})","[{p.value}]"),
             fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC',output = file.path(path_github, 'tables/raw/tableS05.tex'))

# Table S7
df1 <- filter(safe_dt,km_dist>50)
df2 <- filter(safe_dt,km_dist<=50)
models <- list()
models[["baseline"]] <- feols(completely_home_device_perc ~ smokePM | fipsmonth + date, data=safe_dt, weights=safe_dt$population)
models[["temp/prec"]] <- feols(completely_home_device_perc ~ smokePM + temperature + precipitation | fipsmonth + date, data=safe_dt, weights=safe_dt$population)
models[["distance > 50"]] <- feols(completely_home_device_perc ~ smokePM | fipsmonth + date, data=df1, weights=df1$population)
models[["distance < 50"]] <- feols(completely_home_device_perc ~ smokePM | fipsmonth + date, data=df2, weights=df2$population)
models[["FE1"]] <- feols(completely_home_device_perc ~ smokePM | fipsmonth + date^State_FIPS, data=safe_dt, weights=safe_dt$population)
models[["FE2"]] <- feols(completely_home_device_perc ~ smokePM | fipsmonthyear + date, data=safe_dt, weights=safe_dt$population)
models[["smokeweek"]] <- feols(completely_home_device_perc ~ smokePM_lastweek | fipsmonth + date, data=safe_dt, weights=safe_dt$population)
models[["away"]] <- feols(completely_away_perc ~ smokePM | fipsmonth + date, data=safe_dt, weights=safe_dt$population)
models[["away_week"]] <- feols(completely_away_perc ~ smokePM_lastweek | fipsmonth + date, data=safe_dt, weights=safe_dt$population)
models[["away_FE1"]] <- feols(completely_away_perc ~ smokePM | fipsmonth + date^State_FIPS, data=safe_dt, weights=safe_dt$population)
models[["away_FE2"]] <- feols(completely_away_perc ~ smokePM | fipsmonthyear + date, data=safe_dt, weights=safe_dt$population)
modelsummary(models,stars=F, statistic=c("({std.error})","[{p.value}]"),
             fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC',output = file.path(path_github, 'tables/raw/tableS07.tex'))

# Table S2
df <- filter(goog,keyword=="air quality")
df1 <- filter(df,distance>50)
df2 <- filter(df,distance<=50)
models <- list()
models[["baseline"]] <- feols(hits ~ smokePM | dmamonth + date, weights=df$population, data=df)
models[["temp/prec"]] <- feols(hits ~ smokePM + temperature + precipitation | dmamonth + date, weights=df$population, data=df)
models[["distance > 50"]] <- feols(hits ~ smokePM | dmamonth + date, weights=df1$population, data=df1)
models[["distance < 50"]] <- feols(hits ~ smokePM | dmamonth + date, weights=df2$population, data=df2)
models[["FE"]] <- feols(hits ~ smokePM | dmamonthyear + date, weights=df$population, data=df)

df <- filter(goog,keyword=="air filter")
df1 <- filter(df,distance>50)
df2 <- filter(df,distance<=50)
models[["AF_baseline"]] <- feols(hits ~ smokePM | dmamonth + date, weights=df$population, data=df)
models[["AF_temp/prec"]] <- feols(hits ~ smokePM + temperature + precipitation | dmamonth + date, weights=df$population, data=df)
models[["AF_distance > 50"]] <- feols(hits ~ smokePM | dmamonth + date, weights=df1$population, data=df1)
models[["AF_distance < 50"]] <- feols(hits ~ smokePM | dmamonth + date, weights=df2$population, data=df2)
models[["AF_FE"]] <- feols(hits ~ smokePM | dmamonthyear + date, weights=df$population, data=df)
modelsummary(models,stars=F, statistic=c("({std.error})","[{p.value}]"),
             fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC',output = file.path(path_github, 'tables/raw/tableS02.tex'))

# Table S1
models <- list()
ys <- c("air quality","smoke", "air pollution","calidad del aire","humo")
for (y in ys) {
  df <- filter(goog,keyword==y)
  models[[y]] <- feols(hits ~ smokePM | dmamonth + date, weights=df$population, data=df)
}
modelsummary(models,stars=F, statistic=c("({std.error})","[{p.value}]"),
             fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC',output = file.path(path_github, 'tables/raw/tableS01.tex'))

# Table S6
models <- list()
ys <- c("air filter","air purifier","purple air","smoke mask","purificador de aire","filtro de aire")
for (y in ys) {
  df <- filter(goog,keyword==y)
  models[[y]] <- feols(hits ~ smokePM | dmamonth + date, weights=df$population, data=df)
}
options("modelsummary_format_numeric_latex" = "plain")
modelsummary(models,stars=F, statistic=c("({std.error})","[{p.value}]"),
             fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC',output = file.path(path_github, 'tables/raw/tableS06.tex'))

# Table S4
models <- list()
ys <- c("floods","hurricanes","dinosaurs","USWNT","steph curry")
for (y in ys) {
  df <- filter(goog,keyword==y)
  models[[y]] <- feols(hits ~ smokePM | dmamonth + date, weights=df$population, data=df)
  models[[paste0(y,"_alt")]] <- feols(hits ~ smokePM | dmamonthyear + date, weights=df$population, data=df)
}
goog_pm <- goog %>% filter(smokePM==0 & keyword=="smoke")
models[["smoke"]] <- feols(hits ~ pm25 | dmamonth + date, weights=df$population, data=df)
options("modelsummary_format_numeric_latex" = "plain")
modelsummary(models,stars=F, statistic=c("({std.error})","[{p.value}]"),
             fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC',output = file.path(path_github, 'tables/raw/tableS04.tex'))

# Table S3
models <- list()
ys <- c("air quality","air pollution","smoke")
for (y in ys) {
  df <- filter(goog,keyword==y)
  models[[y]] <- feols(hits ~ smokePM*distance | dmamonth + date, weights=df$population, data=df)
}
options("modelsummary_format_numeric_latex" = "plain")
modelsummary(models,stars=F, statistic=c("({std.error})","[{p.value}]"),
             fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC',output = file.path(path_github, 'tables/raw/tableS03.tex'))

#-------------------------------------------------------------------------------
# Heterogenetiy analyses
dt$smokevar = dt$smokePM/1000  #rescaling so coefficients are legible in sentiment regressions. 
dt$median_income = dt$median_household_income/1000  #rescale income as well
safe_dt$median_income = safe_dt$median_household_income/1000
safe_dt$smokevar = safe_dt$smokePM  #renaming so table works out
goog$median_income <- goog$median_household_income/1000
goog$smokevar = goog$smokePM

# Recenter moderators at their mean
recenter <- function(x) {x = x - mean(x,na.rm=T)}
dt <- dt %>% mutate(across(c(median_income, avgpm, avgsmokepm,km_dist), recenter))
safe_dt <- safe_dt %>% mutate(across(c(median_income, avgpm, avgsmokepm,km_dist), recenter))
goog <- goog %>% mutate(across(c(median_income, avgpm, avgsmokepm,distance), recenter))

# Table S8
hetmodels <- list()
df <- filter(goog,keyword=="air quality")
hetmodels[["sal_inc"]] <-feols(hits ~ smokevar*median_income | dmamonth + date, weights=df$population, data=df)
hetmodels[["sal_avgsmoke"]] <- feols(hits ~ smokevar*avgsmokepm | dmamonth + date, weights=df$population, data=df)
hetmodels[["sal_avgpm"]] <- feols(hits ~ smokevar*avgpm | dmamonth + date, weights=df$population, data=df)
hetmodels[["sal_all"]] <- feols(hits ~ smokevar*(median_income + avgsmokepm + avgpm) | dmamonth + date, weights=df$population, data=df)

df <- filter(goog,keyword=="air purifier")
hetmodels[["prot_inc"]] <-feols(hits ~ smokevar*median_income | dmamonth + date, weights=df$population, data=df)
hetmodels[["prot_avgsmoke"]] <- feols(hits ~ smokevar*avgsmokepm | dmamonth + date, weights=df$population, data=df)
hetmodels[["prot_avgpm"]] <- feols(hits ~ smokevar*avgpm | dmamonth + date, weights=df$population, data=df)
hetmodels[["prot_all"]] <- feols(hits ~ smokevar*(median_income + avgsmokepm + avgpm) | dmamonth + date, weights=df$population, data=df)
modelsummary(hetmodels,stars=F, statistic=c("({std.error})","[{p.value}]"),
             fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC',coef_omit = "^(?!.*smokevar)",output = file.path(path_github, 'tables/raw/tableS08.tex'))

# Table S9
hetmodels <- list()
hetmodels[["sent_inc"]] <- feols(sent ~ smokevar*median_income  | fipsmonth + date, data=dt, weights = dt$population,lean = T)
hetmodels[["sent_avgsmoke"]] <- feols(sent ~ smokevar*avgsmokepm  | fipsmonth + date, data=dt, weights = dt$population,lean = T)
hetmodels[["sent_avgpm"]] <- feols(sent ~ smokevar*avgpm  | fipsmonth + date, data=dt, weights = dt$population,lean = T)
hetmodels[["sent_all"]] <- feols(sent ~ smokevar*(median_income + avgsmokepm + avgpm) | fipsmonth + date, data=dt, weights = dt$population,lean = T)

hetmodels[["mob_inc"]] <- feols(completely_home_device_perc ~ smokevar*median_income | fipsmonth + date, data=safe_dt, weights=safe_dt$population,lean = T)
hetmodels[["mob_avgsmoke"]] <- feols(completely_home_device_perc ~ smokevar*avgsmokepm | fipsmonth + date, data=safe_dt, weights=safe_dt$population,lean = T)
hetmodels[["mob_avgpm"]] <- feols(completely_home_device_perc ~ smokevar*avgpm | fipsmonth + date, data=safe_dt, weights=safe_dt$population,lean = T)
hetmodels[["mob_all"]] <- feols(completely_home_device_perc ~ smokevar*(median_income + avgsmokepm + avgpm) | fipsmonth + date, data=safe_dt, weights=safe_dt$population,lean = T)

modelsummary(hetmodels,stars=F, statistic=c("({std.error})","[{p.value}]"),
             fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC',coef_omit = "^(?!.*smokevar)",output = file.path(path_github, 'tables/raw/tableS09.tex'))
