path_fire_processed = file.path(path_dropbox, "fire", "processed")

# Script to generate exact statistics they want us to report for Figs 2 and 3:  df, exact pval, beta, and CI

options("modelsummary_format_numeric_latex" = "plain")

# load main datasets
dt <- read_rds(file.path(path_dropbox, 'panel_county_pm_smoke_day.RDS'))
dt$fips <- as.character(dt$county)
dt <- dt %>% mutate(month=month(date), year=year(date), fipsmonth=paste(fips,month,sep="_")) %>%
  arrange(fips,date)


# linear version of relationships in Fig 2. 
# sentiment
twit <- read_fst(file.path(path_twitter, "county-sentiment.fst"))
twit$date <- as.Date(twit$date)
dt <- left_join(dt,twit,by=c("fips","date"))

dt$smokePMscale = dt$smokePM/1000  #rescaling so coefficients are legible. remember to note in table
mod <- feols(sent ~ smokePMscale  | fipsmonth + date, data=dt, weights = dt$population)
ct <- coeftable(mod)
df <- degrees_freedom(mod,type="t")  #d.o.f in t-test, which is smaller than residual dof due to clustering of s.e. in fixest call
ci <- confint(mod)
print(paste("t-test(df=",df,")=",round(ct[3],3),", p=",ct[4],", eff. size =",round(ct[1],3),", CI:",as.numeric(round(confint(mod)[1],3)),",",as.numeric(round(confint(mod)[2],3)),")",sep=""))

# mobility
safe <- read_rds(file.path(path_safegraph, 'safegraph_completely_home_ALL.rds'))
safe <- safe %>% rename(date=Date,fips=County_FIPS)
safe <- left_join(safe,dt,by=c("fips","date"))
mod <- feols(completely_home_device_perc ~ smokePM | fipsmonth + date, data=safe, weights=safe$population)
ct <- coeftable(mod)
df <- degrees_freedom(mod,type="t")  #d.o.f in t-test, which is smaller than residual dof due to clustering of s.e. in fixest call
ci <- confint(mod)
print(paste("t-test(df=",df,")=",round(ct[3],3),", p=",ct[4],", eff. size =",round(ct[1],3),", CI:",as.numeric(round(confint(mod)[1],3)),",",as.numeric(round(confint(mod)[2],3)),")",sep=""))


# google trends
goog <- read_rds(file.path(path_gtrends, "google_trends_smoke_DMA_normalized_with_covariates.RDS"))
goog <- goog %>% mutate(month=month(date), year=year(date)) %>% mutate(dmamonth = paste(dma,month,sep="_"), dmamonthyear=paste(dma,year,month,sep="_"))
goog_panel <- read_rds(file.path(path_dropbox, 'panel_dma_pm_smoke_day_weekly.RDS'))
avgsmokepm <- goog_panel %>% mutate(year=year(week)) %>% filter(year<2016) %>% 
  group_by(dma) %>% summarise(avgsmokepm=mean(smokePM,na.rm=T), avgpm = mean(pm25,na.rm=T))
goog <- left_join(goog,avgsmokepm)
gf <- read_rds(file.path(path_fire_processed, 'dma_weekly_dist_to_fire_cluster.RDS'))
gf <- gf %>% rename(date=week)
goog <- left_join(goog,gf)

df <- filter(goog,keyword=="air quality")
mod <- feols(hits ~ smokePM | dmamonth + date, weights=df$population, data=df)
ct <- coeftable(mod)
df <- degrees_freedom(mod,type="t")  #d.o.f in t-test, which is smaller than residual dof due to clustering of s.e. in fixest call
ci <- confint(mod)
print(paste("t-test(df=",df,")=",round(ct[3],3),", p=",ct[4],", eff. size =",round(ct[1],3),", CI:",as.numeric(round(confint(mod)[1],3)),",",as.numeric(round(confint(mod)[2],3)),")",sep=""))


df <- filter(goog,keyword=="air filter")
mod <- feols(hits ~ smokePM | dmamonth + date, weights=df$population, data=df)
ct <- coeftable(mod)
df <- degrees_freedom(mod,type="t")  #d.o.f in t-test, which is smaller than residual dof due to clustering of s.e. in fixest call
ci <- confint(mod)
print(paste("t-test(df=",df,")=",round(ct[3],3),", p=",ct[4],", eff. size =",round(ct[1],3),", CI:",as.numeric(round(confint(mod)[1],3)),",",as.numeric(round(confint(mod)[2],3)),")",sep=""))


# heterogeneity by income
dt$smokevar = dt$smokePM/1000  #rescaling so coefficients are legible in sentiment regressions. remember to note in table
dt$median_income = dt$median_household_income/1000  #rescale as well
safe$median_income = safe$median_household_income/1000
safe$smokevar = safe$smokePM  #renaming so table works out
goog$median_income <- goog$median_household_income/1000
goog$smokevar = goog$smokePM

# recenter moderators at their mean
recenter <- function(x) {x = x - mean(x,na.rm=T)}
dt <- dt %>% mutate(median_income = recenter(median_income))
safe <- safe %>% mutate(median_income = recenter(median_income))
goog <- goog %>% mutate(amedian_income = recenter(median_income))

df <- filter(goog,keyword=="air quality")
mod <-feols(hits ~ smokevar*median_income | dmamonth + date, weights=df$population, data=df)
ct <- coeftable(mod)
ci <- confint(mod)
print(paste("p=",ct[2,4],", effect size on linear interaction =",round(ct[2,1],3),", CI:",as.numeric(round(confint(mod)[2,1],3)),",",as.numeric(round(confint(mod)[2,2],3)),sep=""))

df <- filter(goog,keyword=="air filter")
mod <-feols(hits ~ smokevar*median_income | dmamonth + date, weights=df$population, data=df)
ct <- coeftable(mod)
ci <- confint(mod)
print(paste("p=",ct[2,4],", effect size on linear interaction =",round(ct[2,1],3),", CI:",as.numeric(round(confint(mod)[2,1],3)),",",as.numeric(round(confint(mod)[2,2],3)),sep=""))

#sentiment
mod <- feols(sent ~ smokevar*median_income  | fipsmonth + date, data=dt, weights = dt$population,lean = T)
ct <- coeftable(mod)
ci <- confint(mod)
print(paste("p=",ct[2,4],", effect size on linear interaction =",round(ct[2,1],3),", CI:",as.numeric(round(confint(mod)[2,1],3)),",",as.numeric(round(confint(mod)[2,2],3)),sep=""))

#mobility
mod  <- feols(completely_home_device_perc ~ smokevar*median_income | fipsmonth + date, data=safe, weights=safe$population,lean = T)
ct <- coeftable(mod)
ci <- confint(mod)
print(paste("p=",ct[2,4],", effect size on linear interaction =",round(ct[2,1],3),", CI:",as.numeric(round(confint(mod)[2,1],3)),",",as.numeric(round(confint(mod)[2,2],3)),sep=""))


