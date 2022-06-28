#-------------------------------------------------------------------------------
# Main Regressions for Figures 2 and 3
# Written by: Marshall Burke
#-------------------------------------------------------------------------------
options("modelsummary_format_numeric_latex" = "plain")

# Function to do bootstrap on spline
bootspline <- function(data,
                       boot=100,
                       xx=0:150,
                       seed=1,
                       xvar="smokePM",
                       yvar="hits",
                       fe="fipsmonth + date + wday",
                       xunit="fips",
                       kts=c(10,25,50),
                       controls="+ temperature + precipitation") {
  cc <- unique(data[[xunit]])
  nn=length(kts)+1
  set.seed(seed)
  out <- c()
  data$xunit <- data[[xunit]]
  fmla <- as.formula(paste(yvar,"~","ns(",xvar,",knots=c(",
                           paste(as.character(kts),collapse = ","),")) ",
                           controls," |",fe))
  for (i in 1:boot) {
    samp <- data.frame(xunit=sample(cc,length(cc),replace=T))  
    subdata <- inner_join(data,samp,by="xunit")
    # splines won't work if top knot is larger than max of RHS, which 
    # occasionally happens in small samples
    if (max(kts) < max(subdata$smokePM)) {
      cf <- coef(feols(fmla, data=subdata,weights = subdata$population))
      yy = as.numeric(t(as.matrix(cf[1:nn]))%*%t(matrix(nrow=length(xx),ncol=nn,
                                                        data=ns(xx,knots=kts))))
      out <- cbind(out,yy)}
    if (round(i,-1)==i) {print(i)}  # Print every 10
  }
  toplot <- data.frame(xx,out)
  return(toplot)
}

# Function to do bootstrap income interaction
bootinteract <- function(data,
                         boot=100,
                         xx=0:150,
                         seed=1,
                         yvar="hits",
                         xvar="smokePM",
                         fe="dmamonth + date",
                         xunit="dma",
                         controls="+ temperature + precipitation",
                         intervar="median_household_income") {
  cc <- unique(data[[xunit]])
  set.seed(seed)
  out <- c()
  data$xunit <- data[[xunit]]
  for (i in 1:boot) {
    samp <- data.frame(xunit=sample(cc,length(cc),replace=T))  
    subdata <- inner_join(data,samp,by="xunit")
    fmla <- as.formula(paste(yvar,"~",xvar,"*",intervar ,controls," |",fe))
    cf <- coef(feols(fmla, data=subdata,weights = subdata$population))
    cf <- c(cf[xvar],cf[paste0(xvar,":",intervar)])
    yy= cf[1] + cf[2]*xx
    out <- cbind(out,yy)
    if (round(i,-1)==i) {print(i)}  # Print every 10
  }
  toplot <- data.frame(xx,out)
  return(toplot)
}

#-------------------------------------------------------------------------------
# Load main datasets and merge with distance to fire
dt <- read_rds(file.path(path_smokePM, 'panel_county_pm_smoke_day.RDS'))
dt$fips <- as.character(dt$county)
dt <- dt %>% 
  mutate(wday = wday(date), 
         month=month(date), 
         year=year(date), 
         fipsmonth=paste(fips,month,sep="_"), 
         dayofmonth=day(date), 
         state=substr(county,1,2)) %>%
  arrange(fips,date)

# SafeGraph
safe <- read_rds(file.path(path_safegraph, 'safegraph_completely_home_ALL.rds'))
safe <- safe %>% rename(date=Date,fips=County_FIPS)
safe <- left_join(safe,dt,by=c("fips","date"))

safe_dt <- as.data.table(safe, ~ fips+date)
lgs=1:7
anscols = paste0("smokePM_lag",lgs)
safe_dt[, (anscols) := shift(.SD, 1:7, fill=NA, "lag"), .SDcols="smokePM"]
safe_dt <- as.data.frame(safe_dt)
safe_dt <- safe_dt %>% 
  mutate(smokePM_lastweek = (smokePM + smokePM_lag1 + smokePM_lag2 + 
                               smokePM_lag3 + smokePM_lag4 + smokePM_lag5 + 
                               smokePM_lag6 + smokePM_lag7)/8)
# Twitter
twit <- read_fst(file.path(path_twitter, "county-sentiment.fst"))
twit$date <- as.Date(twit$date)
dt <- left_join(dt,twit,by=c("fips","date"))

# Google Trends
goog <- read_rds(file.path(path_gtrends, "google_trends_smoke_DMA_normalized_with_covariates.RDS"))
goog <- goog %>% 
  mutate(month=month(date), 
         year=year(date)) %>% 
  mutate(dmamonth = paste(dma,month,sep="_"))
goog_panel <- read_rds(file.path(path_smokePM, 'panel_dma_pm_smoke_day_weekly.RDS'))
avgsmokepm <- goog_panel %>% 
  mutate(year=year(week)) %>% 
  filter(year<2016) %>% 
  group_by(dma) %>% 
  summarise(avgsmokepm=mean(smokePM,na.rm=T), avgpm = mean(pm25,na.rm=T))
goog <- left_join(goog,avgsmokepm)

#-------------------------------------------------------------------------------
# Sentiment bootstraps - somewhat slow, maybe 1-2 hours
dts <- dt %>% 
  drop_na(c(sent,smokePM)) %>% 
  select(sent,smokePM,fipsmonth,date, 
         fips, population, median_household_income)  #slim down df

if (!file.exists(file.path(path_beh_bootstraps, "bootstraps_twitter_spline.csv"))) {
  toplot <- bootspline(dts,
                       yvar="sent",
                       xvar="smokePM",
                       xunit="fips",
                       controls="",
                       boot=1000,
                       fe="fipsmonth + date")
  write.csv(toplot,file=file.path(path_beh_bootstraps, "bootstraps_twitter_spline.csv"),row.names = F)
}

if (!file.exists(file.path(path_beh_bootstraps, "bootstraps_twitter_incomeinteract.csv"))) {
  toplot <- bootinteract(dts,
                         yvar="sent",
                         xvar="smokePM",
                         xunit="fips",
                         controls="",
                         boot=1000,
                         fe="fipsmonth + date",
                         intervar = "median_household_income",
                         xx=seq(20000,140000,5000))
  write.csv(toplot,file=file.path(path_beh_bootstraps, "bootstraps_twitter_incomeinteract.csv"),row.names = F)
}


# Mobility bootstraps
dts <- safe_dt %>% 
  drop_na(smokePM_lastweek,
          completely_home_device_perc,
          completely_away_perc) %>%
  select(smokePM_lastweek,smokePM,
         completely_home_device_perc,completely_away_perc, 
         fipsmonth,date,population,fips,median_household_income)

# Smoke day
if (!file.exists(file.path(path_beh_bootstraps, "bootstraps_safegraph_home_spline_simpleFE.csv"))) {
  toplot <- bootspline(dts,
                       yvar="completely_home_device_perc",
                       xvar="smokePM",
                       xunit="fips",
                       controls="",
                       boot=1000,
                       fe="fipsmonth + date")
  write.csv(toplot,file=file.path(path_beh_bootstraps, "bootstraps_safegraph_home_spline_simpleFE.csv"),row.names = F)
}

if (!file.exists(file.path(path_beh_bootstraps, "bootstraps_safegraph_incomeinteract_home_simpleFE.csv"))) {
  toplot <- bootinteract(dts,
                         yvar="completely_home_device_perc",
                         xvar="smokePM",
                         xunit="fips",
                         controls="",
                         boot=1000,
                         fe="fipsmonth + date",
                         intervar = "median_household_income",
                         xx=seq(20000,140000,5000))
  write.csv(toplot,file=file.path(path_beh_bootstraps, "bootstraps_safegraph_incomeinteract_home_simpleFE.csv"),row.names = F)
}

if (!file.exists(file.path(path_beh_bootstraps, "bootstraps_safegraph_away_spline_simpleFE.csv"))) {
  toplot <- bootspline(dts,
                       yvar="completely_away_perc",
                       xvar="smokePM",
                       xunit="fips",
                       controls="",
                       boot=1000,
                       fe="fipsmonth + date")
  write.csv(toplot,file=file.path(path_beh_bootstraps, "bootstraps_safegraph_away_spline_simpleFE.csv"),row.names = F)
}

if (!file.exists(file.path(path_beh_bootstraps, "bootstraps_safegraph_incomeinteract_away_simpleFE.csv"))) {
  toplot <- bootinteract(dts,
                         yvar="completely_away_perc",
                         xvar="smokePM",
                         xunit="fips",
                         controls="",
                         boot=1000,
                         fe="fipsmonth + date",
                         intervar = "median_household_income",
                         xx=seq(20000,140000,5000))
  write.csv(toplot,file=file.path(path_beh_bootstraps, "bootstraps_safegraph_incomeinteract_away_simpleFE.csv"),row.names = F)
}

# Smoke week
if (!file.exists(file.path(path_beh_bootstraps, "bootstraps_safegraph_home_smokeweek_spline_simpleFE.csv"))) {
  toplot <- bootspline(dts,
                       yvar="completely_home_device_perc",
                       xvar="smokePM_lastweek",
                       xunit="fips",
                       controls="",
                       boot=1000,
                       fe="fipsmonth + date")
  write.csv(toplot,file=file.path(path_beh_bootstraps, "bootstraps_safegraph_home_smokeweek_spline_simpleFE.csv"),row.names = F)
}

if (!file.exists(file.path(path_beh_bootstraps, "bootstraps_safegraph_incomeinteract_home_smokeweek_simpleFE.csv"))) {
  toplot <- bootinteract(dts,
                         yvar="completely_home_device_perc",
                         xvar="smokePM_lastweek",
                         xunit="fips",
                         controls="",
                         boot=1000,
                         fe="fipsmonth + date",
                         intervar = "median_household_income",
                         xx=seq(20000,140000,5000))
  write.csv(toplot,file=file.path(path_beh_bootstraps, "bootstraps_safegraph_incomeinteract_home_smokeweek_simpleFE.csv"),row.names = F)
}

if (!file.exists(file.path(path_beh_bootstraps, "bootstraps_safegraph_away_smokeweek_spline_simpleFE.csv"))) {
  toplot <- bootspline(dts,
                       yvar="completely_away_perc",
                       xvar="smokePM_lastweek",
                       xunit="fips",
                       controls="",
                       boot=1000,
                       fe="fipsmonth + date")
  write.csv(toplot,file=file.path(path_beh_bootstraps, "bootstraps_safegraph_away_smokeweek_spline_simpleFE.csv"),row.names = F)
}

if (!file.exists(file.path(path_beh_bootstraps, "bootstraps_safegraph_incomeinteract_away_smokeweek_simpleFE.csv"))) {
  toplot <- bootinteract(dts,
                         yvar="completely_away_perc",
                         xvar="smokePM_lastweek",
                         xunit="fips",
                         controls="",
                         boot=100,
                         fe="fipsmonth + date",
                         intervar = "median_household_income",
                         xx=seq(20000,140000,5000))
  write.csv(toplot,file=file.path(path_beh_bootstraps, "bootstraps_safegraph_incomeinteract_away_smokeweek_simpleFE.csv"),row.names = F)
}

if (!file.exists(file.path(path_beh_bootstraps, "bootstraps_safegraph_incomeinteract_home_smokeweek.csv"))) {
  toplot <- bootinteract(safe_dt,
                         yvar="completely_home_device_perc",
                         xvar="smokePM_lastweek",
                         xunit="fips",
                         controls="",
                         boot=100,
                         fe="fipsmonth + date^state + wday",
                         intervar = "median_household_income",
                         xx=seq(20000,140000,5000))
  write.csv(toplot,file=file.path(path_beh_bootstraps, "bootstraps_safegraph_incomeinteract_home_smokeweek.csv"),row.names = F)
}

if (!file.exists(file.path(path_beh_bootstraps, "bootstraps_safegraph_incomeinteract_away_smokeweek.csv"))) {
  toplot <- bootinteract(safe_dt,
                         yvar="completely_away_perc",
                         xvar="smokePM_lastweek",
                         xunit="fips",
                         controls="",
                         boot=100,
                         fe="fipsmonth + date^state + wday",
                         intervar = "median_household_income",
                         xx=seq(20000,140000,5000))
  write.csv(toplot,file=file.path(path_beh_bootstraps, "bootstraps_safegraph_incomeinteract_away_smokeweek.csv"),row.names = F)
}

# Google Trends
goog <- goog %>% drop_na(hits,smokePM)
#  vars <- c("air filter","air purifier","air quality","calidad del aire","filtro de aire","purificador de aire","smoke","humo","smoke mask","wildfire","incendio")
vars <- c("air filter","air purifier","air quality")
for (outc in vars) {
  gdt <- goog %>% filter(keyword==outc)
  if (!file.exists(file.path(path_beh_bootstraps, paste0("bootstraps_gtrends_",outc,".csv")))) {
    toplot <- bootspline(gdt,
                         yvar="hits",
                         controls="",
                         boot=1000,
                         fe="dmamonth + date",
                         xunit="dma")
    write.csv(toplot,file=file.path(path_beh_bootstraps, paste0("bootstraps_gtrends_",outc,".csv")),row.names = F)
  }
  
  if (!file.exists(file.path(path_beh_bootstraps, paste0("bootstraps_gtrends_",outc,"_incomeinteract.csv")))) {
    toplot <- bootinteract(gdt,
                           controls="",
                           boot=1000,
                           xx=seq(30000,100000,1000),
                           fe="dmamonth + date",
                           xunit="dma")
    write.csv(toplot,file=file.path(path_beh_bootstraps, paste0("bootstraps_gtrends_",outc,"_incomeinteract.csv")),row.names = F)
  }
  
  print(outc)
}

#-------------------------------------------------------------------------------
# Calculate Exact Statistics for Figures 2 and 3
# Written by: Marshall Burke
# 
# df, exact pval, beta, and CI
#-------------------------------------------------------------------------------
options("modelsummary_format_numeric_latex" = "plain")

# Load main datasets
dt <- read_rds(file.path(path_smokePM, 'panel_county_pm_smoke_day.RDS'))
dt$fips <- as.character(dt$county)
dt <- dt %>% 
  mutate(month=month(date), 
         year=year(date), 
         fipsmonth=paste(fips,month,sep="_")) %>%
  arrange(fips,date)

#-------------------------------------------------------------------------------
#### Linear version of relationships in Fig 2 ####
# Sentiment
twit <- read_fst(file.path(path_twitter, "county-sentiment.fst"))
twit$date <- as.Date(twit$date)
dt <- left_join(dt,twit,by=c("fips","date"))

dt$smokePMscale = dt$smokePM/1000  # Rescaling so coefficients are legible. Remember to note in table
mod <- feols(sent ~ smokePMscale  | fipsmonth + date, data=dt, weights = dt$population)
ct <- coeftable(mod)
df <- degrees_freedom(mod,type="t")  # d.o.f in t-test, which is smaller than residual dof due to clustering of s.e. in fixest call
ci <- confint(mod)
print("Figure 2 statistics for sentiment:")
print(paste("t-test(df=",df,")=",round(ct[3],3),", p=",ct[4],", eff. size =",
            round(ct[1],3),", CI:",as.numeric(round(confint(mod)[1],3)),",",
            as.numeric(round(confint(mod)[2],3)),")",sep=""))

# Mobility
safe <- read_rds(file.path(path_safegraph, 'safegraph_completely_home_ALL.rds'))
safe <- safe %>% rename(date=Date,fips=County_FIPS)
safe <- left_join(safe,dt,by=c("fips","date"))
mod <- feols(completely_home_device_perc ~ smokePM | fipsmonth + date, data=safe, weights=safe$population)
ct <- coeftable(mod)
df <- degrees_freedom(mod,type="t")  # d.o.f in t-test, which is smaller than residual dof due to clustering of s.e. in fixest call
ci <- confint(mod)
print("Figure 2 statistics for mobility:")
print(paste("t-test(df=",df,")=",round(ct[3],3),", p=",ct[4],", eff. size =",
            round(ct[1],3),", CI:",as.numeric(round(confint(mod)[1],3)),",",
            as.numeric(round(confint(mod)[2],3)),")",sep=""))

# Google Trends
goog <- read_rds(file.path(path_gtrends, "google_trends_smoke_DMA_normalized_with_covariates.RDS"))
goog <- goog %>% mutate(month=month(date), year=year(date)) %>% mutate(dmamonth = paste(dma,month,sep="_"), dmamonthyear=paste(dma,year,month,sep="_"))
goog_panel <- read_rds(file.path(path_smokePM, 'panel_dma_pm_smoke_day_weekly.RDS'))
avgsmokepm <- goog_panel %>% mutate(year=year(week)) %>% filter(year<2016) %>% 
  group_by(dma) %>% summarise(avgsmokepm=mean(smokePM,na.rm=T), avgpm = mean(pm25,na.rm=T))
goog <- left_join(goog,avgsmokepm)
gf <- read_rds(file.path(path_fire, "distance_to_fire", 'dma_weekly_dist_to_fire_cluster.RDS'))
gf <- gf %>% rename(date=week)
goog <- left_join(goog,gf)

# Salience
df <- filter(goog,keyword=="air quality")
mod <- feols(hits ~ smokePM | dmamonth + date, weights=df$population, data=df)
ct <- coeftable(mod)
df <- degrees_freedom(mod,type="t")  # d.o.f in t-test, which is smaller than residual dof due to clustering of s.e. in fixest call
ci <- confint(mod)
print("Figure 2 statistics for salience:")
print(paste("t-test(df=",df,")=",round(ct[3],3),", p=",ct[4],", eff. size =",
            round(ct[1],3),", CI:",as.numeric(round(confint(mod)[1],3)),",",
            as.numeric(round(confint(mod)[2],3)),")",sep=""))

# Health protection
df <- filter(goog,keyword=="air filter")
mod <- feols(hits ~ smokePM | dmamonth + date, weights=df$population, data=df)
ct <- coeftable(mod)
df <- degrees_freedom(mod,type="t")  # d.o.f in t-test, which is smaller than residual dof due to clustering of s.e. in fixest call
ci <- confint(mod)
print("Figure 2 statistics for health protection:")
print(paste("t-test(df=",df,")=",round(ct[3],3),", p=",ct[4],", eff. size =",
            round(ct[1],3),", CI:",as.numeric(round(confint(mod)[1],3)),",",
            as.numeric(round(confint(mod)[2],3)),")",sep=""))

#-------------------------------------------------------------------------------
#### Heterogeneity by income ####
dt$smokevar = dt$smokePM/1000  # Rescaling so coefficients are legible in sentiment regressions. Remember to note in table
dt$median_income = dt$median_household_income/1000  # Rescale as well
safe$median_income = safe$median_household_income/1000
safe$smokevar = safe$smokePM  # Renaming so table works out
goog$median_income <- goog$median_household_income/1000
goog$smokevar = goog$smokePM

# Recenter moderators at their mean
recenter <- function(x) {x = x - mean(x,na.rm=T)}
dt <- dt %>% mutate(median_income = recenter(median_income))
safe <- safe %>% mutate(median_income = recenter(median_income))
goog <- goog %>% mutate(amedian_income = recenter(median_income))

# Salience
df <- filter(goog,keyword=="air quality")
mod <-feols(hits ~ smokevar*median_income | dmamonth + date, weights=df$population, data=df)
ct <- coeftable(mod)
ci <- confint(mod)
print("Figure 3 statistics for salience:")
print(paste("p=",ct[2,4],", effect size on linear interaction =",
            round(ct[2,1],3),", CI:",as.numeric(round(confint(mod)[2,1],3)),",",
            as.numeric(round(confint(mod)[2,2],3)),sep=""))

# Health protection
df <- filter(goog,keyword=="air filter")
mod <-feols(hits ~ smokevar*median_income | dmamonth + date, weights=df$population, data=df)
ct <- coeftable(mod)
ci <- confint(mod)
print("Figure 3 statistics for health protection:")
print(paste("p=",ct[2,4],", effect size on linear interaction =",
            round(ct[2,1],3),", CI:",as.numeric(round(confint(mod)[2,1],3)),",",
            as.numeric(round(confint(mod)[2,2],3)),sep=""))

# Sentiment
mod <- feols(sent ~ smokevar*median_income  | fipsmonth + date, data=dt, weights = dt$population,lean = T)
ct <- coeftable(mod)
ci <- confint(mod)
print("Figure 3 statistics for sentiment:")
print(paste("p=",ct[2,4],", effect size on linear interaction =",
            round(ct[2,1],3),", CI:",as.numeric(round(confint(mod)[2,1],3)),",",
            as.numeric(round(confint(mod)[2,2],3)),sep=""))

# Mobility
mod  <- feols(completely_home_device_perc ~ smokevar*median_income | fipsmonth + date, data=safe, weights=safe$population,lean = T)
ct <- coeftable(mod)
ci <- confint(mod)
print("Figure 3 statistics for mobility:")
print(paste("p=",ct[2,4],", effect size on linear interaction =",
            round(ct[2,1],3),", CI:",as.numeric(round(confint(mod)[2,1],3)),",",
            as.numeric(round(confint(mod)[2,2],3)),sep=""))
