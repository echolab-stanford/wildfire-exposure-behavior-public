#-------------------------------------------------------------------------------
# Table S10
# Written by: Sam Heft-Neal
#-------------------------------------------------------------------------------
# Load data
dat <- read_rds(file.path(path_epa, "epa_smoke_clean.rds"))

dat$unitFE <- dat$id
dat$unitmonthFE <- as.numeric(as.factor(paste(dat$id, dat$month, sep = "-")))
dat$timeFE <- as.numeric(dat$date - as.Date("2005-12-31"))

# Smoke day
mod1 <- (feols(pm25 ~ smoke_day | unitmonthFE + timeFE, data = dat[!is.na(dat$low_count),], cluster = "county"))

# Plume counts by density
dat$low_dummy <- as.numeric(dat$low_count >0)
dat$low_dummy[is.na(dat$low_count)]<-NA

dat$med_dummy <- as.numeric(dat$med_count >0)
dat$med_dummy[is.na(dat$med_count)]<-NA

dat$high_dummy <- as.numeric(dat$high_count >0)
dat$high_dummy[is.na(dat$high_count)]<-NA

mod2 <- (feols(pm25 ~ low_dummy + med_dummy + high_dummy |   unitmonthFE + timeFE, data = dat[!is.na(dat$low_count),], cluster = "county"))

# Save
etable(mod1, mod2, file = file.path(path_tables, "tableS10.tex"))

xx = dat[!is.na(dat$low_count),]

xx$pm25_r <- summary(feols(pm25 ~ unitmonthFE + timeFE, data = xx))$residuals
xx$smoke_day_r <- summary(feols(smoke_day ~ unitmonthFE + timeFE, data = xx))$residuals
xx$low_dummy_r <- summary(feols(low_dummy ~ unitmonthFE + timeFE, data = xx))$residuals
xx$med_dummy_r <- summary(feols(med_dummy ~ unitmonthFE + timeFE, data = xx))$residuals
xx$high_dummy_r <- summary(feols(high_dummy ~ unitmonthFE + timeFE, data = xx))$residuals

summary(feols(pm25_r ~ smoke_day_r + -1, data= xx))
