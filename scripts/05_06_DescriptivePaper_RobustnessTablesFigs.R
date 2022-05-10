options("modelsummary_format_numeric_latex" = "plain")

##################################################################################################
#    ROBUSTNESS TESTS/ TAbles
##################################################################################################

# looking at robustness to distance to fire, temp/precip controls, for main outcomes


# load main datasets and merge with distance to fire
dt <- read_rds(file.path(path_dropbox, "panel_county_pm_smoke_day.RDS"))
dt$fips <- as.character(dt$county)
dt <- dt %>% mutate(wday = wday(date), month=month(date), year=year(date), fipsmonth=paste(fips,month,sep="_"), fipsmonthyear=paste(fips,year,month,sep="_"),dayofmonth=day(date), state=substr(county,1,2)) %>%
  arrange(fips,date)
df <- read_fst(file.path(path_dropbox, "fire/processed/county_pop_weighted_dist_to_fire_2006_2020.fst"))
dt <- left_join(dt,df)
# avg smoke PM exposure pre 2016
avgsmokepm <- dt %>% filter(year<2016) %>% group_by(fips) %>% summarise(avgsmokepm=mean(smokePM,na.rm=T), avgpm = mean(pm25,na.rm=T))
dt <- left_join(dt,avgsmokepm)
# region mapping
regions <- read_rds(file.path(path_dropbox, 'stateFIPS_epaREGION_crosswalk.rds')) %>% 
  select(`State Code`,`EPA Region`) %>% rename(state=`State Code`, epa_region=`EPA Region`) %>%
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
# twitter
twit <- read_fst(file.path(path_twitter, "county-sentiment.fst"))
twit$date <- as.Date(twit$date)
dt <- left_join(dt,twit,by=c("fips","date"))
# google
goog <- read_rds(file.path(path_gtrends, "google_trends_smoke_DMA_normalized_with_covariates.RDS"))
goog <- goog %>% mutate(month=month(date), year=year(date)) %>% mutate(dmamonth = paste(dma,month,sep="_"), dmamonthyear=paste(dma,year,month,sep="_"))
goog_panel <- read_rds(file.path(path_dropbox, 'panel_dma_pm_smoke_day_weekly.RDS'))
avgsmokepm <- goog_panel %>% mutate(year=year(week)) %>% filter(year<2016) %>% 
  group_by(dma) %>% summarise(avgsmokepm=mean(smokePM,na.rm=T), avgpm = mean(pm25,na.rm=T))
goog <- left_join(goog,avgsmokepm)
gf <- read_rds(file.path(path_dropbox, 'fire/processed/dma_weekly_dist_to_fire_cluster.RDS'))
gf <- gf %>% rename(date=week)
goog <- left_join(goog,gf)


# robusteness to temp/precip controls, distance to fire, different FE
dt$smokePMscale = dt$smokePM/1000  #rescaling so coefficients are legible. remember to note in table
df1 <- filter(dt,km_dist>50)
df2 <- filter(dt,km_dist<=50)
models <- list()
models[["baseline"]] <- feols(sent ~ smokePMscale  | fipsmonth + date, data=dt, weights = dt$population)
models[["temp/prec"]] <- feols(sent ~ smokePMscale + temperature + precipitation | fipsmonth + date, data=dt, weights = dt$population)
models[["distance > 50"]] <- feols(sent ~ smokePMscale | fipsmonth + date, data=df1, weights = df1$population)
models[["distance < 50"]] <- feols(sent ~ smokePMscale | fipsmonth + date, data=df2, weights = df2$population)
models[["FE1"]] <- feols(sent ~ smokePMscale | fipsmonth + date^state, data=dt, weights = dt$population)
models[["FE2"]] <- feols(sent ~ smokePMscale | fipsmonthyear + date, data=dt, weights = dt$population)
modelsummary(models,stars=TRUE,fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC',output = file.path(path_github, 'tables/Robust_twitter.tex'))


# mobility
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
modelsummary(models,stars=TRUE,fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC' ,output = file.path(path_github, 'tables/Robust_mobility.tex'))


#salience and protective measures
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
modelsummary(models,stars=TRUE,fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC', output = file.path(path_github, 'tables/Robust_GoogleTrends.tex'))




#####################################################################
# ROBUSTNESS OF SALIENCE AND PROTECTIVE MEASURES TO ALTERNATE WORD CHOICES
models <- list()
ys <- c("air quality","smoke", "air pollution","calidad del aire","humo")
for (y in ys) {
  df <- filter(goog,keyword==y)
  models[[y]] <- feols(hits ~ smokePM | dmamonth + date, weights=df$population, data=df)
}
modelsummary(models,stars=TRUE,fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC',output = file.path(path_github, 'tables/Salience_alt.tex'))


models <- list()
ys <- c("air filter","air purifier","purple air","smoke mask","purificador de aire","filtro de aire")
for (y in ys) {
  df <- filter(goog,keyword==y)
  models[[y]] <- feols(hits ~ smokePM | dmamonth + date, weights=df$population, data=df)
}
options("modelsummary_format_numeric_latex" = "plain")
modelsummary(models,stars=TRUE,fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC',output = file.path(path_github, 'tables/Protection_alt.tex'))

#  ROBUSTNESS TO PLACEBO SEARCH TERMS, also looking at whether wildfire specific search terms show up on non-smoke PM days
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
modelsummary(models,stars=TRUE,fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC',output = file.path(path_github, 'tables/Placebo_search.tex'))


########
#  search heterogeneity by distance to fire
models <- list()
ys <- c("air quality","air pollution","smoke")
for (y in ys) {
  df <- filter(goog,keyword==y)
  models[[y]] <- feols(hits ~ smokePM*distance | dmamonth + date, weights=df$population, data=df)
}
options("modelsummary_format_numeric_latex" = "plain")
modelsummary(models,stars=TRUE,fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC',output = file.path(path_github, 'tables/Search_distance.tex'))





#######################################################3
# HETEROGENEITY TABLES and FIGURES.  need to ahve pulled in data files starting at robustness aboe
dt$smokevar = dt$smokePM/1000  #rescaling so coefficients are legible in sentiment regressions. remember to note in table
dt$median_income = dt$median_household_income/1000  #rescale as well
safe_dt$median_income = safe_dt$median_household_income/1000
safe_dt$smokevar = safe_dt$smokePM  #renaming so table works out
goog$median_income <- goog$median_household_income/1000
goog$smokevar = goog$smokePM

# recenter moderators at their mean
recenter <- function(x) {x = x - mean(x,na.rm=T)}
dt <- dt %>% mutate(across(c(median_income, avgpm, avgsmokepm,km_dist), recenter))
safe_dt <- safe_dt %>% mutate(across(c(median_income, avgpm, avgsmokepm,km_dist), recenter))
goog <- goog %>% mutate(across(c(median_income, avgpm, avgsmokepm,distance), recenter))

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
modelsummary(hetmodels,stars=TRUE,fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC',coef_omit = "^(?!.*smokevar)", output=file.path(path_github, "tables/Heterogeneity1.tex"))  #print model summary, just keeping smoke vars and interactions


hetmodels <- list()
hetmodels[["sent_inc"]] <- feols(sent ~ smokevar*median_income  | fipsmonth + date, data=dt, weights = dt$population,lean = T)
hetmodels[["sent_avgsmoke"]] <- feols(sent ~ smokevar*avgsmokepm  | fipsmonth + date, data=dt, weights = dt$population,lean = T)
hetmodels[["sent_avgpm"]] <- feols(sent ~ smokevar*avgpm  | fipsmonth + date, data=dt, weights = dt$population,lean = T)
hetmodels[["sent_all"]] <- feols(sent ~ smokevar*(median_income + avgsmokepm + avgpm) | fipsmonth + date, data=dt, weights = dt$population,lean = T)

hetmodels[["mob_inc"]] <- feols(completely_home_device_perc ~ smokevar*median_income | fipsmonth + date, data=safe_dt, weights=safe_dt$population,lean = T)
hetmodels[["mob_avgsmoke"]] <- feols(completely_home_device_perc ~ smokevar*avgsmokepm | fipsmonth + date, data=safe_dt, weights=safe_dt$population,lean = T)
hetmodels[["mob_avgpm"]] <- feols(completely_home_device_perc ~ smokevar*avgpm | fipsmonth + date, data=safe_dt, weights=safe_dt$population,lean = T)
hetmodels[["mob_all"]] <- feols(completely_home_device_perc ~ smokevar*(median_income + avgsmokepm + avgpm) | fipsmonth + date, data=safe_dt, weights=safe_dt$population,lean = T)

modelsummary(hetmodels,stars=TRUE,fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC',coef_omit = "^(?!.*smokevar)", output=file.path(path_github, "tables/Heterogeneity2.tex"))  #print model summary, just keeping smoke vars and interactions


# 
# # get values of moderators to evaluate at.  get county means, then get quantiles of those
# avgs <- dt %>% group_by(county) %>% summarise(across(c(smokePM,pm25,median_household_income),mean,na.rm=T))
# quant <- avgs %>% mutate(across(c(smokePM,pm25,median_household_income), recenter))  #recenter to match regressions
# quant <- quant %>% summarise(across(c(smokePM,pm25,median_household_income),function(x) quantile(x,probs = c(0.05,0.95),na.rm=T)))
# names(quant) <- c("avgsmokepm","avgpm","median_income")
# quant$median_income <- quant$median_income/1000  #rescale to match what we ran in regression
# 
# 
# # now calculate effect of going from 5th to 95th percentile of moderator for each model
# outc <- c("sent","mob","sal","prot")  #sentiment, mobility, salience, protection
# mods <- c("inc","avgsmoke","avgpm","all")
# modnms <- c("median_income","avgsmokepm","avgpm")
# smokeval = 50  #value at which to evaluate marginal effect
# effs <- c()
# for (i in outc) {  #hella loops
#   for (j in mods) {
#     cf <- coef(hetmodels[[paste(i,j,sep="_")]])
#     vcov <- vcov(hetmodels[[paste(i,j,sep="_")]])
#     dof <- hetmodels[[paste(i,j,sep="_")]]$nobs - hetmodels[[paste(i,j,sep="_")]]$nparams 
#     if (j=="all") {
#       for (k in modnms) {
#         eff5 = (cf["smokevar"] + cf[paste0("smokevar:",k)]*quant[1,k])*smokeval
#         eff95 = (cf["smokevar"] + cf[paste0("smokevar:",k)]*quant[2,k])*smokeval
#         diff = quant[2,k] - quant[1,k]
#         sediff = sqrt(diff^2*diag(vcov)[paste0("smokevar:",k)])*smokeval  #standard error on the difference
#         effs <- rbind(effs,data.frame(outcome=i,model="all",mediator=k,eff5=as.numeric(eff5),eff95=as.numeric(eff95),sediff=as.numeric(sediff),dof=dof))      
#       }
#     } else {
#       modnm <- which(mods==j)
#       eff5 = (cf["smokevar"] + cf[paste0("smokevar:",modnms[modnm])]*quant[1,modnms[modnm]])*smokeval
#       eff95 = (cf["smokevar"] + cf[paste0("smokevar:",modnms[modnm])]*quant[2,modnms[modnm]])*smokeval
#       diff = quant[2,modnms[modnm]] - quant[1,modnms[modnm]]
#       sediff = sqrt(diff^2*diag(vcov)[paste0("smokevar:",modnms[modnm])])*smokeval
#       effs <- rbind(effs,data.frame(outcome=i,model="single",mediator=modnms[modnm],eff5=as.numeric(eff5),eff95=as.numeric(eff95),sediff=as.numeric(sediff),dof=dof))
#       
#     }
#   }  
# }
# #effs <- effs %>% mutate(diff=eff95-eff5, tstat = diff/sediff) %>% mutate(tstatl = ifelse(abs(tstat)>2,3,0.2))  #last bit is just for line width plotting
# effs <- effs %>% mutate(diff=eff95-eff5, tstat = diff/sediff, pval = 2*pt(-abs(tstat),df=dof)  )
# adjp <- p.fdr(pvalues=effs$pval, adjust.method="BH")
# effs$adj_pval = adjp$`Results Matrix`$`Adjusted p-values`
# effs <- effs %>% mutate(siggy=ifelse(adj_pval<0.05,3,0.2), siggy_bon=ifelse(pval< (0.05/dim(effs)[1]),3,0.2))
# 
# 
# # now make a plot
# outc <- c("sal","sent","mob","prot")  #reorder outcomes so consistent with other plots
# outcn <- c("salience","sentiment","mobility","protective")
# mods <- c("avg income","avg smoke PM","avg PM")
# colz=c("red","orange","blue")
# pdf('output/descriptive/HeterogeneousResponseAll.pdf',width=8,height=7)
# par(mfrow=c(4,3),mar=c(1,5,2,1))
# for (i in outc) {  #hella loops
#   toplot <- filter(effs,outcome==i)
#   rg <- range(c(toplot[,c("eff5","eff95")]))
#   for (j in modnms) {
#     plot(1,type="n",xlim=c(0,1),xlab="",axes=F,ylab="",ylim=rg)
#     axis(2,las=1)
#     abline(h=0,lty=2,col="grey")
#     dat = toplot[toplot$mediator==j & toplot$model=="single",]
#     lines(c(0.1,0.9),dat[c("eff5","eff95")],type="o",col=colz[which(modnms==j)],pch=19,lwd=dat["siggy"])
#     dat = toplot[toplot$mediator==j & toplot$model=="all",]
#     lines(c(0.1,0.9),dat[c("eff5","eff95")],type="o",col=colz[which(modnms==j)],pch=19,lwd=dat["siggy"],lty=2)
#     text(c(0.1,0.9),rg[1],c("5th","95th"))
#     if (j=="median_income") {mtext(outcn[which(outc==i)],2,line=2.5,cex=1.5)}
#     if (i=="sal") {mtext(mods[which(modnms==j)],3,cex=1.5)}
#   }
# }
# dev.off()

# make correlation plot of 3 heterogeneity variables
toplot <- avgs %>% select(-county)
M <- cor(toplot,use="complete.obs")
corrplot(M,type="upper",diag = F,method="number")


# robustness of heterogeneity to FE
mods <- list()
mods[["sent, base FE"]] <- feols(sent ~ smokevar*median_income  | fipsmonth + date + wday, data=dt, weights = dt$population,lean = T)
mods[["sent, alt FE"]] <- feols(sent ~ smokevar*median_income  | fips + date, data=dt, weights = dt$population,lean = T)
mods[["home, base FE"]] <- feols(completely_home_device_perc ~ smokevar*median_income | fipsmonth + date^State_FIPS, data=safe_dt, weights=safe_dt$population,lean = T)
mods[["home, alt FE 1"]] <- feols(completely_home_device_perc ~ smokevar*median_income | fips + date , data=safe_dt, weights=safe_dt$population,lean = T)
mods[["home, alt FE 2"]] <- feols(completely_home_device_perc ~ smokevar*median_income | fips + epa_region^date , data=safe_dt, weights=safe_dt$population,lean = T)
mods[["away, base FE"]] <- feols(completely_away_perc ~ smokevar*median_income | fipsmonth + date^State_FIPS, data=safe_dt, weights=safe_dt$population,lean = T)
mods[["away, alt FE 1"]] <- feols(completely_away_perc ~ smokevar*median_income | fips + date , data=safe_dt, weights=safe_dt$population,lean = T)
mods[["away, alt FE 2"]] <- feols(completely_away_perc ~ smokevar*median_income | fips + epa_region^date , data=safe_dt, weights=safe_dt$population,lean = T)
modelsummary(mods,stars=TRUE,fmt=3,gof_omit = 'R2*|Log.Lik.|Std.Errors|R2|AIC|BIC',coef_omit = "^(?!.*smokevar)", output=file.path(path_github, "tables/Heterogeneity_mobility_robust.html"))  #print model summary, just keeping smoke vars and interactions








