#-------------------------------------------------------------------------------
# Figure 3
# Written by: Marshall Burke
#-------------------------------------------------------------------------------
# Load datasets to generate exposure histograms and sample counts
dt <- read_rds(file.path(path_smokePM, 'panel_county_pm_smoke_day.RDS'))
dt$fips <- as.character(dt$county)
twit <- read_fst(file.path(path_twitter, "county-sentiment.fst"))
twit$date <- as.Date(twit$date)
twit <- left_join(dt,twit,by=c("fips","date"))
goog <- read_rds(file.path(path_gtrends, "google_trends_smoke_DMA_normalized_with_covariates.RDS"))
safe <- read_rds(file.path(path_safegraph, 'safegraph_completely_home_ALL.rds'))
safe <- safe %>% rename(date=Date,fips=County_FIPS)
safe <- left_join(safe,dt,by=c("fips","date"))

#-------------------------------------------------------------------------------
# Function for making heterogeneity plots
mkplot2 <- function(toplot, 
                    xlab="smokePM", 
                    ylab="",
                    title="",
                    xlim=c(0,100),
                    ylim=c(range(ci)), 
                    cicolor="orange",
                    eval=1,
                    hist="no",
                    histvar=dt$smokePM,
                    histscale=2,
                    hist_y=-10,
                    breaks=seq(0,110,1),
                    samptext="",
                    histcolor="lightblue") {
  ci <- apply(toplot[,2:dim(toplot)[2]],1,function(x) quantile(x,probs=c(0.025, 0.975)))*eval
  mn <- apply(toplot[,2:dim(toplot)[2]],1,mean)*eval
  plot(1,type="n",xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,axes=F,yaxs="i")
  axis(2,las=1)
  abline(h=0,lty=2,col="grey")
  xx <- toplot$xx
  polygon(c(xx,rev(xx)),c(ci[1,],rev(ci[2,])),col=cicolor,border=NA)
  lines(xx,mn,col="black",lwd=2)
  cxtxt=0.8
  mtext(samptext,side=3,line=-1.5,adj=0,cex=cxtxt)
  if (hist=="yes") {
    hv <- hist(histvar,plot = F,breaks=c(breaks,max(histvar,na.rm=T)))
    cts <- hv$counts/max(hv$counts)*histscale
    ll=length(hv$breaks)
    rect(hv$breaks[1:(ll-1)],hist_y,hv$breaks[2:ll],cts+hist_y,col=histcolor,lwd=0.1)
  }
}

#-------------------------------------------------------------------------------
# Plot
pdf(file.path(path_figures, 'figure03.pdf'),width=8,height=8)
par(mfrow=c(2,2),mar=c(4,4,1,3))

# Salience
outc <- "air quality"
toplot <- read_csv(file.path(path_beh_bootstraps, paste0("bootstraps_gtrends_",outc,"_incomeinteract.csv")))
df <- goog %>% 
  filter(keyword==outc) %>% 
  mutate(date=as.Date(date),dma=geo) %>% 
  mutate(month=month(date)) %>% 
  mutate(dmamonth = paste(dma,month,sep="_"))
mod <- feols(hits ~ smokePM*median_household_income | dmamonth + date, weights=df$population, data=df)
b=round(coef(mod)["smokePM:median_household_income"]*10000*50,1)  # Evaluating a 50ug exposure at a $10k income increase
pval=fixest::pvalue(mod)["smokePM:median_household_income"]
n=mod$nobs
mkplot2(toplot,ylab=expression(paste("effect of 50",mu,"g day on searches for 'air quality'")),
        eval=50,xlab="median household income ($1000)",
        xlim=c(25000,100000),cicolor="lightblue",ylim=c(0,100),
        samptext = paste0("slope=",b, " per $10k (p=",round(pval,3),")"),
        hist="yes",histvar = goog$median_household_income,
        breaks=seq(25000,100000,2000),hist_y=0,histscale=7,histcolor = "grey")
axis(1, at=seq(30,100,10)*1000,seq(30,100,10))

# Sentiment
toplot <- read_csv(file.path(path_beh_bootstraps, "bootstraps_twitter_incomeinteract.csv"))
twit <- twit %>% mutate(fipsmonth=paste(fips,month(date),sep="_"),wday=wday(date),state=substr(fips,1,2))
mod <- feols(sent ~ smokePM*median_household_income  | fipsmonth + date, data=twit, weights = twit$population)
b=round(coef(mod)["smokePM:median_household_income"]*10000*50,3)
pval=fixest::pvalue(mod)["smokePM:median_household_income"]
n=mod$nobs
mkplot2(toplot,eval=50,xlim=c(25000,100000),cicolor = "lightblue",
        ylab=expression(paste("effect of 50",mu,"g day on sentiment")),
        xlab="median household income ($1000)",ylim=c(-0.015,0.005),
        hist="yes",histvar=safe$median_household_income,breaks=seq(20000,140000,2000),
        hist_y = -0.015,histscale = 0.002,
        samptext = paste0("slope=",b, " per $10k (p=",round(pval,3),")"),histcolor="grey")
axis(1, at=seq(30,100,10)*1000,seq(30,100,10))

# Health protection
outc <- "air filter"
toplot <- read_csv(file.path(path_beh_bootstraps, paste0("bootstraps_gtrends_",outc,"_incomeinteract.csv")))
df <- goog %>% 
  filter(keyword==outc) %>% 
  mutate(date=as.Date(date),dma=geo) %>% 
  mutate(month=month(date)) %>% 
  mutate(dmamonth = paste(dma,month,sep="_"))
mod <- feols(hits ~ smokePM*median_household_income | dmamonth + date, weights=df$population, data=df)
b=round(coef(mod)["smokePM:median_household_income"]*10000*50,1)
pval=fixest::pvalue(mod)["smokePM:median_household_income"]
n=mod$nobs
mkplot2(toplot,ylab=expression(paste("effect of 50",mu,"g day on searches for 'air filter'")),
        eval=50,xlab="median household income ($1000)",
        xlim=c(30000,100000),cicolor="lightblue",ylim=c(-30,60),
        samptext = paste0("slope=",b, " per $10k (p=",round(pval,3),")"),
        hist="yes",histvar = goog$median_household_income,
        breaks=seq(30000,100000,2000),hist_y=-30,histscale=7,histcolor = "grey")
axis(1, at=seq(30,100,10)*1000,seq(30,100,10))

# Mobility
safe <- safe %>% mutate(fipsmonth=paste(fips,month(date),sep="_"))
toplot <- read_csv(file.path(path_beh_bootstraps, "bootstraps_safegraph_incomeinteract_home_simpleFE.csv"))
mod <- feols(completely_home_device_perc ~ smokePM*median_household_income | fipsmonth + date, data=safe, weights=safe$population)
b=round(coef(mod)["smokePM:median_household_income"]*10000*50,1)
pval=fixest::pvalue(mod)["smokePM:median_household_income"]
n=mod$nobs
mkplot2(toplot,title="Smoke effect on % home",ylab="effect of 50ug on % completely at home",eval=50,
        xlim=c(25000,100000),ylim=c(-8,7),cicolor="lightblue",xlab="median hhold income ($1000)",
        hist="yes",histvar=safe$median_household_income,breaks=seq(20000,140000,2000),
        samptext = paste0("slope=",b, " per $10k (p=",round(pval,3),")"),histcolor = "grey",hist_y=-8)
axis(1, at=seq(30,100,10)*1000,seq(30,100,10))

dev.off()
