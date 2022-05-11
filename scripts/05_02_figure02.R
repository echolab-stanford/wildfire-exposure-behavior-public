

# Script to make figure 2, after running regressions in 


# function to make plot from bootstrap outputs
mkplot <- function(toplot, xlab=expression(paste("smoke PM (",mu,"g/",m^{3},")")), ylab="",title="",xlim=c(0,100),ylim=c(range(ci)),xmax=100, cicolor="orange",eval=1,hist="no",histvar=dt$smokePM,histscale=2,hist_y=-10,breaks=seq(0,110,1),samptext="",histcolor="lightblue") {
  toplot <- toplot %>% filter(xx<=xmax)
  ci <- apply(toplot[,2:dim(toplot)[2]],1,function(x) quantile(x,probs=c(0.025, 0.975)))*eval
  mn <- apply(toplot[,2:dim(toplot)[2]],1,mean)*eval
  plot(1,type="n",xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,axes=F,yaxs="i")
  axis(1)
  abline(h=0,lty=2,col="grey")
  xx <- toplot$xx
  polygon(c(xx,rev(xx)),c(ci[1,],rev(ci[2,])),col=cicolor,border=NA)
  lines(xx,mn,col="black",lwd=2)
  cxtxt=0.8
  mtext(samptext,side=3,line=-1.5,adj=0,cex=cxtxt)
  mtext(paste0("n = ",round(n/1000,0),"k"),side = 3,line=-2.5,adj=0,cex=cxtxt)
  if (hist=="yes") {
    hv <- Winsorize(histvar,minval = 0,maxval=110)
    hh <- log10(hist(hv,plot = F,breaks = breaks)$counts)
    hh[hh<0] <- 0
    ll=length(breaks)
    cts <- hh/max(hh)*histscale
    labvals=unique(round(hh))
    labvalsat = labvals/max(hh)*histscale
    rect(breaks[1:(ll-1)],hist_y,breaks[2:ll],cts+hist_y,col="grey",lwd=0.1)
  }
}

# load datasets to generate exposure histograms and sample counts
dt <- read_rds(file.path(path_dropbox, 'panel_county_pm_smoke_day.RDS'))
dt$fips <- as.character(dt$county)
twit <- read_fst(file.path(path_twitter, "county-sentiment.fst"))
twit$date <- as.Date(twit$date)
twit <- left_join(dt,twit,by=c("fips","date"))
goog <- read_rds(file.path(path_gtrends, "google_trends_smoke_DMA_normalized_with_covariates.RDS"))
safe <- read_rds(file.path(path_safegraph, 'safegraph_completely_home_ALL.rds'))
safe <- safe %>% rename(date=Date,fips=County_FIPS)
safe <- left_join(safe,dt,by=c("fips","date"))




pdf(file.path(path_github, 'figures/raw/figure02.pdf'),width=8,height=8)
par(mfrow=c(2,2),mar=c(4,4,1,4))
cll <- apply(sapply(c("orange"), col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=0.5)) 

# salience
outc <- "air quality"
toplot <- read_csv(file.path(path_output, paste0("bootstraps_gtrends_",outc,".csv")))
df <- goog %>% filter(keyword==outc) %>% drop_na('smokePM','hits')
n=dim(df)[1]
mkplot(toplot,ylim=c(-10,60),xlim=c(0,50),ylab="searches for 'air quality' (index)",samptext = "US marketing areas, weekly 2016-20", cicolor=cll, hist="yes",histvar = df$smokePM,histscale = 8,hist_y = -10, xmax=50)
axis(2,las=1,at=seq(0,60,10),labels=seq(0,60,10))


# happiness
toplot <- read_csv(file.path(path_output, "bootstraps_twitter_spline.csv"))
df <- twit %>% drop_na('smokePM','sent')
n=dim(df)[1]
mkplot(toplot,ylab="sentiment",ylim=c(-0.06,0.02),samptext = "US counties, daily 2017-20", cicolor=cll, hist="yes",histvar = df$smokePM,histscale = 0.01,hist_y = -0.06,xmax=100)
axis(2,las=1,at=seq(-0.05,0.02,0.01),labels=seq(-0.05,0.02,0.01))

# health seeking
outc <- "air filter"
toplot <- read_csv(file.path(path_output, paste0("bootstraps_gtrends_",outc,".csv")))
df <- goog %>% filter(keyword==outc) %>% drop_na('smokePM','hits')
n=dim(df)[1]
mkplot(toplot,ylim=c(-10,50),xlim=c(0,50),ylab="searches for 'filter' (index)",samptext="US marketing areas, weekly 2016-20", cicolor=cll, hist="yes",histvar = df$smokePM,histscale = 8,hist_y = -10,xmax=50)
axis(2,las=1,at=seq(0,50,10),labels = seq(0,50,10))

# mobility
toplot <- read_csv(file.path(path_output, "bootstraps_safegraph_home_spline_simpleFE.csv"))
df <- safe %>% drop_na('smokePM','completely_home_device_perc')
n=dim(df)[1]
mkplot(toplot,ylab="% completely at home", ylim=c(-1,5),samptext="US counties, daily 2019-2020", cicolor=cll,hist="yes",histvar=df$smokePM,hist_y = -1,histscale=0.8,xmax=100)
axis(2,las=1,at=0:5,labels = 0:5)

dev.off()
