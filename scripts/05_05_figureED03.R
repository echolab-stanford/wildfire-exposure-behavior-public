
# replication for Fig ED3

# not sure we need all these
# library(RColorBrewer)


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



#  function to plot interaction models
mkplot2 <- function(toplot, xlab="smokePM", ylab="",title="",xlim=c(0,100),ylim=c(range(ci)), cicolor="orange",eval=1,hist="no",histvar=dt$smokePM,histscale=2,hist_y=-10,breaks=seq(0,110,1),samptext="",histcolor="lightblue") {
  ci <- apply(toplot[,2:dim(toplot)[2]],1,function(x) quantile(x,probs=c(0.025, 0.975)))*eval
  mn <- apply(toplot[,2:dim(toplot)[2]],1,mean)*eval
  plot(1,type="n",xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,axes=F,yaxs="i")
  axis(2,las=1)
  axis(1)
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



#  make combined Fig ED3
pdf(file.path(path_github, 'figures/raw/figureED03.pdf'),width=8,height=8)
par(mfrow=c(2,2),mar=c(4,4,1,1))
cll <- apply(sapply(c("orange"), col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=0.5)) 

toplot <- read_csv(file.path(path_output, "bootstraps_safegraph_home_spline_simpleFE.csv"))
df <- safe %>% drop_na('smokePM','completely_home_device_perc')
n=dim(df)[1]
mkplot(toplot,ylab="% completely at home", ylim=c(-1,5),samptext="US counties, daily 2019-2020", cicolor=cll,hist="yes",histvar=df$smokePM,hist_y = -1,histscale=0.8,xmax=100)
axis(2,las=1,at=0:5,labels = 0:5)

toplot <- read_csv(file.path(path_output, "bootstraps_safegraph_away_spline_simpleFE.csv"))
df <- safe %>% drop_na('smokePM','completely_home_device_perc')
n=dim(df)[1]
mkplot(toplot,ylab="% completely away", ylim=c(-1,5),samptext="US counties, daily 2019-2020", cicolor=cll,hist="yes",histvar=df$smokePM,hist_y = -1,histscale=0.8,xmax=100)
axis(2,las=1,at=0:5,labels = 0:5)



# mobility robustness panels
eval = 50

cll <- apply(sapply(c("orange","lightblue"), col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=0.5)) 
toplot <- read_csv(file.path(path_output, "bootstraps_safegraph_incomeinteract_home_smokeweek.csv"))
mkplot2(toplot,title="Smoke previous week, effect on % home",ylab="effect of 50ug no % completely at home",eval=50,xlim=c(30000,120000),ylim=c(-10,10),cicolor=cll[1],xlab="median hhold income",hist="yes",histvar=safe$median_household_income,breaks=seq(20000,140000,5000))
toplot <- read_csv(file.path(path_output, "bootstraps_safegraph_incomeinteract_home_smokeweek_simpleFE.csv"))
ci <- apply(toplot[,2:dim(toplot)[2]],1,function(x) quantile(x,probs=c(0.025, 0.5, 0.975),na.rm=T))*eval
mn <- apply(toplot[,2:dim(toplot)[2]],1,mean,na.rm=T)*eval
xx <- toplot$xx
polygon(c(xx,rev(xx)),c(ci[1,],rev(ci[3,])),col=cll[2],border=NA)
lines(xx,mn,col="black",lwd=2)
abline(v=mean(dt$median_household_income,na.rm=T),lty=3)

toplot <- read_csv(file.path(path_output, "bootstraps_safegraph_incomeinteract_away_smokeweek.csv"))
mkplot2(toplot,title="Smoke previous week, effect on % away",ylab="effect of 50ug on % completely away",eval=50,xlim=c(30000,120000),ylim=c(-10,10),cicolor=cll[1],xlab="median hhold income",hist="yes",histvar=safe$median_household_income,breaks=seq(20000,140000,5000))
toplot <- read_csv(file.path(path_output, "bootstraps_safegraph_incomeinteract_away_smokeweek_simpleFE.csv"))
ci <- apply(toplot[,2:dim(toplot)[2]],1,function(x) quantile(x,probs=c(0.025, 0.5, 0.975),na.rm=T))*eval
mn <- apply(toplot[,2:dim(toplot)[2]],1,mean,na.rm=T)*eval
xx <- toplot$xx
polygon(c(xx,rev(xx)),c(ci[1,],rev(ci[3,])),col=cll[2],border=NA)
lines(xx,mn,col="black",lwd=2)
abline(v=mean(dt$median_household_income,na.rm=T),lty=3)
dev.off()

