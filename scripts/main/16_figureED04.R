#-------------------------------------------------------------------------------
# Figure ED04
# Written by: Marshall Burke
#-------------------------------------------------------------------------------
# Load data
dt <- read_rds(file.path(path_smokePM, 'panel_county_pm_smoke_day.RDS'))
dt$fips <- as.character(dt$county)

# Cut by income group
dt$inc_cut <- cut(dt$median_household_income/1000,breaks=quantile(dt$median_household_income/1000,probs = seq(0,1,0.1)))
sumexp <- dt %>% group_by(inc_cut) %>% summarise(meansmokePM=mean(smokePM,na.rm = T),smokeday=sum(smokePM>0,na.rm=T),smoke25=sum(smokePM>25,na.rm=T),smoke50=sum(smokePM>50,na.rm=T),n=sum(is.na(smokePM)==F))
sumexp <- sumexp %>% mutate(smokedaypct = smokeday/n, smoke25pct = smoke25/n, smoke50pct=smoke50/n)

cuts <- sumexp$inc_cut[is.na(sumexp$inc_cut)==F]
cll <- apply(sapply("black", col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=c(0.1)))

# Plot
pdf(file.path(path_figures, 'figureED04.pdf'),width=12,height=8)
par(mar=c(5,10,1,18))
plot(1,type="n",xlim=c(0,300),ylim=c(1,11),axes=F,xlab="",ylab="")
axis(1,cex.axis=1.5)

# Make axis labels
labs <- paste0("$",substr(cuts,2,5),"-",substr(cuts,7,10))
labs[10] <- substr(labs[10],1,9)  #manually fix last one
axis(2,at=1:10,labels=labs,las=1,cex.axis=1.5)
for (i in 1:length(cuts)) {
  samp <- dt %>% filter(inc_cut==cuts[i]) %>% select(smokePM) %>% filter(is.na(smokePM)==F & smokePM>0)
  ll=dim(samp)[1]
  points(samp$smokePM,rep(i,ll)+rnorm(ll,0,0.1),pch=19,col=cll,cex=0.5)
}
cexl=1.5
mtext(side=4,line=0,at=c(1:10,10.7,11),text=rev(c("% days",">0ug",round(sumexp$smokedaypct[1:10]*100,1))),cex=cexl,las=1,adj=0)
mtext(side=4,line=6,at=c(1:10,10.7,11),text=rev(c("% days",">25ug",round(sumexp$smoke25pct[1:10]*100,2))),cex=cexl,las=1,adj=0)
mtext(side=4,line=12,at=c(1:10,10.7,11),text=rev(c("% days",">50ug",round(sumexp$smoke50pct[1:10]*100,2))),cex=cexl,las=1,adj=0)
mtext(side=2,line=8,text="county median income decile ($1000)",cex=cexl)
mtext(side=1,line=3,text="smoke PM2.5",cex=cexl)
dev.off()
