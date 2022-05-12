dt <- read_rds(file.path(path_dropbox, 'bay_area_fig4_wf_indoor_outdoor_pm.rds'))

bins = seq(0,100,5)
dt$bin <- xtile(dt$pm_out,cutpoints=bins)

# dropping top bins since just one in each bin
dt <- filter(dt,pm_out<=80)


bns <- unique(dt$bin)
pdf(file=file.path(path_github, 'figures/raw/figureED10.pdf'),width=7,height=5,useDingbats = F)
plot(1,type="n",xlim=c(1,length(bns)+1),ylim=c(0,40),xlab="avg outdoor PM during smoke event", ylab="avg indoor PM during smoke event",axes=F)
axis(2,las=1)
for (i in 1:length(bns)) {
  toplot <- filter(dt,bin==bns[i])
  ratio <- max(toplot$pm_in)/min(toplot$pm_in)
  n <- dim(toplot)[1]
  points(i+0.5+rnorm(n,0,0.05),toplot$pm_in,pch=19,col=alpha("black",0.5))
  text(i+0.5,37,round(ratio,0),col="red")
  text(i+0.5,40,n,col="black")
}
labs <- dt %>% group_by(bin) %>% summarise(mean=mean(pm_out))
labs <- floor(labs$mean/5)
axis(1,at=1:length(bns)+0.5,paste(labs*5,(labs+1)*5,sep="-"),las=2)
dev.off()

