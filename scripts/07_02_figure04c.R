#-------------------------------------------------------------------------------
# Figure 4 Panel c
# Written by: Sam Heft-Neal
#-------------------------------------------------------------------------------
# Histogram of infiltration
inf_dist <- read_rds(file.path(path_infiltration, "estimates", "PA_monitor_level_infiltration_estimates_sfr_clean_pc.rds"))

# Set up data frame with info needed to make plot (I do barplots instead of histogram here just because I have more control)
bincounts <- statar::xtile(inf_dist$est_dl, cutpoints = seq(.025,.575,.025)) %>% table() %>% as.numeric() #create counts of data in each bin
bardata <- data.frame(xleft = seq(.025,.6,.025)-.0125,xright = seq(.025,.6,.025)+.0125, ht = bincounts) #data frame with cols for bar left, right, and height
bardata[nrow(bardata),"ht"]<-33 #fix wrong count in top bin

# Plot
pdf(file.path(path_figures, "figure04c.pdf"), width =11, height = 3)

par(mar =c(3,3,0,0))

plot(0, 0, axes=F, xlab = "",ylab = "",pch = 16, cex=2, ylim = c(0, 200), col = NA, xlim =c(0,0.6)) #empty plot

plotHist(bardata, 'gray80', .5, 0,max(bardata$ht),max(bardata$ht), add.alpha('gray50', .25)) #call user defined function for plotting hists at bottom of response curves 

axis(1, at = seq(0, 0.6, .05),cex.axis = 1.1)
axis(2, las = 2, cex.axis = 1.1)

mtext(side = 1, text = "Infiltration Rate",line=3,cex=1.1)
mtext(side = 2, text = "Number of Residences",line=4,cex=1.1)

dev.off()
