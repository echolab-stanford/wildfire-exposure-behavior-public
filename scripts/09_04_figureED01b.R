dt <- read_rds(file.path(path_dropbox, "station_pair_smokePM_correlation.rds"))
dt <- dt %>% filter(N>=1000) %>% mutate(r = sqrt(R2))
dt <- mutate(dt,bin=floor(distance_km/10)*10+5)
meds <- dt %>% group_by(bin) %>% summarise(r_med = median(r))
dt <- left_join(dt,meds)

pdf(file=file.path(path_github, "figures/raw/figureED01b.pdf"),width=6,height=5)
ggplot(dt, aes(x=distance_km, y=r) ) +
  geom_bin2d(bins = 50) +
  scale_fill_continuous(type = "viridis") +
  xlab("pairwise distance (km)") + ylab("r (pearson)") + 
  #  geom_smooth(color="black")  +
  geom_line(aes(x=bin,y=r_med),size=1.2) +
  theme_minimal()
dev.off()
