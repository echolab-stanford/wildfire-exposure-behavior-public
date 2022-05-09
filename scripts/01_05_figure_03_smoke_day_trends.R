#-------------------------------------------------------------------------------
# Plot Map of Smoke Day Trends
# Written by: Anne Driscoll
# Last edited by: Jessica Li
#-------------------------------------------------------------------------------
# Load grid
grid = readOGR(file.path(path_dropbox, "10km_grid"), "10km_grid")
county_proj = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
grid = spTransform(grid, CRS(county_proj))

# Load smoke days over grid
sparse_smoke_days = readRDS(file.path(path_smoke, "sparse_smoke_grid.RDS"))
# Get annual count of smoke days of at least medium or heavy density for 2011-2020
# in each grid cell
sparse_smoke_days %<>% 
  filter(date > "20110101") %>%
  mutate(year = substr(date, 1, 4)) %>%
  group_by(id, year) %>%
  summarise(med_heavy_days = sum(medium>0 | dense>0), 
            heavy_days = sum(dense>0))

# Fill for non-smoke days
year_panel = expand.grid(id = unique(grid$ID), year = unique(sparse_smoke_days$year))
sparse_smoke_days %<>%
  merge(year_panel, by=c("id", "year"), all=T) %>%
  mutate(med_heavy_days = ifelse(is.na(med_heavy_days), 0, med_heavy_days),
         heavy_days = ifelse(is.na(heavy_days), 0, heavy_days))

# Calculate smoke day trends
id_vec = unique(grid$ID)
betas = data.frame(id=as.character(id_vec), 
                   beta_days_16_27=as.numeric(NA), 
                   beta_days_27=as.numeric(NA))
start_time = Sys.time()
# Takes ~ 10 minutes
for (i in 1:length(id_vec)) {
  cur_id = id_vec[i]
  cur_smoke = sparse_smoke_days[sparse_smoke_days$id == cur_id,]
  
  beta_days_16_27 = lm(med_heavy_days ~ as.numeric(year), cur_smoke)$coefficients[[2]]
  beta_days_27 = lm(heavy_days ~ as.numeric(year), cur_smoke)$coefficients[[2]]
  
  betas[i, 2:3] = c(beta_days_16_27, beta_days_27)
}
end_time = Sys.time()
end_time - start_time

# TEMPORARY, CAN REMOVE LATER; SIMPLY HELPFUL FOR REFACTORING
saveRDS(betas, file.path(path_github, "data/figure_smoke_day_trends_betas.rds"))
betas = readRDS(file.path(path_github, "data/figure_smoke_day_trends_betas.rds"))

# Prepare smoke day trends data for plotting
grid_geo = fortify(grid, region="ID")
data = merge(grid_geo, betas, by="id", all=T)
data = data %>% arrange(id, piece, order)

saveRDS(data, file.path(path_github, "data/figure_smoke_day_trends.RDS"))

# Read in EPA locations
epa_ll = readOGR(file.path(path_dropbox, "epa_station_locations"), 
                 "epa_station_locations") 
epa_ll = epa_ll[epa_ll$lon >= -125.18 & epa_ll$lon <= -66.63 &
                  epa_ll$lat >= 24.64 & epa_ll$lat <= 49.41,]

# Prepare for plotting
cols = c('#000080', '#0047AB', '#0096FF', '#89CFF0', '#ffffff',
         '#EE4B2B', '#D22B2B', '#DC143C', '#800000')
lim = c(min(betas$beta_days_27)-0.01, max(betas$beta_days_27)+0.01)
diff = c(seq(lim[1], 0, length.out=4), 0, seq(0, lim[2], length.out=4))

# Plot map and save
g = ggplot() + # the data
  geom_polygon(data=data, aes(long,lat,group=group, fill=beta_days_27)) + # make polygons 
  geom_point(aes(lon, lat), size=0.0001, data=epa_ll@data) + 
  scale_fill_gradientn(limits=lim, colors=cols, values=scales::rescale(diff), n.breaks = 6) + 
  theme(line = element_blank(),  # remove the background, tickmarks, etc
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank()) + 
  coord_map("bonne", mean(data$lat)) + labs(fill="")
ggsave(file.path(path_github, "figures/smoke_day_trends.jpg"), 
       plot = g, width=10, height=5, units="in")
ggsave(file.path(path_github, "figures/smoke_day_trends.pdf"),
       plot = g, width=10, height=5, units="in")

# Save legend separately
legend = cowplot::get_legend(g)
pdf(file=file.path(path_github, "figures/smoke_day_trends_legend.pdf"), 
    width=800, height=2000)
ggpubr::as_ggplot(legend)
dev.off()
