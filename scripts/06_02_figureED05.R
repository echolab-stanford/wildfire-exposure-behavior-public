# ------------------------------------------------------------------------------
# Plot distributions of income (tract level) across US and
# only places where we have PurpleAir monitors
# ------------------------------------------------------------------------------
# Get income at tract level
vars_acs_all <- load_variables(year = 2019, dataset = "acs5", cache = TRUE)
conus <- setdiff(states()$STUSPS, c("AK", "AS", "GU", "HI", "MP", "PR", "VI"))
# income_vbl_name <- "B19001_001" # total household income
# income_vbl_name <- "B19301_001" # per-capita income
income_vbl_name <- "B19013_001" # median household income
dat_acs <- get_acs(geography = "tract",
                   variables = income_vbl_name,
                   output = "wide",
                   year = 2019,
                   survey = "acs5",
                   state = conus,
                   geometry = TRUE) %>% 
  select(GEOID, matches("^B.*E$"))
names(dat_acs)[2] <- "median_income"
saveRDS(dat_acs, file.path(path_dropbox, "ACS_data.rds"))

# Get PA locations
pa_ll <- readRDS(file.path(path_dropbox, "PA_locations_all.rds")) %>% 
  st_as_sf(coords = c("Lon", "Lat"), crs = 4269, remove = FALSE) %>% 
  st_transform(st_crs(dat_acs))

# Merge ACS and PA
dat_merged <- pa_ll %>% st_join(dat_acs, left = FALSE)
saveRDS(dat_merged, file.path(path_dropbox, "ACS_PA_data.rds"))

# Lose 7 monitors that are on Alactraz Island, coastline, or country border
# leaflet(pa_ll) %>%
#   addTiles() %>%
#   addCircles(lng = pa_ll %>% filter(!(ID %in% dat_merged$ID)) %>% pull(Lon),
#              lat = pa_ll %>% filter(!(ID %in% dat_merged$ID)) %>% pull(Lat))

# ------------------------------------------------------------------------------
#monitors matched to acs
df <- read_rds(file.path(path_dropbox, 'ACS_PA_data.rds')) %>% 
  as.data.frame() %>% 
  select(-geometry)
#all tracts
acs <- read_rds(file.path(path_dropbox, 'ACS_data.rds'))

# collapse monitors to track obs so we count them correctly
df <- df %>% 
  group_by(GEOID,device_location) %>% 
  summarise(median_income=mean(median_income))

pdf(file=file.path(path_github, 'figures/PurpleAir_ACSIncome.pdf'),width=7,height=5)
hist(acs$median_income/1000,breaks=50,las=1,main="",xlab="median household income (1000s)",border=NA,ylab="",axes=F)
axis(1)
abline(v=median(acs$median_income/1000,na.rm=T),lty=2)
mx=5000
toplot <- filter(df,device_location=="outside")
hh <- hist(toplot$median_income/1000,plot=F,breaks=50)
hh$counts <- round(hh$counts/max(hh$counts)*mx,0)
plot(hh,add=T,col=alpha("red",0.2),border = NA)
abline(v=median(toplot$median_income/1000,na.rm=T),col="red",lty=2)
mx=3000
toplot <- filter(df,device_location=="inside")
hh <- hist(toplot$median_income/1000,plot=F,breaks=50)
hh$counts <- round(hh$counts/max(hh$counts)*mx,0)
plot(hh,add=T,col=alpha("blue",0.2),border = NA)
abline(v=median(toplot$median_income/1000,na.rm=T),col="blue",lty=2)
dev.off()
