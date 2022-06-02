if (!dir.exists(file.path(path_fire, "processed", "grid_dist_fire"))) dir.create(file.path(path_fire, "processed", "grid_dist_fire"), recursive = T)

#-------------------------------------------------------------------------------
# Get Distance to Fire Over 10km Grid
# Written by: Anne Driscoll
# 
# Get distance to nearest fire point for all grid cell-days.
#-------------------------------------------------------------------------------
years = 2006:2020

# Load 10km grid
grid = readRDS(file.path(path_dropbox, "grid.RDS"))
poly_grid = gBuffer(grid, byid=T, width=5000, capStyle="SQUARE") #slow

# loop through years
fire_files = list.files(path_fire)
for (i in 1:length(years)) {
  
  # get year
  year = years[i]
  
  # get list of days in that year
  days = seq(as.Date(paste0(year, "-01-01")), 
             as.Date(paste0(year, "-12-31")), "days")
  days = gsub("-", "", as.character(days))
  out = as.list(rep(NA, length(days)))
  
  # loop through days in the year
  for (j in 1:length(days)) {
    
    #figure out file to open
    file = paste0("hms_fire", days[j], ".shp")
    
    if (file %in% fire_files) {
      fires = readOGR(file.path(path_fire, paste0("hms_fire", days[j], ".shp")), 
                      paste0("hms_fire", days[j]), verbose=F)
      crs(fires) = "+proj=longlat +datum=WGS84 +no_defs"
      fires = fires[fires$Lon > -144 & fires$Lon < -32 & 
                      fires$Lat > -7 & fires$Lat < 70, ] #limit to north america
      fires = spTransform(fires, crs(grid))
      
      dist = get.knnx(coordinates(fires), coordinates(grid), k=1)
      
      df = data.frame(id=grid$ID, date=days[j], km_dist=dist$nn.dist/1000)
    } else {
      df = data.frame(id=grid$ID, date=days[j], km_dist=NA)
    }
    
    out[[j]] = df
    if (j%%50==0) {print(j)}
  }
  
  out = rbindlist(out)
  saveRDS(out, file.path(path_fire, "processed", "grid_dist_fire", paste0("dist_to_fire_", year, ".RDS")))
  
  print(year)
  
}
