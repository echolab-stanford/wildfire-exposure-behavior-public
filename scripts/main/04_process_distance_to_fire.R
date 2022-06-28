if (!dir.exists(file.path(path_fire, "clusters"))) dir.create(file.path(path_fire, "clusters"))

#-------------------------------------------------------------------------------
# Get Fire Clusters
# Written by: Anne Driscoll
# 
# Gets clusters from the fire points to try to identify large fires (eg Camp Fire).
#-------------------------------------------------------------------------------
if (!all(file.exists(file.path(path_fire, "clusters", paste0("clusters_", 2006:2020, ".RDS"))))) {
  # Read in data
  # Original fire data at: https://www.ospo.noaa.gov/Products/land/hms.html
  # here all the individual day files are stored in a list
  # As used in Burke et al 2021 "The changing risk and burden of wildfire in the United States"
  fire = read_rds(file.path(path_fire, "hms_fires.RDS"))
  years = 2006:2020
  fire = fire[names(fire) < paste0(max(as.numeric(years))+1,"0101")]
  
  # width is the number fire points are buffered to merge into fire clusters
  width = 2.9/111 # km/number of km at eq (to get in lat lon units)
  # 2.9 because it's a 4km grid cell, to reach diagonal need sqrt(2^2+2^2)
  
  # Get data for each grid cell, for each year. 
  j = 1
  
  # Loop through years
  for (i in 1:length(years)) {
    
    # Get the fires that happened during the year of interest
    y = years[i]
    year_fire = grepl(paste0("^", y), names(fire))
    first = max(c(1, Position(function(x){x==T}, year_fire)-3))
    last = max(c(length(year_fire) - Position(function(x){x==T}, rev(year_fire)) + 1))
    year_fire = fire[first:last]
    year_fire_sp = as.list(rep(NA, length(year_fire)))
    start_loop = ifelse(first == 1, 1, 4)
    
    # Loop through days in the year
    prog = txtProgressBar(min=0, max=length(year_fire), initial=0, char="-", style=3)
    for (k in start_loop:length(year_fire)) {
      
      # sf to sp
      date = names(year_fire)[[k]]
      
      # Using fire data for the 3 days previous as well to capture long burning fires
      # Figure out which days to combine to get the correct set
      if (k>4) {
        f = year_fire[(k-3):k]
        for (z in 1:4) {
          st_crs(f[[z]]) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        }
        f = rbind(f[[1]][, c("geometry")], 
                  f[[2]][, c("geometry")], 
                  f[[3]][, c("geometry")], 
                  f[[4]][, c("geometry")])
      } else {f = year_fire[[k]][,  c("geometry")]}
      if (nrow(f)==0) {next}
      
      # Convert f to an SP object
      f = tryCatch({
        as_Spatial(f)
      }, error = function(e) {
        f = f[!is.na(st_is_valid(f)) & !st_is_empty(f),]
        f = st_cast(f, "POINT")
        as_Spatial(f)
      })
      
      # Buffer by 'width' to merge adjacent pixels
      f_buf = gBuffer(f, byid=T,  width=width, capStyle="SQUARE", quadsegs=1)
      f_buf = st_cast(st_union(st_as_sf(f_buf)), "POLYGON")
      f_buf = SpatialPolygonsDataFrame(as_Spatial(f_buf), data.frame(id=1:length(f_buf)), match.ID=F)
      f_buf$area = gArea(f_buf, byid=T)*12321
      f_buf$date = date
      f = f_buf
      
      # Create new ids for clustered fires so that they can all be merged at the end
      f$ID = j:(nrow(f)+j-1)
      row.names(f) = as.character(j:(length(f$ID)+j-1))
      j = length(f$ID)+j
      year_fire_sp[[k]] = f
      
      if (k %% 5 == 0) {setTxtProgressBar(prog, k)}
    }
    
    # rbind the list of results to get one massive data frame
    w = sapply(year_fire_sp, function(x){"SpatialPolygonsDataFrame" %in%  class(x)})
    year_fire = year_fire_sp[w] #remove the ones that were empty or broken
    year_fire_df = rbindlist(lapply(year_fire, function(x){x@data}), fill=T)
    year_fire = unlist(lapply(year_fire, function(x){x@polygons}))
    
    # Convert to a SpatialPolygonsDataFrame
    row.names(year_fire_df) = year_fire_df$ID
    year_fire = SpatialPolygonsDataFrame(SpatialPolygons(year_fire), year_fire_df)
    crs(year_fire) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    saveRDS(year_fire, file.path(path_fire, "clusters", paste0("clusters_", y, ".RDS")))
    
    print(y)
  }
}

if (!dir.exists(file.path(path_fire, "distance_to_fire", "grid_dist_fire"))) {
  dir.create(file.path(path_fire, "distance_to_fire", "grid_dist_fire"), recursive = T)
}

#-------------------------------------------------------------------------------
# Get Distance to Fire Over 10km Grid
# Written by: Anne Driscoll
# 
# Get distance to nearest fire point for all grid cell-days.
#-------------------------------------------------------------------------------
years = 2006:2020

if (!all(file.exists(file.path(path_fire, "distance_to_fire", "grid_dist_fire", 
                               paste0("dist_to_fire_", 2006:2020, ".RDS"))))) {
  
  # Load 10km grid
  grid = readRDS(file.path(path_boundaries, "grid.RDS"))
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
        fires = readOGR(file.path(path_fire, paste0("points/hms_fire", days[j], ".shp")), 
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
    saveRDS(out, file.path(path_fire, "distance_to_fire", "grid_dist_fire", 
                           paste0("dist_to_fire_", year, ".RDS")))
    
    print(year)
    
  }
}

#-------------------------------------------------------------------------------
# Get Population Over 10km Grid
# Written by: Anne Driscoll
#-------------------------------------------------------------------------------
# Read in data needed
pop = raster(file.path(path_population, "gpw_v4_population_count_rev11_2010_2pt5_min.tif"))

# Define grid as shapes rather than points
grid = readRDS(file.path(path_boundaries, "grid.RDS"))
grid = gBuffer(grid, byid=T, width=5000, capStyle="SQUARE")
grid = spTransform(grid, crs(pop))

# Extract
pop = crop(pop, extent(grid))
grid_pop = exact_extract(pop, grid, 
                         fun=function(val, wts){sum(val, na.rm=T)})
data_out = data.frame(id=grid$ID, pop=grid_pop)
saveRDS(data_out, file.path(path_population, "grid_population.RDS"))

if (!dir.exists(file.path(path_fire, "distance_to_fire", "county_pop_weighted"))) {
  dir.create(file.path(path_fire, "distance_to_fire", "county_pop_weighted"))
}

#-------------------------------------------------------------------------------
# Get County Distance to Fire
# Written by: Anne Driscoll
# Last edited by: Jessica Li
# 
# Get population weighted distance to nearest fire point for all county-days.
#-------------------------------------------------------------------------------
# Read in data
crs_m = "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
years = 2006:2020

if (!all(file.exists(file.path(
  path_fire, "distance_to_fire", "county_pop_weighted", 
  paste0("county_pop_weighted_dist_to_fire_", 2006:2020, ".RDS"))))) {
  
  grid_pop = readRDS(file.path(path_population, "grid_population.RDS"))
  grid = readOGR(file.path(path_boundaries, "10km_grid"), "10km_grid")
  grid = spTransform(grid, crs_m)
  
  counties = readRDS(file.path(path_boundaries, "all_national_counties.RDS"))
  counties = counties[counties$INTPTLAT > 24.7 & counties$INTPTLAT < 49.3 &
                        counties$INTPTLON > -125.2 & counties$INTPTLON < -66.4, ]
  counties = spTransform(counties, crs_m)
  
  # Get the county to grid mapping
  crosswalk = over(counties, grid, returnList=T)
  for (i in 1:length(crosswalk)) {
    cur = crosswalk[[i]]
    if (nrow(cur) == 0) {next}
    cur$county = counties$GEOID[i]
    crosswalk[[i]] = cur
  }
  w = sapply(crosswalk, nrow) > 0
  crosswalk = rbindlist(crosswalk[w])
  crosswalk = merge(crosswalk, grid_pop, by.x="ID", by.y="id", all.x=T)
  crosswalk = crosswalk %>% select(county, ID, pop)
  
  for (i in 1:length(years)) {
    year = years[i]
    
    dists = readRDS(file.path(path_fire, "distance_to_fire", "grid_dist_fire", 
                              paste0("dist_to_fire_", year, ".RDS")))
    dists = merge(dists, crosswalk, by.x="id", by.y="ID",
                  all=T, allow.cartesian=T)
    dists = dists %>%
      group_by(county, date) %>%
      summarise(km_dist = wtd.mean(km_dist, weights=pop))
    
    saveRDS(dists, file.path(path_fire, "distance_to_fire", "county_pop_weighted", 
                             paste0("county_pop_weighted_dist_to_fire_", year, ".RDS")))
  }
  
  # Combine
  df <- c()
  for (i in 2006:2020) {
    dt <- read_rds(file.path(path_fire, "distance_to_fire", "county_pop_weighted", 
                             paste0('county_pop_weighted_dist_to_fire_',i,".RDS")))
    df <- rbind(df,dt)
    print(i)
  }
}

if (!file.exists(file.path(path_fire, "distance_to_fire", "county_pop_weighted", 
                           "county_pop_weighted_dist_to_fire_2006_2020.fst"))) {
  df$date <- ymd(df$date)
  write_fst(df, file.path(path_fire, "distance_to_fire", "county_pop_weighted", 
                          "county_pop_weighted_dist_to_fire_2006_2020.fst"))
}
