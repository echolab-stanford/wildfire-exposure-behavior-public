if (!dir.exists(file.path(path_fire, "clusters"))) dir.create(file.path(path_fire, "clusters"))

#-------------------------------------------------------------------------------
# Get Fire Clusters
# Written by: Anne Driscoll
# 
# Gets clusters from the fire points to try to identify large fires (eg Camp Fire).
#-------------------------------------------------------------------------------
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
    saveRDS(year_fire, file.path(path_fire, paste0("clusters/clusters_", y, ".RDS")))
  
    print(y)
}
