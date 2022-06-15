#-------------------------------------------------------------------------------
# Get Smoke Days Over Grid
# Written by: Anne Driscoll
# 
# Count the number of plumes overhead at the grid level for 2006-2020.
#-------------------------------------------------------------------------------
# Set years
years = 2006:2020

#-------------------------------------------------------------------------------
#### Read in data ####
# Original smoke data at: https://www.ospo.noaa.gov/Products/land/hms.html
# Here all the individual day files have been processed and combined
# As used in Burke et al 2021 "The changing risk and burden of wildfire in the United States"
smoke = readRDS(file.path(path_smoke, "smoke_plumes_spdf.RDS"))

# Get grid over which to query for smoke
poly_grid = readRDS(file.path(path_boundaries, "grid.RDS"))
poly_grid = gBuffer(poly_grid, byid=T, width=5000, capStyle="SQUARE")

#-------------------------------------------------------------------------------
#### Process smoke ####
# Prepare smoke data 
smoke@data = as.data.frame(smoke@data)
smoke$year = substr(smoke$date, 1, 4)
smoke = smoke[smoke$year < max(as.numeric(years))+1 &
                smoke$year > min(as.numeric(years))-1, ]
smoke$month = as.numeric(substr(smoke$date, 5, 6))
smoke$Density = as.character(smoke$Density)
smoke$Density = gsub(".000", "", smoke$Density)

# Transform
crs(smoke) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
poly_grid = spTransform(poly_grid, crs(smoke))

# Get the plumes over each grid id for all time
# Takes ~11 hours 40 minutes
if (!dir.exists(file.path(path_smoke, "intermediate"))) dir.create(file.path(path_smoke, "intermediate"))
start_time = Sys.time()
for (i in 1:length(years)) {
  
  print(years[i])
  cur = smoke[smoke$year == years[i], ]
  post = years[i] > 2010
  
  for (j in 1:12) {
    
    cur_month = cur[cur$month == j, ]
    
    tic()
    # Get the overlaps
    overlap = over(poly_grid, cur_month, returnList=T)
    toc()
    
    tic()
    
    # Get the overlaps depending on year
    if (post) {
      # Get number of plumes from list
      overlap = lapply(overlap, get_plumes)
    } else {
      # Get number of plumes from list
      overlap = lapply(overlap, get_plumes_pre2011) 
    }
    
    # Get the grid ID in there
    for (k in 1:length(overlap)) {
      temp = overlap[[k]]  
      temp$id = poly_grid$ID[k]
      overlap[[k]] = temp
    }

    # Make a data frame
    overlap = as.data.frame(data.table::rbindlist(overlap))
    toc()
    
    # Save
    saveRDS(overlap, 
            file.path(path_smoke, "intermediate", 
                      paste0("smoke_grid_", years[i], "_", j, ".RDS")))
    print(j)
  }
}
end_time = Sys.time()
end_time - start_time

# Combine
files = list.files(file.path(path_smoke, "intermediate"))
combined = as.list(rep(NA, length(files)))

for (i in 1:length(files)) {
  combined[[i]] = readRDS(file.path(path_smoke, "intermediate", files[i]))
}
combined = rbindlist(combined, fill=T)
combined = combined %>% mutate(date = as.character(date))

# Save
saveRDS(combined, file.path(path_smoke, "sparse_smoke_grid.RDS"))
