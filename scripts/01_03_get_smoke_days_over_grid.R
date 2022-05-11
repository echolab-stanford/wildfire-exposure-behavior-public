#-------------------------------------------------------------------------------
# Get Smoke Days Over Grid
# Written by: Anne Driscoll
# Last edited by: Jessica Li
# 
# Count the number of plumes overhead at the grid level for 2006-2020.
#-------------------------------------------------------------------------------
years = 2006:2020

#-------------------------------------------------------------------------------
#### Read in data ####
# Original smoke data at: https://www.ospo.noaa.gov/Products/land/hms.html
# Here all the individual day files have been processed and combined
smoke = readRDS(file.path(path_smoke, "smoke_plumes_spdf.RDS")) # same as used in wildfire-map-public
# doesn't include the manually downloaded smoke shapes Jessica later found

# Get grid over which to query for smoke
poly_grid = readRDS(file.path(path_dropbox, "grid.RDS"))
poly_grid = gBuffer(poly_grid, byid=T, width=5000, capStyle="SQUARE") #slow

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

crs(smoke) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
poly_grid = spTransform(poly_grid, crs(smoke))

# Get the plumes over each grid id for all time
# Takes ~11 hours 40 minutes
path_intermediate = file.path(path_smoke, "intermediate")
if (!dir.exists(path_intermediate)) dir.create(path_intermediate)
start_time = Sys.time()
for (i in 1:length(years)) {
  
  print(years[i])
  cur = smoke[smoke$year == years[i], ]
  post = years[i] > 2010
  
  for (j in 1:12) {
    
    cur_month = cur[cur$month == j, ]
    
    tic()
    overlap = over(poly_grid, cur_month, returnList=T) # get the overlaps
    toc()
    
    tic()
    
    if (post) { # get the overlaps depending on year
      overlap = lapply(overlap, get_plumes) # get number of plumes from list 
    } else {  
      overlap = lapply(overlap, get_plumes_pre2011) # get number of plumes from list 
    }
    
    for (k in 1:length(overlap)) { # get the grid ID in there
      temp = overlap[[k]]  
      temp$id = poly_grid$ID[k]
      overlap[[k]] = temp
    }
    overlap = as.data.frame(data.table::rbindlist(overlap)) # make a data frame
    toc()
    
    saveRDS(overlap, 
            file.path(path_intermediate, 
                      paste0("smoke_grid_", years[i], "_", j, ".RDS")))
    print(j)
  }
}
end_time = Sys.time()
end_time - start_time

files = list.files(path_intermediate)
combined = as.list(rep(NA, length(files)))

for (i in 1:length(files)) {
  combined[[i]] = readRDS(file.path(path_intermediate, files[i]))
}
combined = rbindlist(combined, fill=T)
combined = combined %>% mutate(date = as.character(date))
saveRDS(combined, file.path(path_smoke, "sparse_smoke_grid.RDS"))
