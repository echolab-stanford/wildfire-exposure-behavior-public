tract_files = list.files(file.path(path_dropbox, "tracts"))
tract_list = as.list(rep(NA, length(tract_files)))

prog = txtProgressBar(max=length(tract_files), style=3)
for (i in 1:length(tract_files)) {
    t = tract_files[i]
    t = readOGR(file.path(path_dropbox, t), t)
    
    ts = gSimplify(t, 0.005, topologyPreserve=T)
    
    #plot(ts[5:11, ], border="red")
    #plot(add=T, t[5:11, ])
    
    t = SpatialPolygonsDataFrame(ts, t@data)
    t = clgeo_Clean(t)
    
    t$INTPTLAT = as.numeric(as.character(t$INTPTLAT)) # convert lat/lon to num
    t$INTPTLON = as.numeric(as.character(t$INTPTLON))
    tract_list[[i]] = t
    setTxtProgressBar(prog, i)
}

combo = do.call("rbind", c(args = tract_list, makeUniqueIDs = TRUE))

saveRDS(combo, file.path(path_dropbox, "all_national_tracts.rds"))
if (!dir.exists(file.path(path_dropbox, "all_national_tracts"))) dir.create(file.path(path_dropbox, "all_national_tracts"))
writeOGR(combo, dsn = file.path(path_dropbox, "all_national_tracts"), layer = "all_national_tracts", driver = "ESRI Shapefile")
