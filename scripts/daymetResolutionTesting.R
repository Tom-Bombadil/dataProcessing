
library(ncdf)
    
# Open the NetCDF     
NCDF <- open.ncdf(file.path('F:/KPONEIL/SourceData/climate/DAYMET/unzipped/Daily/11934_2011/prcp.nc'))    #netcdf
    
    

# Dimension limits of each of the variables we'll use:
# ----------------------------------------------------
start1 = c(1,1)
latcount <- c(NCDF$var$lat$varsize[1], NCDF$var$lat$varsize[2])
loncount <- c(NCDF$var$lon$varsize[1], NCDF$var$lon$varsize[2])
YDcount  <- NCDF$var$yearday$varsize      

start2 = c(1, 1, 1)
varcount = c(NCDF$var$lat$varsize[1], NCDF$var$lat$varsize[2], NCDF$var$yearday$varsize)

# Read in variables:
# ------------------
lat = get.var.ncdf ( nc=NCDF, varid="lat",                 start = start1, count = latcount )
lon = get.var.ncdf ( nc=NCDF, varid="lon",                 start = start1, count = loncount )
dOY = get.var.ncdf ( nc=NCDF, varid="yearday",             start = 1,      count = YDcount  )
var = get.var.ncdf ( nc=NCDF, varid= "prcp",               start = start2, count = varcount )

close.ncdf(NCDF)

# Correction for Daymet doy which starts at 0.
dOY <- dOY + 1  
















    
    
    # Dimension limits of each of the variables
    start1 = c(1,1)
    latcount <- c(NCDF$var$lat$varsize[1], NCDF$var$lat$varsize[2])
    loncount <- c(NCDF$var$lon$varsize[1], NCDF$var$lon$varsize[2])
    
    # Read in variables
    lat = get.var.ncdf ( nc=NCDF, varid="lat", start = start1, count = latcount )
    lon = get.var.ncdf ( nc=NCDF, varid="lon", start = start1, count = loncount )
    close.ncdf(NCDF)
    
    # Join coordinate lists
    tempCoords <- cbind( as.vector(lon), as.vector(lat))
    colnames(tempCoords) <- c("Longitude", "Latitude")
    
    # Generate list
    if (i ==1) {masterCoords <- tempCoords}
    if (i > 1) {masterCoords <- rbind(masterCoords, tempCoords)}
  }
  
  masterCoordsMatrix <- masterCoords
  masterCoords <- as.data.frame(masterCoords)
  
  #----------------------------------------------
  # Loop through catchmentIDs and NetCDFs, getting data.
  #----------------------------------------------  
  
  fids <- unique(record[,c(catchmentID)])
  
  masterLength <- length(delineatedCatchmentsList)
  
  for ( i in seq_along(fids) ){
    
    print(paste0(round(i/length(fids), digits = 3)*100, '% done.'))
    
    # Define the catchment polygon:
    #------------------------------
    featureID <- fids[i] 
    features <- delineatedCatchmentsList[[which(sapply(c(1:masterLength),FUN = function(x){delineatedCatchmentsList[[x]][1]==featureID})==TRUE)]]
    
    catchmentShape <- catchmentShapefile[catchmentShapefile$FEATUREID %in% features,]
    basinShape     <- gUnaryUnion(catchmentShape) #dissolve individual catchments
    a <- SpatialPoints(masterCoords, proj4string=CRS(proj4.NHD))
    
    inside <- as.data.frame(a[!is.na(over(a, basinShape)),])
    
    #If no point falls within the catchment, find the nearest one:
    #-------------------------------------------------------------
    if(nrow(inside) == 0 ){
      
      tempLat <- coordinates(basinShape)[,2]
      tempLon <- coordinates(basinShape)[,1]
      
      distances <- spDistsN1(masterCoordsMatrix, c(tempLon, tempLat), longlat = TRUE)
      minDist <- min(distances)
      distpos <- which(distances == minDist)[1]
      
      nearLon  <- masterCoords[distpos, 1]
      nearLat  <- masterCoords[distpos, 2]
      
      inside[1,1] <- nearLon
      inside[1,2] <- nearLat
    }
    
    #Pull the Tiles for the points within the catchment:
    #---------------------------------------------------
    for(k in 1:length(inside[,1])){
      
      siteLon <- inside[k,1]
      siteLat <- inside[k,2]
      
      #Index the tile by site location:
      tile <- indexDaymetTileByLatLon(siteLat,siteLon)
      
      temp <- data.frame('Longitude' = siteLon, 'Latitude' = siteLat, 'Tile' = tile)
      
      if (k ==1) spatialLocs <- temp
      if (k > 1) spatialLocs <- rbind(spatialLocs, temp)
    }
    rm(siteLat, siteLon)
    
    # Dataframe of coordinates and which tiles they fall into
    spatialLocs <- spatialLocs[ order(spatialLocs$Tile), ]
    subTiles <- unique(spatialLocs$Tile)
    
    
    #Link record with catchment area:
    #--------------------------------
    
    # Pull records for current catchmentID in range of years with data
    curRecord <- record[which(record[,c(catchmentID)] == fids[i]),c(catchmentID, 'year', 'dOY')]
    
    # Years in the record
    recYears <- unique(curRecord$year)
    
    # Don't loop over years with no Daymet data
    loopYears <- recYears[which(recYears >= minDayYr & recYears <= maxDayYr)]
    
    # Order
    loopYears <- loopYears[order(loopYears)]
    
    # First year
    begYr <- min(loopYears)
    
    # Loop through daymet variables
    for (j in 1:length(variables)){
      
      # Loop through record years
      for ( year in loopYears ){
        
        # Loop through tiles
        for ( t in 1:length(subTiles)){
                    
          # Open NetCDF
          NCDF <- open.ncdf(file.path(daymetDirectory, paste0(subTiles[t], '_1980'), paste0(variables[j], '.nc') ) )    #netcdf
          
          # Dimension limits of each of the variables we'll use:
          # ----------------------------------------------------
          start1 = c(1,1)
          latcount <- c(NCDF$var$lat$varsize[1], NCDF$var$lat$varsize[2])
          loncount <- c(NCDF$var$lon$varsize[1], NCDF$var$lon$varsize[2])
          YDcount  <- NCDF$var$yearday$varsize      
          
          start2 = c(1, 1, 1)
          varcount = c(NCDF$var$lat$varsize[1], NCDF$var$lat$varsize[2], NCDF$var$yearday$varsize)
          
          # Read in variables:
          # ------------------
          lat = get.var.ncdf ( nc=NCDF, varid="lat",                 start = start1, count = latcount )
          lon = get.var.ncdf ( nc=NCDF, varid="lon",                 start = start1, count = loncount )
          dOY = get.var.ncdf ( nc=NCDF, varid="yearday",             start = 1,      count = YDcount  )
          var = get.var.ncdf ( nc=NCDF, varid= paste0(variables[j]), start = start2, count = varcount )
          
          close.ncdf(NCDF)
          
          # Correction for Daymet doy which starts at 0.
          dOY <- dOY + 1  
          
          tileCoords <- as.data.frame(cbind( as.vector(lon), as.vector(lat)))
          names(tileCoords) <- c('Lon', 'Lat')
          
          # Which coordinates are in the current tile
          xx <- spatialLocs[which(spatialLocs$Tile == subTiles[t]),]
          
          # Index data for points within the watershed
          for ( m in 1:nrow(xx) ){
            position <- which(lon == xx$Longitude[m] & lat == xx$Latitude[m], arr.in = TRUE)
            
            if ( t == 1 & m == 1) {tempVar <- data.frame(year, dOY, var[position[1], position[2], 1:365])} else(tempVar <- cbind(tempVar, var[position[1], position[2], 1:365]))
          }
          
        } # end tile loop
        
        # Take the average across all points
        ifelse( ncol(tempVar) > 3, R <- rowMeans(tempVar[,-c(1,2)], na.rm = TRUE, dims = 1),  R <- tempVar[,-c(1,2)] )
        
        # Replace with means
        tempVar <- data.frame(tempVar[,c(1,2)], R)
        
        # Name columns
        names(tempVar) <- c("year", "dOY", paste0(variables[j]))
        
        # Store spatial averages for current variable
        if ( year == begYr ) ( mainVar <- tempVar)
        if ( year >  begYr ) ( mainVar <- rbind(mainVar, tempVar))
        
        rm(tempVar, R)
      } # end year loop
      
      # Store spatial average for all variables
      if (j == 1) {allVars <- mainVar} else {allVars <- merge(allVars, mainVar, by = c('year','dOY'), all.x = T)}  
    } # end variable loop
    
    #Add data into main dataframe
    allVars[ ,paste(catchmentID)] <- fids[i]
    
    if (i == 1) {tempRecord <- allVars} else {tempRecord <- rbind(tempRecord, allVars)}
  }
  
  # Merge into original dataframe by catchmentID
  outRecord <- merge(record, tempRecord, by = c(catchmentID, "year", "dOY"), all.x = T, all.y = F, sort = F)
  
  # Rename output
  names(outRecord)[names(outRecord) == 'year']      <- yearCol
  names(outRecord)[names(outRecord) == 'dOY']       <- dOYCol
  if( exists('siteCol') ) { names(outRecord)[names(outRecord) == 'site'] <- siteCol}
  
  # Return record with Daymet variables
  return(outRecord)
}
