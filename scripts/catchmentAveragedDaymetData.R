library(ncdf4)
library(ncdf)
library(maptools)
library(raster)

proj4.NHD  <- "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"

# Catchments shapefile
catchments <- readShapePoly ( "C:/KPONEIL/gis/nhdPlusV2/stateCatchments/MA_Catchment.shp", proj4string=CRS(proj4.NHD))


NCDF <- nc_open('C:/KPONEIL/temporary/dayl_1980.nc4')    #netcdf


# Variable count
start1 = c(1,1)
latcount <- c(NCDF$var$lat$varsize[1], NCDF$var$lat$varsize[2])
loncount <- c(NCDF$var$lon$varsize[1], NCDF$var$lon$varsize[2])
YDcount  <- NCDF$var$yearday$varsize

# Read in variables
lat = ncvar_get ( nc=NCDF, varid="lat", start = start1, count = latcount )
lon = ncvar_get ( nc=NCDF, varid="lon", start = start1, count = loncount )
dOY = ncvar_get ( nc=NCDF, varid="yearday",             start = 1,      count = YDcount  )

# Correction for Daymet doy which starts at 0.
dOY <- dOY + 1  



EXT <- extent(catchments)

matInd <- which(lat >= EXT@ymin & lat <= EXT@ymax & lon >= EXT@xmin & lon <= EXT@xmax, arr.ind = T)

minRow <- min(matInd[,1])
maxRow <- max(matInd[,1])
minCol <- min(matInd[,2])
maxCol <- max(matInd[,2])


countx = maxRow - minRow + 1 
county = maxCol - minCol + 1 

z = ncvar_get( NCDF, "dayl", start=c(minRow,minCol, 1), count=c(countx,county,365) )
tLat = ncvar_get( NCDF, "lat", start=c(minRow,minCol), count=c(countx,county) )
tLon = ncvar_get( NCDF, "lon", start=c(minRow,minCol), count=c(countx,county) )


# Check that the points line up with catchment layer:

plot(tLon, tLat)
polygon(catchments)



catchments <- readShapePoly ( "C:/KPONEIL/gis/nhdPlusV2/stateCatchments/MA_NW.shp", proj4string=CRS(proj4.NHD))
#NCDF <- open.ncdf('F:/KPONEIL/SourceData/climate/DAYMET/unzipped/Daily/11934_1980/dayl.nc')    #netcdf



NetCDF <- NCDF
vraiable <- "dayl"
catchmentShapefile <- catchments
projectionString <- CRS(proj4.NHD)



spatialAverageDaymet2 <- function(NetCDF, variable, catchmentShapefile, projectionString){
  
  # Index the portion of the NetCDF covered by the shapefile
  # --------------------------------------------------------
  # Read in variables
  lat = ncvar_get ( nc=NetCDF, varid="lat", start = start1, count = c(NetCDF$var$lat$varsize[1], NetCDF$var$lat$varsize[2]) )
  lon = ncvar_get ( nc=NetCDF, varid="lon", start = start1, count = c(NetCDF$var$lon$varsize[1], NetCDF$var$lon$varsize[2]) )
 
  # Get the extent of the shapefile
  EXT <- extent(catchments)
  
  # Positions in the array of coordinates within the shapefile extent
  matInd <- which(lat >= EXT@ymin & lat <= EXT@ymax & lon >= EXT@xmin & lon <= EXT@xmax, arr.ind = T)
  
  # Corners of the box in the array
  minRow <- min(matInd[,1])
  maxRow <- max(matInd[,1])
  minCol <- min(matInd[,2])
  maxCol <- max(matInd[,2])
  
  # Number of rows and columns
  countx = maxRow - minRow + 1 
  county = maxCol - minCol + 1 
  
  # Remove the full NetCDF lat/lon coordinates. These will be replaced with the 
  rm(lat,lon)
  
  # Read the variables for the subsetted netcdf
  var = ncvar_get( nc = NetCDF, varid="dayl",    start = c(minRow, minCol, 1), count = c(countx,county,365) )
  lat = ncvar_get( nc = NetCDF, varid="lat",     start = c(minRow, minCol),    count = c(countx,county) )
  lon = ncvar_get( nc = NetCDF, varid="lon",     start = c(minRow, minCol),    count = c(countx,county) )
  dOY = ncvar_get( nc = NetCDF, varid="yearday", start = 1,                    count = NetCDF$var$yearday$varsize  )
  
  # Correction for Daymet doy which starts at 0.
  dOY <- dOY + 1 
  
  
  
  par(mar=c(1,1,1,1))
  plot(catchments)
  
  rect(min(lon), min(lat), max(lon), max(lat))
       
       , density = NULL, angle = 45,
       col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"),
       ...)
  
  
  lines(min(lon), c(min(lat):max(lat)) )
  lines(max(lon), min(lat):max(lat) )
  
  lines(min(lon):max(lon), min(lat) )
  lines(min(lon):max(lon), min(lat) )
  points(lon, lat)
  
  
  
  
  # Prep the points for indexing
  # ----------------------------
  
  # Join coordinate lists
  masterCoords <- cbind( as.vector(lon), as.vector(lat))
  colnames(masterCoords) <- c("Longitude", "Latitude")
  masterCoords       <- as.data.frame(masterCoords)
  masterCoordsMatrix <- as.matrix(masterCoords)
  masterCoordSpPts   <- SpatialPoints(masterCoords, proj4string = projectionString)
  
  # Generate list of catchment centroids
  centroids <- data.frame(catchmentShapefile@data$FEATUREID, coordinates(catchmentShapefile) )
  names(centroids) <- c('FEATUREID', 'LON', 'LAT')
  
  # Overlay points on catchment shapefile
  overPoints <- over(masterCoordSpPts, catchmentShapefile)
  
  # Generate a list of the points and which 
  pointsInside <- overPoints[which(!is.na(overPoints$FEATUREID)),]
  
  # Add the coordinates of the points inside the catchments
  ptsInCoords <- as.data.frame(masterCoordSpPts[as.numeric(row.names(pointsInside))])
  
  pointsInside$Longitude <- ptsInCoords$x
  pointsInside$Latitude  <- ptsInCoords$y
  
  # List of FeatureIDs to average over
  fids <- catchmentShapefile@data$FEATUREID
  
  # Replace with means
  varMeans <- data.frame(dOY = dOY)
  
  for ( i in seq_along(fids) ){
    
    print(paste0(round(i/length(fids), digits = 3)*100, '% done.    ', i))    
    
    inside <- pointsInside[which(pointsInside$FEATUREID == fids[i]),c('Longitude', 'Latitude')]
      
    #If no point falls within the catchment, find the nearest one:
    #-------------------------------------------------------------
    if(nrow(inside) == 0 ){
      
      tempLat <- centroids$LAT[centroids$FEATUREID == fids[i]]
      tempLon <- centroids$LON[centroids$FEATUREID == fids[i]]
      
      distances <- spDistsN1(masterCoordsMatrix, c(tempLon, tempLat), longlat = TRUE)
      minDist <- min(distances)
      distpos <- which(distances == minDist)[1]
      
      nearLon  <- masterCoords[distpos, 1]
      nearLat  <- masterCoords[distpos, 2]
      
      inside[1,1] <- nearLon
      inside[1,2] <- nearLat
    }
        
    # Index data for points within the watershed
    # ------------------------------------------
    for ( m in 1:nrow(inside) ){
      
      # Find th position in the array of the variable
      position <- which(lon == inside$Longitude[m] & lat == inside$Latitude[m], arr.in = TRUE)
      # Pull those values
      indVar <- var[as.numeric(position[,1]), as.numeric(position[,2]), 1:365]
      # Join them in an array
      if (m == 1) {tempVar <- data.frame(dOY, indVar)} else(tempVar <- cbind(tempVar, indVar))
    }
    
    # Which columns to average
    toBeAvg <- which(names(tempVar) == 'indVar')
    # Take the average across all points
    ifelse( length(toBeAvg) > 1, R <- rowMeans(tempVar[,toBeAvg], na.rm = TRUE, dims = 1),  R <- tempVar[,2] )
    
    # Add means to data.frame
    varMeans[,paste(fids[i])] <- R
  }
  
  # Return the records
  return(varMeans)
}


beg <- proc.time()[3]
testOut <- spatialAverageDaymet2(NetCDF = NCDF, variable = "dayl", catchmentShapefile = catchments, projectionString = CRS(proj4.NHD))
end <- proc.time()[3]

runTime <- (end - beg)/3600

# Takes about 4 minutes for one variable for 1 year in MA (11483 catchments)















# Variable count
start1 = c(1,1)
latcount <- c(NetCDF$var$lat$varsize[1], NetCDF$var$lat$varsize[2])
loncount <- c(NetCDF$var$lon$varsize[1], NetCDF$var$lon$varsize[2])
YDcount  <- NetCDF$var$yearday$varsize
varcount <- c(NetCDF$var$lon$varsize[1], NetCDF$var$lon$varsize[2], NetCDF$var$yearday$varsize)
start2 = c(1, 1, 1)  


# Read in variables
lat = get.var.ncdf ( nc=NetCDF, varid="lat", start = start1, count = latcount )
lon = get.var.ncdf ( nc=NetCDF, varid="lon", start = start1, count = loncount )
dOY = get.var.ncdf ( nc=NetCDF, varid="yearday",             start = 1,      count = YDcount  )
var = get.var.ncdf ( nc=NetCDF, varid= paste0(variable), start = start2, count = varcount )



# Correction for Daymet doy which starts at 0.
dOY <- dOY + 1

















testCentroid <- function(NetCDF, variable, catchmentShapefile){  #, projectionString
  
  require(raster)
  
  # Variable count
  start1 = c(1,1)
  latcount <- c(NetCDF$var$lat$varsize[1], NetCDF$var$lat$varsize[2])
  loncount <- c(NetCDF$var$lon$varsize[1], NetCDF$var$lon$varsize[2])
  YDcount  <- NetCDF$var$yearday$varsize
  
  # Read in variables
  lat = ncvar_get ( nc=NetCDF, varid="lat", start = start1, count = latcount )
  lon = ncvar_get ( nc=NetCDF, varid="lon", start = start1, count = loncount )
  dOY = ncvar_get ( nc=NetCDF, varid="yearday",             start = 1,      count = YDcount  )
  
  # Correction for Daymet doy which starts at 0.
  dOY <- dOY + 1  
  
  # Join coordinate lists
  masterCoords <- cbind( as.vector(lon), as.vector(lat))
  colnames(masterCoords) <- c("Longitude", "Latitude")
  masterCoords <- as.data.frame(masterCoords)
  
  EXT <- extent(catchmentShapefile)
  masterCoords <- masterCoords[which(masterCoords$Latitude  >= EXT@ymin & 
                                     masterCoords$Latitude  <= EXT@ymax & 
                                     masterCoords$Longitude >= EXT@xmin & 
                                     masterCoords$Longitude <= EXT@xmax),]
  
  masterCoordsMatrix <- as.matrix(masterCoords)

  centroids <- data.frame(catchmentShapefile@data$FEATUREID, coordinates(catchmentShapefile))
  names(centroids) <- c('FEATUREID', 'Longitude', 'Latitude')
    
  # Replace with means
  varMeans <- data.frame(dOY = dOY)
  
  for ( i in 1:nrow(centroids) ){
    
    print(paste0(round(i/nrow(centroids), digits = 3)*100, '% done.'))
       
    distances <- spDistsN1(masterCoordsMatrix, c(centroids$Longitude[i], centroids$Latitude[i]), longlat = TRUE)
    minDist <- min(distances)
    distpos <- which(distances == minDist)[1]
      
    nearLon  <- masterCoords[distpos, 1]
    nearLat  <- masterCoords[distpos, 2]

    position <- which(lon == nearLon & lat == nearLat, arr.in = TRUE)
      
    start2 = c(as.numeric(position[,1]), as.numeric(position[,2]), 1)
    varcount = c(1, 1, NetCDF$var$yearday$varsize)
      
    var = ncvar_get( nc=NetCDF, varid= paste0(variable), start = start2, count = varcount )

    varMeans[,paste(centroids$FEATUREID[i])] <- var
  }
  
  return(varMeans)
}



beg <- proc.time()[3]
testOut <- testCentroid(NetCDF = NCDF, variable = "dayl", catchmentShapefile = catchments)
end <- proc.time()[3]

runTime <- (end - beg)/3600

# runTime = 0.377 for 10% of MA catchments
# Estimated 3.77 hours for 11483 for 1 variable per year

# Total estimated time: 8700 hours (36 days)



NetCDF <- NCDF
vraiable <- "dayl"
catchmentShapefile <- catchments
projectionString <- CRS(proj4.NHD)



spatialAverageDaymet <- function(NetCDF, variable, catchmentShapefile, projectionString){
  
  # Variable count
  start1 = c(1,1)
  latcount <- c(NetCDF$var$lat$varsize[1], NetCDF$var$lat$varsize[2])
  loncount <- c(NetCDF$var$lon$varsize[1], NetCDF$var$lon$varsize[2])
  YDcount  <- NetCDF$var$yearday$varsize
  
  # Read in variables
  lat = ncvar_get ( nc=NetCDF, varid="lat", start = start1, count = latcount )
  lon = ncvar_get ( nc=NetCDF, varid="lon", start = start1, count = loncount )
  dOY = ncvar_get ( nc=NetCDF, varid="yearday",             start = 1,      count = YDcount  )
  
  # Correction for Daymet doy which starts at 0.
  dOY <- dOY + 1  

  
  # Join coordinate lists
  masterCoords <- cbind( as.vector(lon), as.vector(lat))
  colnames(masterCoords) <- c("Longitude", "Latitude")
  masterCoords <- as.data.frame(masterCoords)
  
  EXT <- extent(catchmentShapefile)
  masterCoords <- masterCoords[which(masterCoords$Latitude  >= EXT@ymin & 
                                       masterCoords$Latitude  <= EXT@ymax & 
                                       masterCoords$Longitude >= EXT@xmin & 
                                       masterCoords$Longitude <= EXT@xmax),]
  
  masterCoordsMatrix <- as.matrix(masterCoords)
  
  a <- SpatialPoints(masterCoords, proj4string = projectionString)
  
  centroids <- data.frame(catchmentShapefile@data$FEATUREID, coordinates(catchmentShapefile) )
  names(centroids) <- c('FEATUREID', 'LON', 'LAT')
  
  #######
  overPoints <- over(a, catchmentShapefile)
  
  pointsInside <- overPoints[which(!is.na(overPoints$FEATUREID)),]
  
  pointsInside$index <- as.numeric(row.names(pointsInside))
  
  fids <- catchmentShapefile@data$FEATUREID
  
  # Replace with means
  varMeans <- data.frame(dOY = dOY)
    
  for ( i in seq_along(fids) ){
    
    print(paste0(round(i/length(fids), digits = 3)*100, '% done.    ', i))    
    
    temp <- pointsInside[which(pointsInside$FEATUREID == fids[i]),]
    
    inside <- as.data.frame(a[temp$index,])
    
    
    #If no point falls within the catchment, find the nearest one:
    #-------------------------------------------------------------
    if(nrow(inside) == 0 ){
            
      tempLat <- centroids$LAT[centroids$FEATUREID == fids[i]]
      tempLon <- centroids$LON[centroids$FEATUREID == fids[i]]
      
      distances <- spDistsN1(masterCoordsMatrix, c(tempLon, tempLat), longlat = TRUE)
      minDist <- min(distances)
      distpos <- which(distances == minDist)[1]
      
      nearLon  <- masterCoords[distpos, 1]
      nearLat  <- masterCoords[distpos, 2]
      
      inside[1,1] <- nearLon
      inside[1,2] <- nearLat
    }
    
    names(inside) <- c('Longitude', 'Latitude')

    # Index data for points within the watershed
    # ------------------------------------------
    for ( m in 1:nrow(inside) ){
      position <- which(lon == inside$Longitude[m] & lat == inside$Latitude[m], arr.in = TRUE)
            
      start2 = c(as.numeric(position[,1]), as.numeric(position[,2]), 1)
      varcount = c(1, 1, NetCDF$var$yearday$varsize)
            
      var = ncvar_get( nc=NetCDF, varid= paste0(variable), start = start2, count = varcount )
            
      if (m == 1) {tempVar <- data.frame(dOY, var)} else(tempVar <- cbind(tempVar, var))
    }
        
    # Take the average across all points
    ifelse( ncol(tempVar) > 2, R <- rowMeans(tempVar[,-1], na.rm = TRUE, dims = 1),  R <- tempVar[,2] )
        
    # Add means to data.frame
    varMeans[,paste(fids[i])] <- R
  }
  
  return(varMeans)
}


beg <- proc.time()[3]
spatialAverageDaymet(NetCDF = NCDF, variable = "dayl", catchmentShapefile = catchments, projectionString = CRS(proj4.NHD))
end <- proc.time()[3]

runTime <- (end - beg)/3600



# runTime = 0.3155667 for 500 catchments
# Estimated 7.247312 hours for 11483 for 1 variable per year

# Total estimated time: 728 days



catchmentShape <- as(catchmentShapefile[catchmentShapefile$FEATUREID %in% fids[1:10],], "SpatialPolygons")


inside <- as.data.frame(a[!is.na(over(a, catchmentShape)),])

#######
overPoints <- over(a, catchmentShapefile)

pointsInside <- overPoints[which(!is.na(overPoints$FEATUREID)),]

pointsInside$index <- as.numeric(row.names(pointsInside))


temp <- pointsInside[which(pointsInside$FEATUREID == fids[i]),]



plotTestPoly <- as(catchmentShapefile[catchmentShapefile$FEATUREID == 5849340,], "SpatialPolygons")


locs <- as.data.frame(a[temp$index,])




a[]temp$index,





catchmentShape <- catchmentShapefile[catchmentShapefile$FEATUREID %in% fids[1:10],]

test <- over(a, catchmentShape)



x <- test[which(!is.na(test$FEATUREID)),]

test2 <- over(catchmentShape, a)






plot(catchmentShape)
label(catchmentShape@data$AreaSqKM)
points(a)


































#######################################################################################
#                              CUT/PASTE WORKSPACE
#######################################################################################
#######################################################################################


#library(raster)
#EXT <- extent(catchmentShape)
#tempCoords <- masterCoords[which(masterCoords$Latitude >= EXT@ymin & masterCoords$Latitude <= EXT@ymax & masterCoords$Longitude >= EXT@xmin & masterCoords$Longitude <= EXT@xmax),]
#b <- SpatialPoints(masterCoords, proj4string=CRS(projectionString))
#inside <- as.data.frame(b[!is.na(over(b, catchmentShape)),])














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






