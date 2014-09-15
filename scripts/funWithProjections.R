rm(list = ls())

library(ncdf4)
library(maptools)
library(raster)


#proj4.Daymet     <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

proj4.NHD  <- "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
proj4.Daymet  <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"




# Just spTransform the basic coordinate system info.
#
# Specifically compare them to each other after to make sure it makes sense
#

test2 <- spTransform(catchments, CRS(proj4string(proj4.Daymet)))


NetCDF <- NCDF
# Read in variables
lat = ncvar_get ( nc=NetCDF, varid="lat", start = c(1,1), count = c(NetCDF$var$lat$varsize[1], NetCDF$var$lat$varsize[2]) )
lon = ncvar_get ( nc=NetCDF, varid="lon", start = c(1,1), count = c(NetCDF$var$lon$varsize[1], NetCDF$var$lon$varsize[2]) )

lat <- lat[1:100,1:100]
lon <- lon[1:100,1:100]


masterCoords <- as.data.frame(cbind( as.vector(lon), as.vector(lat)))
coordsNHD   <- SpatialPointsDataFrame(masterCoords, data = masterCoords, proj4string = CRS(proj4.NHD))
coordsDay   <- SpatialPointsDataFrame(masterCoords, data = masterCoords, proj4string = CRS(proj4.Daymet))
coordsTransform <- spTransform(coordsNHD, CRS(proj4.Daymet), method = "SpatialPointsDataFrame")


setwd('C:/KPONEIL/workspace/Rprojections')
writeOGR(coordsNHD,  ".", layer = "coordsNHD", driver = "ESRI Shapefile")
writeOGR(coordsDay,  ".", layer = "coordsDay", driver = "ESRI Shapefile")
writeOGR(coordsTransform,  ".", layer = "coordsTransform", driver = "ESRI Shapefile")















#proj4.NHDLambert <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83  +units=m +no_defs "


# Catchments shapefile
catchments <- readShapePoly ( "C:/KPONEIL/gis/nhdPlusV2/stateCatchments/MA_Catchment.shp", proj4string=CRS(proj4.NHD))

NCDF <- nc_open('C:/KPONEIL/temporary/dayl_1980.nc4')    #netcdf

NCDFTile <- open.ncdf('F:/KPONEIL/SourceData/climate/DAYMET/unzipped/Daily/11934_1980/dayl.nc')    #netcdf

# Read in variables
lat = get.var.ncdf ( nc=NCDFTile, varid="lat", start = c(1,1), count = c(NCDFTile$var$lat$varsize[1], NCDFTile$var$lat$varsize[2]) )
lon = get.var.ncdf ( nc=NCDFTile, varid="lon", start = c(1,1), count = c(NCDFTile$var$lon$varsize[1], NCDFTile$var$lon$varsize[2]) )
  
















xxx <- as(catchments, 'SpatialPolygons')
xxxLCC<- spTransform(xxx,CRS(proj4.Daymet))

# This works
cent <- coordinates(catchments)
location <- SpatialPoints(cent, CRS(proj4string(catchments)))
# convert to Lambert Conformal Conic (LCC)
location_LCC <- spTransform(location,CRS(proj4.Daymet))
plot(location_LCC)
plot(tile_outlines, add = TRUE)


# This doesn't work, despite being the same process in the opposite direction
masterCoords <- cbind( as.vector(lon), as.vector(lat))
masterCoordSpPts   <- SpatialPoints(masterCoords, proj4string = CRS(proj4.Day))

test <- spTransform(masterCoordSpPts, CRS(proj4.NHD))
plot(catchments)
plot(test, add = TRUE)
plot(masterCoordSpPts, add = TRUE)
plot(SpatialPoints(masterCoords, proj4string = CRS(proj4.NHD)), add = TRUE)

test2 <- spTransform(catchments, CRS(proj4string(proj4.Daymet)))






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
  
  # Get the year for output
  YEAR <- ncatt_get( NCDF, varid = 0, attname="start_year")$value
  
  # Index and replace missval
  # -------------------------
  for( h in 1:length(NCDF$var) ){
    if ( NCDF$var[[h]]$name == variable ) {varIndex <- h}
  }
  missingValue <- NCDF$var[[varIndex]]$missval
  
  # Replace
  var <- replace(var, var == missingValue, NA)
  
  # Prep the points for indexing
  # ----------------------------
  
  