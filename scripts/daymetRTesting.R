library(maptools)
library(DaymetR)

proj4.NHD  <- "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
catchments <- readShapePoly ( "C:/KPONEIL/gis/MA_Catchment.shp", proj4string=CRS(proj4.NHD))

centroids <- data.frame(FEATUREID = catchments@data$FEATUREID, coordinates(catchments))

beg <- proc.time()[3]
for( i in 1:100){
  get.Daymet(site=paste(centroids$FEATUREID[i]), lat=centroids[i,3], lon=centroids[i,2], start_yr=1980, end_yr=2013,internal=TRUE)
  print(i)
}
end <- proc.time()[3]

runTime <- (end - beg)/3600

# This doesn't seem to be limited by our internet speed @ Conte. It will probably be too time consuming to do it this way.
