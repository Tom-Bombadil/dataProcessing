library(maptools)
library(DaymetR)

proj4.NHD  <- "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
catchments <- readShapePoly ( "C:/KPONEIL/gis/nhdPlusV2/stateCatchments/MA_Catchment.shp", proj4string=CRS(proj4.NHD))

centroids <- data.frame(FEATUREID = catchments@data$FEATUREID, coordinates(catchments))

beg <- proc.time()[3]
for( i in 1:100){
  get.Daymet(site=paste(centroids$FEATUREID[i]), lat=centroids[i,3], lon=centroids[i,2], start_yr=1980, end_yr=2013,internal=TRUE)
  print(i)
}
end <- proc.time()[3]

runTime <- (end - beg)/3600
