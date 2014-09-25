library(foreign)
library(maptools)


# Need to go back and determine impoundment area and include it in with upstream distance table...


## Doesn't work for some reason. Skipping for now...
#PROJ4.NHD   <- "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
#CATCHMENTS <- readShapePoly ( "C:/KPONEIL/GitHub/personal/dataProcessing/impoundments/arcFiles/MA_Buffer_NHDFlowlines.shp", proj4string=CRS(PROJ4.NHD))


# Load just the MA catchments to get the list of catchments desired....



reachLengths <- read.dbf ( "C:/KPONEIL/GitHub/personal/dataProcessing/impoundments/arcFiles/MA_Buffer_NHDFlowlines.dbf")
reachLengths <- reachLengths[,c('COMID', 'LENGTHKM')]


dist <- read.dbf('C:/KPONEIL/GitHub/personal/dataProcessing/impoundments/arcFiles/MA_Wetlands_25kmBuf.dbf')

names(dist)[ which(names(dist) == 'RID' )] <- 'FEATUREID'


load('C:/KPONEIL/GitHub/projects/conteStreamTemperature_northeast/dataIn/delineatedCatchments/DelineatedCatchments_NHDPlus_NENY.RData')

# Go through all of these files and rename these objects to the same name to make scripting them easier
delin <- NENYDelineatedCatchments


# Load master covariate data
load('C:/KPONEIL/GitHub/projects/conteStreamTemperature_northeast/dataIn/NENY_CovariateData_2014-06-12.RData')



# Cut off at 25km upstream

#i=impoundment area
#w=Contributing drainage area to impoundment
#d=distance from point of interest to impoundment
#a=impoundment specific coefficient (to be calibrated in model)


DAs <- UpstreamStats [UpstreamStats$FEATUREID %in% COMIDs, c('FEATUREID', 'TotDASqKM') ]

UpstreamStats$FEATUREID


COMIDs <- reachLengths$COMID

for ( i in seq_along(COMIDs) ){
  
  TotDASqKM <- UpstreamStats$TotDASqKM[ UpstreamStats$FEATUREID == COMIDs[i] ]
  
  
  #Line to access delineated catchments list (from Z-stats code)
  
  
  COMIDs[i]
  
  
  
  
  
}


