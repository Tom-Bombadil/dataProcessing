
daymetMosaicDownload <- function(startYear, endYear, variables, destinationFolder){

  # Loop through years
  for ( year in seq(from = startYear, to = endYear, by = 1) ){
    
    # Loop through variables
    for( var in variables){
    
    #THREDDS server address
    address <- paste0('http://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1219/', year, '/', var, '_', year, '.nc4')
  
    # Name the output file
    outFile <- file.path(destinationFolder, paste0(var, '_', year, '.nc4'))
    
    # Download the file
    download.file(url = address, destfile = paste0('C:/Users/koneil/Downloads/TESTING2.nc4'), quiet = FALSE )
    
    }# end variable loop
  }# end year loop
}# end function


beg <- proc.time()[3]
daymetMosaicDownload(startYear = 1980, 
                     endYear = 2013, 
                     variables = c('tmax', 'tmin', 'prcp', 'dayl', 'srad', 'vp', 'swe'), 
                     destinationFolder = 'F:/KPONEIL/SourceData/climate/DAYMET/mosaics')
end <- proc.time()[3]
runTime <- (end - beg)/3600

