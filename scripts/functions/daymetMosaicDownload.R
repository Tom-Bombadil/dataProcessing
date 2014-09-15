daymetMosaicDownload <- function(startYear, endYear, variables, destinationFolder){
  
  # Loop through years
  for ( year in seq(from = startYear, to = endYear, by = 1) ){
    
    # Loop through variables
    for( var in variables){
      
      beg <- proc.time()[3]
      
      #THREDDS server address
      address <- paste0('http://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1219/', year, '/', var, '_', year, '.nc4')
      
      # Name the output file
      outFile <- file.path(destinationFolder, paste0(var, '_', year, '.nc4'))
      
      # Download the file
      download.file(url = address, destfile = outFile, quiet = FALSE, mode = 'wb')
      
      end <- proc.time()[3]
      runTime <- (end - beg)/3600
      
      print(paste0("Download took ", runTime, " hours.") )
      
    }# end variable loop
  }# end year loop
}# end function
