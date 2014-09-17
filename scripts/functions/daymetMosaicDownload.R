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
      
      # Don't download if the file already exists
      if(!file.exists(outFile)){
      
        # Download the file
        download.file(url = address, destfile = outFile, quiet = FALSE, mode = 'wb')
        
        # Time download
        end <- proc.time()[3]
        runTime <- (end - beg)/3600
        
        # Print download time
        print(paste0("Download took ", runTime, " hours.") )
        
      } else(print(paste0("File '", outFile ,"' already exists in download directory. Please delete before downloading.")) )

    }# end variable loop
  }# end year loop
}# end function
