
variable <- 'tmin'

year <- '2012'

address <- paste0('ftp://daac.ornl.gov/data/daymet/Daymet_mosaics/data/', variable, '_', year, '.nc4')


beg <- proc.time()[3]
download.file(url = address, destfile = paste0('C:/Users/koneil/Downloads', variable,'_', year, '.nc') )
end <- proc.time()[3]

runTime <- (end - beg)/3600
