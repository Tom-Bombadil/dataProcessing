# Working with writing to and from SQLite

library(dplyr)
library(RSQLite)

setwd('C:/KPONEIL/GitHub/personal/dataProcessing/daymet')



my_db <- src_sqlite("dayTest2", create = T)


# Connecting to a table and writing to it:
db <- dbConnect(SQLite(), "dayTest2")
dbWriteTable(db, "data", testOut, append = FALSE)




# For reading a table (dplyr)
setwd('C:/KPONEIL/GitHub/personal/dataProcessing/daymet')
x1 <- src_sqlite("dayTest2", create = F)
cmon <- tbl(x1, "data")



d <- dbGetQuery(db, "SELECT FROM testing")










testing_sqlite <- copy_to(my_db, testing[1:10,], temporary = FALSE, indexes = list(
  c("year", "dOY")) )





data("hflights", package = "hflights")
hflights_sqlite <- copy_to(my_db, hflights, temporary = FALSE, indexes = list(
  c("Year", "Month", "DayofMonth"), "UniqueCarrier", "TailNum"))

hflights_sqlite <- tbl(hflights_sqlite(), "hflights")


