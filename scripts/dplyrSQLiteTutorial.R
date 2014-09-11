library(dplyr)
library(RSQLite)


setwd('C:/KPONEIL/GitHub/personal/DATAing/daymet')

## Create the database
#my_db <- src_sqlite("test.sqlite3", create = T)

# Access the existing database
my_db <- src_sqlite("test.sqlite3", create = T)

#src_sqlite(path = 'C:/Users/koneil/DaymetForNHDPlusV2.db', create = FALSE)

data("hflights", package = "hflights")
hflights_sqlite <- copy_to(my_db, hflights, temporary = FALSE, indexes = list(
  c("Year", "Month", "DayofMonth"), "UniqueCarrier", "TailNum"))

hflights_sqlite <- tbl(hflights_sqlite(), "hflights")


select(hflights_sqlite, Year:DayofMonth, DepDelay, ArrDelay)

filter(hflights_sqlite, depDelay > 240)

arrange(hflights_sqlite, Year, Month, DayofMonth)

mutate(hflights_sqlite, speed = AirTime / Distance)




c1 <- filter(hflights_sqlite, DepDelay > 0)
c2 <- select(c1, Year, Month, DayofMonth, UniqueCarrier, DepDelay, AirTime, Distance)
c3 <- mutate(c2, Speed = Distance / AirTime * 60)
c4 <- arrange(c3, Year, Month, DayofMonth, UniqueCarrier)


c4

t <- collect(c4)
