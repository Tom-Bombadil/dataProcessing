library(dplyr)
library(hflights)



test <- data.frame(x = c(1,2,3), y = c(4,5,6), z = c(7,8,9))

test <- mutate(test, a = x+y)
arrange(test, x)
filter(test, x > 2)

summarise(test)



group_by(test, x,y)
