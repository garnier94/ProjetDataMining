library(tidyverse)
library(randomForest)

load("~/StatML/DataProjet/AggregatedData/AggratedData2014.RData")
load("~/StatML/DataProjet/AggregatedData/Full_data2016.RData")

DataTrain <- summarise(group_by(Data2014, Day, Hour),nbS = sum(nbS), nbE= sum(nbE))
DataTest <- summarise(group_by(Data2016, Day, Hour, dow),nbS = sum(nbS), nbE= sum(nbE),temp = mean(temp), pluvio =  mean(pluvio))

DataTrain$temp[is.na(DataTrain$temp)] <- 0
rf0 <- randomForest(nbS ~ Day + Hour + temp + pluvio + dow, data= DataTrain)

period = which(DataTest$Day <= "2016-06-25" & DataTest$Day  >= "2016-06-18" )

test <- predict(rf0, newdata = DataTest[period,] )

plot( DataTest$nbE[period], type ='l', col= 'red')

lines( test, type ='l')

