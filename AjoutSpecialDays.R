load("~/Documents/Orsay/M2/Data Mining/ProjetDataMining/AggratedData2013.RData")

# data_frame avec les jours fériés en 2013
# same special days with the function holidayNYSE in the package timeDate
special = c("2013-01-01","2013-01-21","2013-02-12","2013-02-18","2013-05-27","2013-07-04","2013-09-02","2013-10-14","2013-11-11","2013-11-28","2013-11-29","2013-12-25")

Date <- as.POSIXct(strptime(special,"%Y-%m-%d"))
special <- Date
rm(Date)

type_days = c("NewYears","MLK Day","Lincoln'sBirthday","PresidentDay","MemorialDay","IndependenceDay","LaborDay","ColumbusDay", "VeteransDay","Thanksgiving","ThanksgivingFriday","ChristmasDay")

special_days <- data.frame(special, type_days)

# day of the week : 

Data2013perHour$dow = as.numeric(format(Data2013perHour$Day, format = "%u"))
Data2013perHour$weekday = format(Data2013perHour$Day, format = "%a")

for(i in 1:length(special)){
  positions <- which(Data2013perHour$Day == special_days$special[i])
  Data2013perHour$dow[positions] = 8
  Data2013perHour$weekday[positions] = as.character(special_days$type_days[i])
}

# examples
head(Data2013perHour[which(Data2013perHour$Day == special_days$special[10]),],5)
head(Data2013perHour[which(Data2013perHour$Day == special_days$special[7]),],5)

save(Data2013perHour, file="Aggregate_data_special_days.RData")
