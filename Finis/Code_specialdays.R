# Code qui permet d'obtenir la table SpecialDays pour le package 
# A ne pas mettre dans le package !! 

special_2013 = c("2013-01-01","2013-01-21","2013-02-12","2013-02-18","2013-05-27","2013-07-04","2013-09-02","2013-10-14","2013-11-11","2013-11-28","2013-11-29","2013-12-25")
special_2014 = c("2014-01-01","2014-01-20","2014-02-12","2014-02-17","2014-05-26","2014-07-04","2014-09-01","2014-10-13","2014-11-11","2014-11-27","2014-11-28","2014-12-25")
special_2015 = c("2015-01-01","2015-01-19","2015-02-12","2015-02-16","2015-05-25","2015-07-04","2015-09-07","2015-10-12","2015-11-11","2015-11-26","2015-11-27","2015-12-25")
special_2016 = c("2016-01-01","2016-01-18","2016-02-12","2016-02-15","2016-05-30","2016-07-04","2016-09-05","2016-10-10","2016-11-11","2016-11-24","2016-11-25","2016-12-25")
special_2017 = c("2017-01-01","2017-01-16","2017-02-12","2017-02-20","2017-05-29","2017-07-04","2017-09-04","2017-10-09","2017-11-11","2017-11-23","2017-11-24","2017-12-25")

Date <- as.POSIXct(strptime(special_2013,"%Y-%m-%d"))
special_2013 <- Date
rm(Date)

Date <- as.POSIXct(strptime(special_2014,"%Y-%m-%d"))
special_2014 <- Date
rm(Date)

Date <- as.POSIXct(strptime(special_2015,"%Y-%m-%d"))
special_2015 <- Date
rm(Date)

Date <- as.POSIXct(strptime(special_2016,"%Y-%m-%d"))
special_2016 <- Date
rm(Date)

Date <- as.POSIXct(strptime(special_2017,"%Y-%m-%d"))
special_2017 <- Date
rm(Date)

type_days = c("NewYears","MLK Day","Lincoln'sBirthday","PresidentDay","MemorialDay","IndependenceDay","LaborDay","ColumbusDay", "VeteransDay","Thanksgiving","ThanksgivingFriday","ChristmasDay")

special_days <- data.frame(type_days,special_2013,special_2014,special_2015,special_2016,special_2017)

rm(special_2013,special_2014,special_2015,special_2016,special_2017)

devtools::use_data(special_days, pkg="~/Documents/Orsay/M2/Semestre 1/Data Mining/ProjetDataMining/Package")

