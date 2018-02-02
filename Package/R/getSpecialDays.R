#' getSpecialDays
#'
#' get a data frame with three colums :
#' first one for the day of the year ;
#' second one with the name of the week or of the special days ;
#' last one with the day of the week where 1 is monday, ... , 7 is sunday and 8 is special day.
#'
#' @param year the year considerate
#' @param save_data if TRUE the data are saved in a .RData format with the name "SpecialDays<year>"
#' @return
#'
#' @author Rémy Garnier & Camille Palmier
#' @export

getSpecialDays <- function(year, save_data = TRUE){




# day of the week : 

Data <- Data2017
special <- special_days$special_2017

Data$dow = as.numeric(format(Data$Day, format = "%u"))
Data$weekday = format(Data$Day, format = "%a")

for(i in 1:length(special)){
  positions <- which(Data$Day == special[i])
  Data$dow[positions] = 8
  Data$weekday[positions] = as.character(special_days$type_days[i])
}


Data2017 <-Data

#save(Data2017, file="Full_data2017.RData")

}
