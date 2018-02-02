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

  data("special_days")

  # first column with all the days of the year

  minDate <- as.POSIXct(strptime( paste(year, "01-01", sep = "-"), "%Y-%m-%d"  ) )
  maxDate <- as.POSIXct(strptime( paste(year+1, "01-01", sep = "-"), "%Y-%m-%d"  ) )
  listDate <- seq(minDate, maxDate, by = "day")
  Data <- data.frame(Day = listDate)

  # take the column of special_days corresponding to the year

  special <- special_days[,c(year-2011)]

  # day of the week :

  Data$dow <- as.numeric(format(Data$Day, format = "%u"))
  Data$weekday <- format(Data$Day, format = "%a")

  # creation of the column 2 and 3

  for(i in 1:length(special)){
    positions <- which(Data$Day == special[i])
    Data$dow[positions] <- 8
    Data$weekday[positions] <- as.character(special_days$type_days[i])
  }

  if (save_data == TRUE){
    save(Data, file=paste("SpecialDaysData", year,".RData", sep = ""))
  }

  return(Data)
}
