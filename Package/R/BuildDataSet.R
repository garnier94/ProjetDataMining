#' Build Data Set
#'
#' Build the full data set
#'
#' @param year the year
#' @param raw_data data coming from Aggregate Data
#' @param districts a Large SpatialPolygonDataFrame containing the different aera . The name of the column containing the area should be SLDLST
#' @param Stations.map a data frame containing the position of the stations (latitude, longitude)
#' @param save_data if TRUE the data are saved in a .RData format with the name "SpecialDays<year>"
#' @return A data frame containing the following information
#'
#' Time : the timestamp of the observation
#' Day : The Day of observation
#' Hour : The Hour of the observation
#' dow : the position of the day in the week (8 for holydays)
#' weekday : name of the day (or of the hollydays)
#' district : the subregion considered
#' nbE : number of bikes arriving in a station of the considered district during the period of 1 hour
#' nbS : number of bikes leaving in a station of the considered district during the period of 1 hour
#' diff : nbE - nbS
#' temp : temperature
#' pluvio : pluviometry
#'
#' @author Rémy Garnier & Camille Palmier
#' @export

BuildDataSet <- function(year, raw_data, districts, Stations, save_data = FALSE)
{
  Comp <- groupByGeospatialData(raw_data, districts, Stations, year, save_data = FALSE)
  nb_stations_by_district <- Comp$nb
  Joined <- Comp$Joined
  Meteo <- getMeteo(year, save_data = FALSE)
  ListDays <- getSpecialDays(year, save_data = FALSE)

  dis <- summarise(group_by(Joined, district))
  minDate <- as.POSIXct(strptime( paste(year, "01-01 01:00", sep = "-"), "%Y-%m-%d %H:%M"  ) )
  maxDate <- as.POSIXct(strptime( paste(year+1, "01-01 01:00", sep = "-"), "%Y-%m-%d %H:%M"  ) )
  listDate <- seq(minDate, maxDate, by = "hour")

  Join <- Joined[,c(1,4)]

  expansion <- expand(Join, Time = listDate, district )
  Data <- left_join(expansion, Joined , by = c("district","Time"))
  Data <- left_join(Data, nb_stations_by_district, by = "district")

  Data$nbE[which(is.na(Data$nbE))] <- 0
  Data$nbS[which(is.na(Data$nbS))] <- 0
  Data$diff <- (Data$nbE -Data$nbS)/Data$nb_stations
  ListDays$Day <- format( ListDays$Day, "%Y-%m-%d")
  Data <- left_join(Data, Meteo, by = "Time")
  Data$Day <-  format(Data$Time, "%Y-%m-%d" )
  Data <- left_join(Data, ListDays, by = "Day")
  Data$Hour <- hour(Data$Time)
  if(save_data == TRUE){
    save(Data, file=paste("FullData", year,".RData", sep = ""))
  }
  return(Data)
}
