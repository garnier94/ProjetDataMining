#' groupByGeospatialData
#'
#' Group the station by geographical area
#'
#' @param data trips dataframe grouped by hour (see aggregateData for the format)
#' @param area a Large SpatialPolygonDataFrame containing the different aera . The name of the column containing the area should be SLDLST
#' @param stations a data frame containing the position of the stations (latitude, longitude)
#' @param save_data decide whether to save the data or not
#' @param year the year
#' @return
#'
#' @author Rémy Garnier & Camille Palmier
#' @export
#'

groupByGeospatialData <- function(data, area, stations, year,  save_data = FALSE)
{
  stations.copy <- stations

  # Gestion des cartes
  coordinates(stations) <- ~ longitude + latitude
  proj4string(stations) <- proj4string(illinoisCR)
  stations.copy$district <-as.integer( over(stations, illinoisCR)$SLDLST)

  #Jointure Table
  Datat <- left_join(Data,stations.copy, by=c("station"="id"))
  JoinedData  <- summarise(group_by(Datat,  Time, Day, Hour, district ), nbE = sum(nbE), nbS = sum(nbS))

  #Nb of stations by district
  nb_stations_district <- summarise(group_by(stations.copy, district), nb_stations = n()  )


  JoinedData$diff  <- JoinedData$nbE - JoinedData$nbS

  if(save_data == TRUE)
  {
    save(JoinedData, file=paste("GeographicalData", year,".RData", sep = ""))
  }
  return(list( Joined = JoinedData, nb =  nb_stations_district))
}
