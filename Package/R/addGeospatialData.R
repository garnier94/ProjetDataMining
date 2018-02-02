#' groupByGeospatialData
#'
#' Group the station by geographical area 
#' 
#' @param data trips dataframe grouped by hour (see aggregateData for the format)
#' @param area a Large SpatialPolygonDataFrame containing the different aera . The name of the column containing the area should be SLDLST
#' @param stations a data frame containing the position of the stations (latitude, longitude)
#' @param save_data decide whether to save the data or not
#' @return
#'
#' @author Rémy Garnier & Camille Palmier
#' @export
#' 

groupByGeospatialData <- fun(data, area, stations, save_data = TRUE)
{
  # Gestion des cartes
  coordinates(Stations.map) <- ~ longitude + latitude
  proj4string(Stations.map) <- proj4string(illinoisCR)
  Stations$district <-as.integer( over(Stations.map, illinoisCR)$SLDLST)
  
  #Jointure Table
  Datat <- left_join(Data,Stations, by=c("station"="id"))
  JoinedData  <- summarise(group_by(Datat,  Time, Day, Hour, district ), nbE = sum(nbE), nbS = sum(nbS)  ) 
  JoinedData$diff  <- JoinedData$nbE - JoinedData$nbS
  return(JoinedData)
}