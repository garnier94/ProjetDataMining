#' aggregateData
#'
#' aggregate the raw Divvy_Bike_Data in a Data_Frame with aggregated parameter
#'
#' @param trips the raw data
#' @param year the predictions
#' @return
#'
#' @author Rémy Garnier
#' @export
aggregateData  <- function(trips, year )
{
  if(year == 2017 )
    {
      Date <- as.POSIXct(strptime(trips$start_time,"%m/%d/%Y %H:%M"))
    }
  else
    {
      Date <- as.POSIXct(strptime(trips$starttime,"%m/%d/%Y %H:%M"))
    }
  trips$starttime <- Date
  rm(Date)

  if(year == 2017 )
  {
    Date <- as.POSIXct(strptime(trips$stoptime,"%m/%d/%Y %H:%M"))
  }
  else
  {
    Date <- as.POSIXct(strptime(trips$end_time,"%m/%d/%Y %H:%M"))
  }
  trips$stoptime <- Date
  rm(Date)

  minDate = floor_date(min(trips$starttime), unit= "hour")
  maxDate = as.POSIXct(strptime( paste(year, "12-31 23:00", sep = "-"), "%Y-%m-%d %H:%M"  ) )
  listDate = seq(minDate, maxDate, by = "hour")

  Data <- mutate(trips, Hour = hour(trips$starttime), Time = floor_date(trips$starttime, unit = "hour"))
  Data2 <- mutate(trips, Hour = hour(trips$stoptime), Time = floor_date(trips$stoptime, unit = "hour"))
  Data2 <- Data2[which(year(Data2$Time)== year),] # On élimine les données supérieures à l'année considérée

  TrajetStationEntrant <- summarise(group_by(Data2, Time, Hour,station = to_station_id),nbE=n())
  TrajetStationSortant <- summarise(group_by(Data, Time, Hour,station = from_station_id),nbS=n())

  Data <- full_join(TrajetStationEntrant, TrajetStationSortant, by = c("Time","station","Hour" ))
  Data <- mutate(Data, Day = format(Time, "%Y-%m-%d" ))

  Data[is.na(Data)] <- 0
  Data$nbE[which(is.na(Data$nbE))] <- 0
  Data$nbS[which(is.na(Data$nbS))] <- 0

  dftime = data.frame(Time = listDate)
  TimeValues = summarise(group_by(Data,Time), ind = 1)
  difftable =  left_join(dftime, TimeValues, by = "Time")
  MissingValues <- data.frame( Time = difftable$Time[which(is.na(difftable$ind))])
  MissingValues <- mutate(MissingValues, Day = format(Time, "%Y-%m-%d" ), Hour = hour(Time) )
  save(Data, MissingValues, file=paste(  "AggratedData", year,".RData", sep = ""))
}



