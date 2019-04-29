#' @export
#' @title make the descriptive stats of sensors of a set of stations for a specific period of time
#' @author Thomas Goossens
#' @import mlr
#' @importFrom magrittr %>%
#' @param records a list of hourly dataframes or a single dataframe
#' @param sensor a character vector specifying the name of the sensors you want to get the descriptive statistics
#' @param stations a character vector specifying the sid's of the stations to characterize
#' @param dfrom a datetime string specifying the dateTime
#' Must have the form \code{"YYYY-MM-DDTHH:MM:SSZ"}
#' @param dto a datetime string specifying the dateTime
#' Must have the form \code{"YYYY-MM-DDTHH:MM:SSZ"}
#' @return a dataframe

makeStationsSensorStats = function(
  records,
  sensors,
  stations = unique(records$sid),
  dfrom = NULL,
  dto = NULL){


  # Make it a big dataframe if it is a list of dataframes
  if (class(records) == "list") {
    records = records %>%
      purrr::reduce(dplyr::bind_rows)
  }

  # check which sensor are available compared to those wanted
  checkSensors = function(records, sensors){
    sensorsInRecords = colnames(records)[colnames(records) %in% sensors]
    stopifnot(length(sensorsInRecords) > 0)
    if (length(sensorsInRecords) < length(sensors)) {
      warning("agrometeor::makeDescStat warning : Not all the sensors are available in the records.",
        "\n",
        "Here are the missing sensors : ",
        paste(sensors[!sensors %in% sensorsInRecords], collapse = ", "),
        ". ",
        "\n",
        "The analysis will ignore these missing sensors. ")
    }
    sensors = sensorsInRecords
  }

  # check which sensors are available compared to those wanted
  sensors = checkSensors(records, sensors)

  # instancing the sensors list
  sensors = as.list(sensors)
  names(sensors) = sensors

  # Filter the range of records you want to keep according to dfrom and dto
  if (!is.null(dto)) {
    records = records %>%
      dplyr::filter(mtime <= as.POSIXct(dto))
  }
  if (!is.null(dfrom)) {
    records = records %>%
      dplyr::filter(mtime >= as.POSIXct(dfrom))
  }

  # Filter the stations you want to keep
  records = records %>%
    dplyr::filter(sid %in% stations)

  # Add month/date/hour columns
  records = records %>%
    dplyr::mutate(month = as.factor(lubridate::month(mtime))) %>%
    dplyr::mutate(week = as.factor(lubridate::week(mtime))) %>%
    dplyr::mutate(date = as.factor(lubridate::date(mtime))) %>%
    dplyr::mutate(hour = as.factor(lubridate::hour(mtime)))

  # add a dailyMax column if tsa is available
  if("tsa" %in% sensors) {
    records = data.frame(
      records %>%
        dplyr::group_by(sid) %>%
        dplyr::group_by(date) %>%
        dplyr::mutate(daily_max = max(tsa)) %>%
        dplyr::ungroup())
  }


  getSensorDescStats = function(sensor){
    funs = c("sum", "mean", "min", "max", "sd", "var")
    stats = paste0(funs, "(", sensor,", na.rm=TRUE)")
    names(stats) = funs
    as.list(stats)

    probs = c("10", "20", "25", "30", "40", "50", "70", "75", "80", "90")
    quants = paste0("quantile(", sensor, ", probs=.", probs, ", na.rm=TRUE)")
    names(quants) = paste0("q", probs)
    as.list(quants)

    stats = do.call(c, list(stats, quants))

    descStats = records %>%
      dplyr::select_("sid", sensor) %>%
      dplyr::group_by_("sid") %>%
      dplyr::summarise_(.dots = stats)

    return(data.frame(descStats))
  }

  descStatsBySensor = lapply(
    sensors,
    getSensorDescStats)

  return(descStatsBySensor)

}
