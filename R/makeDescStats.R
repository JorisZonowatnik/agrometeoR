#' @export
#' @title make the descriptive stats of a sensor for a station on a specific period of time
#' @author Thomas Goossens
#' @import mlr
#' @import ggplot2
#' @importFrom magrittr %>%
#' @param records a list of hourly dataframes or a single dataframe ::TODO ::Must contain sunset and sunrise !
#' @param sensor a character vector specifying the name of the sensors you want to get the descriptive statistics
#' @param stations a character vector specifying the sid's of the stations to use
#' @param dfrom a character vector
#' @param dto a character vector
#' @return a list

makeDescStats = function(
  records,
  sensors,
  stations = unique(records$sid),
  dfrom = NULL,
  dto = NULL){


  # Make it a big dataframe if it is a list of dataframes
  if (class(records) == "list") {
    records = records %>%
      purrr::reduce(bind_rows)
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

    probs = c("25", "50", "75")
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
