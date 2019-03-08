#' @export
#' @title make the descriptive stats of a sensor for a station on a specific period of time
#' @author Thomas Goossens
#' @import mlr
#' @import ggplot2
#' @importFrom magrittr %>%
#' @param dataset a dataframe containing the records from the 2 stations you want to compare. ::TODO ::Must contain sunset and sunrise !
#' @param sensor a character specifying the name of the sensor you want to compare
#' @param sensor a character specifying the name of the sids you want to compare
#' @return a list wihch elements are objects of class mlr::benchmark()

makeDescStats = function(records, sensor, sid, dfrom, dto){

  # Add month/date/hour columns
  records = records %>%
    dplyr::mutate(month = as.factor(lubridate::month(mtime))) %>%
    dplyr::mutate(week = as.factor(lubridate::week(mtime))) %>%
    dplyr::mutate(date = as.factor(lubridate::date(mtime))) %>%
    dplyr::mutate(hour = as.factor(lubridate::hour(mtime)))

  # add a dailyMax column
  records = data.frame(
    datasetDfDayDmax %>%
      dplyr::group_by(sid) %>%
      dplyr::group_by(date) %>%
      dplyr::mutate(daily_max = max(sensor)) %>%
      dplyr::ungroup())

  funs = c("sum", "mean", "min", "max", "sd", "var")
  stats = paste0("round(", funs, "(", sensor,", na.rm=TRUE), 2)")
  names(stats) = funs
  as.list(stats)

  probs = c("25", "50", "75")
  quants = paste0("round(quantile(", sensor, ", probs=.", probs, ", na.rm=TRUE), 2)")
  names(quants) = paste0("q", probs)
  as.list(quants)

  stats = do.call(c, list(stats, quants))

  descStats = records %>% select_("sid", sensor) %>%
    group_by_("sid") %>%
    summarise_(.dots = stats)

  return(data.frame(descStats))
}