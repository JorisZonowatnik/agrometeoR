#' @export
#' @title make a dataset of stations records
#' @author Thomas Goossens
#' @param stations a character specifying the sid's of the stations to use separated by commas
#' @param dfrom a datetime string specifying the dateTime
#' Must have the form "YYYY-MM-DDTHH:MM:SSZ"
#' @param dto a datetime string specifying the dateTime
#' Must have the form "YYYY-MM-DDTHH:MM:SSZ"
#' @param sensor a character specifying the sensor data you want to spatialize.
#' One of tsa, hct, hra
#' @param dynExpl a character vector specifying the dynamic explanatory variables you want to add.
#' Any combinations of inca, ens
#' @return an object of class dataframe containing the desired records
makeTasks <- function(
  stations = paste0(as.character(stations.df$sid), collapse = ","),
  dfrom,
  dto,
  sensor = "tsa",
  dynExpl = NULL
){

  # make an API call to retrieve the dynamic data
  data = typeData(
    getData(dfrom = dfrom, dto = dto, sensors = sensor, sid = stations )
  )

  # remove useless columns
  data = data %>%
    dplyr::select(c("sid", sensor, "mtime"))

  # rename X and Y to x and y for mlr (gstat learner compatibility)
  data = data %>%
    dplyr::rename("x" = "X") %>%
    dplyr::rename("y" = "Y")

}