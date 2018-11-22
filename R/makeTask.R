#' @export
#' @title make a mlr regr task for an hourly/daily set of records
#' @author Thomas Goossens
#' @param stations a character specifying the sid's of the stations to use separated by commas
#' @param dateTime a string specifying the dateTime
#' Must have the form "YYYY-MM-DDTHH:MM:SSZ"
#' @param time a numeric specifying the hour. Must be between 0 and 23
#' @param sensor a character specifying the sensor data you want to spatialize.
#' One of tsa, hct, hra
#' @param dynExpl a character vector specifying the dynamic explanatory variables you want to add.
#' Any combinations of inca, ens
#' @param drop a character vector specifying the explanattory variables you want to drop.
#' Any combinations of "altitude", "elevation", "slope", "aspect", "Agricultural_areas", "Artificials_surfaces", "Forest","Herbaceous_vegetation"
#' @return an object of class list containing an mlr::makeRegrTask() and a sumamry dataframe of the task dataset
makeTask <- function(
  stations = paste0(as.character(stations.df$sid), collapse = ","),
  date,
  time,
  sensor = "tsa",
  dynExpl = NULL,
  drop = c("sid", "altitude", "slope", "aspect", "Agricultural_areas", "Artificials_surfaces", "Forest","Herbaceous_vegetation")
){

  # make an API call to retrieve the dynamic data
  data = typeData(
    getData(dfrom = date, dto = date, sensors = sensor, sid = stations )
  )

  # remove useless columns
  data = data %>%
    dplyr::select(c("sid", sensor))

  # join with explanatoy vars
  data = stations.df %>%
    dplyr::left_join(data, by = "sid")

  # make a machine learning regression taks
  task = mlr::makeRegrTask(
    data = data,
    target = sensor,
    id = date)

  # drop some features
  task = mlr::dropFeatures(
    task = task,
    features = drop)

  # summarise the data
  insights = summary(mlr::getTaskData(task))

  # return the list containing the task and its insights
  return(list(
    task = task,
    insights = insights
  ))

}