#' @export
#' @title make a mlr regr task for an hourly/daily set of records
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
#' @param drop a character vector specifying the explanattory variables you want to drop.
#' Any combinations of "altitude", "elevation", "slope", "aspect", "Agricultural_areas", "Artificials_surfaces", "Forest","Herbaceous_vegetation"
#' @return an object of class list containing an mlr::makeRegrTask() and a sumamry dataframe of the task dataset
makeTasks <- function(
  stations = paste0(as.character(stations.df$sid), collapse = ","),
  dfrom,
  dto,
  sensor = "tsa",
  dynExpl = NULL,
  drop = c("sid", "altitude", "slope", "aspect", "Agricultural_areas", "Artificials_surfaces", "Forest","Herbaceous_vegetation")
){

  # make an API call to retrieve the dynamic data
  data = typeData(
    getData(dfrom = dfrom, dto = dto, sensors = sensor, sid = stations )
  )

  # remove useless columns
  data = data %>%
    dplyr::select(c("sid", sensor, "mtime"))

  # join with explanatory vars
  data = stations.df %>%
    dplyr::left_join(data, by = "sid")

  # rename X and Y to x and y for mlr (gstat learner compatibility)
  data = data %>%
    dplyr::rename("x" = "X") %>%
    dplyr::rename("y" = "Y")

  # group by mtime and make lists of dataframes
  data = split(data, data$mtime)

  # make machine learning regression tasks for each list element
  tasks = lapply(seq_along(data), function(d)
    mlr::dropFeatures(
      task = mlr::makeRegrTask(
        data = dplyr::select(data[[d]], -mtime),
        target = sensor,
        id = gsub(":", "", names(data)[d])),
      features = drop)
    )
  names(tasks) = gsub(":", "", names(data))

  # summarise the data
  # insights = summary(mlr::getTaskData(task))

  # return the list containing the task and its insights
  return(tasks)

}
