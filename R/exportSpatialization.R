#' @export
#' @title export a spatialized object either to csv, json or geojson
#' @description you can both store the encoded string in a variable and in a file by specifying write = TRUE
#' @author Thomas Goossens
#' @importFrom magrittr %>%
#' @param spatialized a dataframe containing the gridded predicted values
#' @param path a character specifying the path where you want your geosonfile to be stored. Default = working directory
#' @param filename a character specifying the name you want to give to the file. If NULL the output is returned as a character. Default = NULL
#' @param format a character specifying the type of export format. One of "csv", "json" or "geojson". Default = "csv"
#' @return A 2 elements named list : \code{snitch} & \code{output}. \cr
#' \code{snitch} is \code{TRUE} if function has provided the expected result. \cr
#' \code{output} is a named list which contains :
#' \itemize{
#'   \item value is a character vector containing the data encoded into the desired exportation format.
#'   \item condition is a character specifying if the function has encountered success, warning, error.
#'   \item message is the message relative to the condition.
#' }
#' @examples
#'\dontrun{
#' # create the dataset
#' myDataset = makeDataset(
#'   dfrom = "2017-03-04T15:00:00Z",
#'   dto = "2017-03-04T18:00:00Z",
#'   sensor = "tsa")
#'
#' # extract the list of hourly sets of records
#' myDataset = myDataset$output$value
#'
#' # create the tasks
#' myTasks = purrr::map(myDataset, makeTask, target = "tsa")
#'
#' # extract the tasks from the outputs
#' myTasks = myTasks %>% purrr::modify_depth(1, ~.$"output"$"value"$"task")
#'
#' # keep the first task
#' myTask = myTasks[[1]]
#'
#' # create the model
#' myModel = makeModel(
#'   task = myTask,
#'   learner = agrometeorLearners$mulLR_lonLatAlt_NA)
#'
#' # extract the relevant information
#' myModel = myModel$output$value
#'
#' # spatialize using the trained model
#' mySpatialization = makeSpatialization(
#' model = myModel$trained,
#' pred.grid = grid.df) # grid.df comes precompiled with the package
#'
#' # get the relevant information
#' mySpatialization = mySpatialization$output$value
#'
#' # export the spatialized data a json as a character returned into myJson variable
#' myJson = exportSpatialization(spatialized = mySpatialization$spatialized, format = "json")
#'
#' # show th json string
#' myJson$output$value
#'
#' # show myJson
#' myJson$output$value
#'
#' # export as a csv file
#' exportSpatialization(spatialized = mySpatialization$spatialized, filename = "test", format = "csv")
#'}
#'
exportSpatialization <- function(
  spatialized,
  path = getwd(),
  filename = NULL,
  format = "csv"){

  output = list(value = NULL, condition = list(type = NULL, message = NULL))
  snitch = FALSE

  doexportSpatialization = function(){

    # predicting on the grid
    message("Exporting spatialized data...")

    spatializedNoCoords = spatialized %>%
      dplyr::select(one_of(c("px", "response" ,"se")))

    if (format == "csv") {
      message("Encoding data to csv...")
      if (!is.null(filename)) {
        write.csv(data.frame(spatializedNoCoords), paste0(path, "/", filename, ".", format), row.names = FALSE)
        message(paste0("File written to", path, "/", filename, ".", format))
      } else{
        csv.con = textConnection("csv.con", "w")
        write.csv(spatializedNoCoords, csv.con, row.names = FALSE)
        string = textConnectionValue(csv.con)
        close(csv.con)
        message("Success ! Data encoded")
        return(string)
      }
    }
    if (format == "json") {
      message("Encoding data to json...")
      if (!is.null(filename)) {
        jsonlite::write_json(x = spatializedNoCoords, path = paste0(path, "/", filename, ".", format))
        message(paste0("File written to", path, "/", filename, ".", format))
      } else{
        string = jsonlite::toJSON(spatializedNoCoords)
        #cat(jsonString)
        message("Success ! Data encoded")
        return(string)
      }
    }
    if (format == "geojson") {
      message("Encoding data to geojson...")
      string = geojsonio::geojson_json(spatialized, lat = "Y", lon = "X")
      if (!is.null(filename)) {
        geojsonio::geojson_write(string, file = paste0(path, "/", filename, ".", format))
        message(paste0("File written to", path, "/", filename, ".", format))
      }else{
        #cat(geojsonString)
        message("Success ! Data encoded")
        return(string)
      }
    }

  }
  tryCatch(
    expr = {
      # check if Argument spatialized has class data.frame.
      stopifnot(class(spatialized) == "data.frame")
      # check if Colnames of spatialized argument do match \"px\", \"response\", \"X\", \"Y\
      stopifnot(all(colnames(spatialized) %in% c("px", "response", "se", "X", "Y")))
      # check if good export format specified
      stopifnot(format %in% c("csv","json","geojson"))

      # in case everything went fine, do exportSpatialization
      output$value = doexportSpatialization()
      output$condition$type = "success"
      output$condition$message = "Dataset created"
      snitch = TRUE

    },
    warning = function(w){
      warning = paste0(
        "AgrometeoR::exportSpatialization. raised a warning -> ",
        w)
      snitch <<- TRUE
      output$value <<- doexportSpatialization()
      output$condition$type <<- "warning"
      output$condition$message <<- warning
    },
    error = function(e){
      error = paste0(
        "AgrometeoR::exportSpatialization. raised an error -> ",
        e,
        "HINT 1 : check if model has proper class. ",
        "\n",
        "HINT 2 : check if features used to build your model are present in your prediction grid. ",
        "\n")
      output$condition$type <<- "error"
      output$condition$message <<- error
    },
    finally = {
      finalMessage = paste0(
        "exportSpatialization has encountered : ",
        output$condition$type,
        ". \n",
        "All done with exportSpatialization. "
      )
      message(finalMessage)
      return(list(snitch = snitch, output = output))
    }
  )
}