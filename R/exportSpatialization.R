#' @export
#' @title export a spatialized object either to csv, json or geojson
#' @description you can both store the encoded string in a variable and in a file by specifying write = TRUE
#' @author Thomas Goossens
#' @importFrom magrittr %>%
#' @param spatialized a dataframe containing the gridded predicted values
#' @param path a character specifying the path where you want your geosonfile to be stored. Default = wd
#' @param filename a character specifying the name you want to give to the file. Default = NULL
#' @param format a character specifying the type of export format. One of "csv", "json" or "geojson". Default = "csv"
#' @param write a boolean specifying if formatted data must be written to file (TRUE) or printed to console (FALSE)
#' @return a list containing a boolean and a character containing the data encoded in the required format. Default = csv
exportSpatialization <- function(
  spatialized,
  path = getwd(),
  filename = NULL,
  format = "csv",
  write = FALSE){

  output = list(value = NULL, condition = list(type = NULL, message = NULL))
  bool = FALSE

  doExportSpatialisation = function(){

    # predicting on the grid
    message("Exporting spatialized data...")

    spatializedNoCoords = spatialized %>%
      dplyr::select(c("px", "response" ,"se"))

    if (format == "csv") {
      message("Encoding data to csv...")
      if (isTRUE(write)) {
        write.csv(data.frame(spatializedNoCoords), paste0(path, "/", filename, ".", format), row.names = FALSE)
        message(paste0("File written to", path, "/", filename, ".", format))
      } else{
        csv.con = textConnection("csv.con", "w")
        write.csv(spatializedNoCoords, csv.con, row.names = FALSE)
        string = textConnectionValue(csv.con)
        #cat(csvString)
        close(csv.con)
        message("Success ! Data encoded")
      }
    }
    if (format == "json") {
      message("Encoding data to json...")
      if (isTRUE(write)) {
        jsonlite::write_json(x = spatializedNoCoords, path = paste0(path, "/", filename, ".", format))
        message(paste0("File written to", path, "/", filename, ".", format))
      } else{
        string = jsonlite::toJSON(spatializedNoCoords)
        #cat(jsonString)
        message("Success ! Data encoded")
      }
    }
    if (format == "geojson") {
      message("Encoding data to geojson...")
      string = geojsonio::geojson_json(spatialized, lat = "Y", lon = "X")
      if (isTRUE(write)) {
        geojsonio::geojson_write(string, file = paste0(path, "/", filename, ".", format))
        message(paste0("File written to", path, "/", filename, ".", format))
      }else{
        #cat(geojsonString)
        message("Success ! Data encoded")
      }
    }
    return(string)
  }
  tryCatch(
    expr = {
      # check if Argument spatialized has class data.frame.
      stopifnot(class(spatialized) == "data.frame")
      # check if Colnames of spatialized argument do match \"px\", \"response\", \"X\", \"Y\
      stopifnot(all(colnames(spatialized) == c("px", "response", "X", "Y")))
      # check if good export format specified
      stopifnot(format %in% c("csv","json","geojson"))
      # check filename specified if write = TRUE
      stopifnot(!isTRUE(write) && is.null(filename))

      # in case everything went fine, do exportSpatialisation
      output$value = doExportSpatialisation()
      output$condition$type = "success"
      output$condition$message = "Dataset created"
      bool = TRUE

    },
    warning = function(w){
      warning = paste0(
        "AgrometeoR::exportSpatialisation. raised a warning -> ",
        w)
      bool <<- TRUE
      output$value <<- doExportSpatialisation()
      output$condition$type <<- "warning"
      output$condition$message <<- warning
    },
    error = function(e){
      error = paste0(
        "AgrometeoR::exportSpatialisation. raised an error -> ",
        e,
        "HINT 1 : check if model has proper class. ",
        "\n",
        "HINT 2 : check if features used to build your model are present in your prediction grid. ",
        "\n")
      output$processing$type <<- "error"
      output$processing$message <<- error
    },
    finally = {
      finalMessage = paste0(
        "exportSpatialisation has encountered a condition of type : ",
        output$condition$type,
        ". \n",
        "All done with exportSpatialisation. "
      )
      message(finalMessage)
      return(list(bool = bool, output = output))
    }
  )
}