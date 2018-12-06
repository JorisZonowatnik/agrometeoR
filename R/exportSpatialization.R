#' @export
#' @title export a spatialized object either to csv, json or geojson
#' @description you can both store the encoded string in a variable and in a file by specifying write = TRUE
#' @author Thomas Goossens
#' @importFrom magrittr %>%
#' @param spatialized a dataframe containing the gridded predicted values
#' @param path a character specifying the path where you want your geosonfile to be stored. Default = wd
#' @param filename a character specifying the name you want to give to the file.
#' @param csv a boolean specifying if data must be exported as csv. Default = FALSE
#' @param json a boolean specifying if data must be exported as json. Default = FALSE
#' @param geojson a boolean specifying if data must be exported as geojson. Default = FALSE
#' @param write a boolean specifying if fomatted data must be written to file (TRUE) or printed to console (FALSE)
#' @return a list containing a boolean and a character containing the data encoded in the required format. Default = csv
exportSpatialization <- function(
  spatialized,
  path = getwd(),
  filename = NULL,
  format = "csv",
  write = FALSE){

  out = tryCatch({

    output = NA
    bool = FALSE

    if (!format %in% c("csv","json","geojson")) {
      message("Bad format specified. Setting format to default .csv format")
      format = "csv"
    }
    if (isTRUE(write) && is.null(filename)) {
      message("Write set to true but no filename specified. Setting filename to 'myfile'")
      filename = "myfile"
    }

    spatializedNoCoords = spatialized %>%
      dplyr::select(c("px", "response" ,"se"))

    if (format == "csv") {
      message("Encoding data to csv...")
      if (isTRUE(write)) {
        write.csv(data.frame(spatializedNoCoords), paste0(path, "/", filename, format), row.names = FALSE)
        message(paste0("File written to", path, "/", filename, format))
      } else{
        csv.con = textConnection("csv.con", "w")
        write.csv(spatializedNoCoords, csv.con, row.names = FALSE)
        output = textConnectionValue(csv.con)
        #cat(csvString)
        close(csv.con)
        message("Success ! Data encoded")
        bool = TRUE
      }
    }

    if (format == "json") {
      message("Encoding data to json...")
      if (isTRUE(write)) {
        jsonlite::write_json(x = spatializedNoCoords, path = paste0(path, "/", filename, format))
        message(paste0("File written to", path, "/", filename, format))
      } else{
        output = jsonlite::toJSON(spatializedNoCoords)
        #cat(jsonString)
        message("Success ! Data encoded")
        bool = TRUE
      }
    }
    if (format == "geojson") {
      message("Encoding data to geojson...")
      output = geojsonio::geojson_json(spatialized, lat = "Y", lon = "X")
      if (isTRUE(write)) {
        geojsonio::geojson_write(output, file = paste0(path, "/", filename, ".geojson"))
        message(paste0("File written to", path, "/", filename, format))
      }else{
        #cat(geojsonString)
        message("Success ! Data encoded")
        bool = TRUE
      }
    }
  },
    error = function(err){
      message("AgrometeoR Error : exportSpatialization failed. Here is the original error message : ")
      message(paste0(err, "\n"))
      message("Setting value of output to NA")
      return(list(bool, output))
    },
    warning = function(cond){
      message(cond)
    },
    finally = {
      return(list(bool = bool, output = output))
    })
  return(out)
}