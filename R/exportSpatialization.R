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
#' @return a character containing the data encoded in the required format. Default = csv
exportSpatialization <- function(
  spatialized,
  path = getwd(),
  filename = NULL,
  format = "csv",
  write = FALSE){

  out = tryCatch({

    if (!format %in% c("csv","json","geojson")) {
      warning("Bad format specified. Setting format to .csv")
      format = "csv"
    }
    if (isTRUE(write) && is.null(filename)) {
      warning("Write set to true but no filename specified. Setting filename to 'myfile'")
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
        csvString = textConnectionValue(csv.con)
        #cat(csvString)
        close(csv.con)
        message("Success ! Data encoded")
        return(csvString)
      }
    }

    if (format == "json") {
      message("Encoding data to json...")
      if (isTRUE(write)) {
        jsonlite::write_json(x = spatializedNoCoords, path = paste0(path, "/", filename, format))
        message(paste0("File written to", path, "/", filename, format))
      } else{
        jsonString = jsonlite::toJSON(spatializedNoCoords)
        #cat(jsonString)
        message("Success ! Data encoded")
        return(jsonString)
      }
    }
    if (format == "geojson") {
      message("Encoding data to geojson...")
      geojsonString = geojsonio::geojson_json(spatialized, lat = "Y", lon = "X")
      if (isTRUE(write)) {
        geojsonio::geojson_write(geojsonString, file = paste0(path, "/", filename, ".geojson"))
        message(paste0("File written to", path, "/", filename, format))
      }else{
        #cat(geojsonString)
        message("Success ! Data encoded")
        return(geojsonString)
      }
    }
  },
    error = function(cond){
      message("AgrometeoR Error : No proper output format specified. Please specify either json, csv or geojson")
      # message(cond)
      return(NA)
    },
    warning = function(cond){
      message(cond)
      # Choose a return value in case of warning
      return(null)
    },
    finally = {


    })
  return(out)
}