#' @export
#' @title export a spatialized object either to json or geojson
#' @author Thomas Goossens
#' @importFrom magrittr %>%
#' @param spatialized a dataframe containing the gridded predicted values
#' @param path a character specifying the path where you want your geosonfile to be stored. Default = wd
#' @param filename a character specifying the name you want to give to the file.
#' @param csv a boolean specifying if data must be exported as csv. Default = FALSE
#' @param json a boolean specifying if data must be exported as json. Default = FALSE
#' @param geojson a boolean specifying if data must be exported as geojson. Default = FALSE
#' @param write a boolean specifying if fomatted data must be written to file (TRUE) or printed to console (FALSE)

exportSpatialization <- function(
  spatialized,
  path = getwd(),
  filename,
  csv = FALSE,
  json = FALSE,
  geojson = FALSE,
  write = FALSE){

  spatializedNoCoords = spatialized %>%
    dplyr::select(c("px", "response" ,"se"))

 if (isTRUE(csv)) {
   if (isTRUE(write)) {
     write.csv(data.frame(spatializedNoCoords), paste0(path, "/", filename, ".csv"), row.names = FALSE)
   } else{
     csv.con = textConnection("csv.con", "w")
     write.csv(spatializedNoCoords, csv.con, row.names = FALSE)
     cat(textConnectionValue(csv.con))
     close(csv.con)
   }
 }

 if (isTRUE(json)) {
   if (isTRUE(write)) {
     jsonlite::write_json(x = spatializedNoCoords, path = paste0(path, "/", filename, ".json"))
   } else{
     cat(spatializedNoCoords)
   }
 }

 if (isTRUE(geojson)) {
   spatialized.geojson = geojsonio::geojson_json(spatialized, lat = "Y", lon = "X")
   if (isTRUE(write)){
     geojsonio::geojson_write(spatialized.geojson, file = paste0(path, "/", filename, ".geojson"))
   } else{
     cat(spatialized.geojson)
   }
 }
}

