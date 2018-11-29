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
  path,
  filename,
  csv = FALSE,
  json = FALSE,
  geojson = FALSE,
  write = FALSE){

  # convert to spatial object to change CRS
 spatialized = spatialized %>%
    sf::st_as_sf(coords = c("x","y"))

  # set crs
 spatialized = spatialized %>%
    sf::st_set_crs(3812)

  # convert to CRS = 4326 (geojson standard)
 spatialized = spatialized %>%
    sf::st_transform(4326)

 if (isTRUE(csv)) {
   coords = sf::st_coordinates(spatialized,)
   sf::st_geometry(spatialized) = NULL
   spatialized = spatialized %>%
     dplyr::bind_cols(data.frame(coords))
   if (isTRUE(write)) {
     write.csv(data.frame(spatialized), paste0(path, "/", filename, ".csv"))
   } else{
     csv.con = textConnection("csv.con", "w")
     write.csv(spatialized, csv.con)
     cat(textConnectionValue(csv.con))
     close(csv.con)
   }
 }

 if (isTRUE(json)) {
   coords = sf::st_coordinates(spatialized)
   sf::st_geometry(spatialized) = NULL
   spatialized = spatialized %>%
     dplyr::bind_cols(coords)
   if (isTRUE(write)) {
     jsonlite::write_json(x = spatialized, path = paste0(path, "/", filename, ".json"))
   } else{
     cat(spatialized)
   }
 }

 if (isTRUE(geojson)) {
   spatialized.geojson = geojsonio::geojson_json(spatialized)
   if (isTRUE(write)){
     geojsonio::geojson_write(spatialized.geojson, path = path, filename = filename)
     # sf::st_write(obj = spatialized, dsn = paste0(path, "/", filename, ".geojson"))
   } else{
     cat(spatialized.geojson)
   }
 }
}

