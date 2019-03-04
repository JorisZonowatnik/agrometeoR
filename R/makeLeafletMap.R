#' @export
#' @title make an interactive leaflet map of your spatialized data
#' @author Thomas Goossens
#' @import leaflet
#' @import leaflet.extras
#' @param target a character specifying the sensor your want to map
#' @param spatialized a dataframe containing the spatialized data and their px reference
#' @param polygon_grid a sf object containing the polygonized grid and the px references
#' @param stations a dataframe containing the stations observations of the sensor
#' @param key a character specifying the key used to match the spatialized dataframe with the polygonized grid
#' @param stations_coords a character vector specifying the name of the coordinates columns of the stations dataframe
#' @param crs a numeric corresponding to the EPSG code of the stations spatial coordinates
#'https://stackoverflow.com/questions/28665918/create-square-polygons-from-single-centre-coordinates-and-area-in-r
# https://github.com/ldavadan/agromet_test/blob/master/R/create_map.

makeLeafletMap = function(
  target,
  spatialized,
  polygon_grid = grid.squares.sf,
  stations,
  key = "px",
  stations_coords = c("x", "y"),
  crs = 3812
){

  # injecting the spatialized data intot the polygon grid ::todo:: check for same nrow
  spatialized = dplyr::left_join(polygon_grid, spatialized, by = key)

  # making the stations data a spatial object
  stations = sf::st_set_crs(sf::st_as_sf(stations, coords = stations_coords), crs)

  # be sure we are in the proper 4326 EPSG => ::TODO:: must be a warning
  spatialized = sf::st_transform(spatialized, 4326)
  stations = sf::st_transform(stations, 4326)

  # to make the map responsive
  responsiveness = "\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'"

  # Sometimes the interpolation and the stations don't have values in the same domain.
  # this lead to mapping inconsistency (transparent color for stations)
  # Thus we create a fullDomain which is a rowbinding of interpolated and original data
  fullDomain = c(spatialized$response, stations[[target]])

  # defining the color palette for the response
  varPal <- leaflet::colorNumeric(
    palette = "RdYlBu", #"RdBl",
    reverse = TRUE,
    domain = fullDomain, #spatialized$response,
    na.color = "black"
  )

  # Definition of the function to create whitening
  alphaPal <- function(color) {
    alpha <- seq(0,1,0.1)
    r <- col2rgb(color, alpha = T)
    r <- t(apply(r, 1, rep, length(alpha)))
    # Apply alpha
    r[4,] <- alpha*255
    r <- r/255.0
    codes <- (rgb(r[1,], r[2,], r[3,], r[4,]))
    return(codes)
  }

  # actually building the map
  prediction.map = leaflet::leaflet(spatialized) %>%
    # basemaps
    addProviderTiles(group = "Stamen",
      providers$Stamen.Toner,
      options = providerTileOptions(opacity = 0.25)
    ) %>%
    addProviderTiles(group = "Satellite",
      providers$Esri.WorldImagery,
      options = providerTileOptions(opacity = 1)
    ) %>%
    # centering the map
    fitBounds(sf::st_bbox(spatialized)[[1]],
      sf::st_bbox(spatialized)[[2]],
      sf::st_bbox(spatialized)[[3]],
      sf::st_bbox(spatialized)[[4]]
    ) %>%
    # adding layer control button
    addLayersControl(baseGroups = c("Stamen", "Satellite"),
      overlayGroups = c("prediction"),
      #overlayGroups = c("prediction", "se", "Stations", "Admin"),
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    # fullscreen button
    leaflet.extras::addFullscreenControl() %>%
    # location button
    addEasyButton(easyButton(
      icon = "fa-crosshairs", title = "Locate Me",
      onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
    htmlwidgets::onRender(paste0("
      function(el, x) {
      $('head').append(",responsiveness,");
      }")
    ) %>%
    # predictions
    addPolygons(
      group = "prediction",
      color = "#444444", stroke = FALSE, weight = 1, smoothFactor = 0.8,
      opacity = 1.0, fillOpacity = 0.8,
      fillColor = ~varPal(response),
      label = ~ paste(
        "prediction:", format(round(spatialized$response, 2), nsmall = 2),
        "se: ", format(round(spatialized$se, 2), nsmall = 2)),
      # popup = ~as.character(response),
      highlightOptions = highlightOptions(color = "white", weight = 2,
        bringToFront = TRUE)
    ) %>%
    addLegend(
      position = "bottomright", pal = varPal, values = ~response,
      title = "prediction",
      group = "prediction",
      opacity = 1
    )

  # if se.bool = TRUE
  # if (!is.null(spatialized$se)) {
  #   uncPal <- leaflet::colorNumeric(
  #     palette = alphaPal("#5af602"),
  #     domain = spatialized$se,
  #     alpha = TRUE
  #   )
  #
  #   prediction.map = prediction.map %>%
  #     addPolygons(
  #       group = "se",
  #       color = "#444444", stroke = FALSE, weight = 1, smoothFactor = 0.5,
  #       opacity = 1.0, fillOpacity = 1,
  #       fillColor = ~uncPal(se),
  #       highlightOptions = highlightOptions(color = "white", weight = 2,
  #         bringToFront = TRUE),
  #       label = ~ paste("prediction:", signif(spatialized$response, 2), "\n","se: ", signif(spatialized$se, 2))
  #     ) %>%
  #     addLegend(
  #       group = "se",
  #       position = "bottomleft", pal = uncPal, values = ~se,
  #       title = "se",
  #       opacity = 1
  #     )
  # }

  # prediction.map = prediction.map %>%
  #   # admin boundaries
  #   addPolygons(
  #     data = wallonia,
  #     group = "Admin",
  #     color = "#444444", weight = 1, smoothFactor = 0.5,
  #     opacity = 1, fillOpacity = 0, fillColor = FALSE) %>%
  #   # stations location
  #   addCircleMarkers(
  #     data = stations,
  #     group = "Stations",
  #     color = "black",
  #     weight = 2,
  #     fillColor = ~varPal(tsa),
  #     stroke = TRUE,
  #     fillOpacity = 1,
  #     label = ~htmltools::htmlEscape(as.character(tsa)))

  return(prediction.map)


}




