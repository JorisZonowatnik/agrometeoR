#' #' @export
#' #' @title make an interactive leaflet map of your sptialized data
#' #' @author Thomas Goossens
#' #' @import leaflet
#' #'
#' #'
#' #'
#' #'https://stackoverflow.com/questions/28665918/create-square-polygons-from-single-centre-coordinates-and-area-in-r
#' stations = mlr::getTaskData(ex_makeTask$output$value$task)
#' spatialized = ex_makeSpatialization$output$value$spatialized
#'
#' # Definition of the function to build a leaflet map for prediction with associated uncertainty
#'
#'   target = "tsa"
#'   grid = grid.df
#'   spatialized
#'   key = "px"
#'   grid.coords = c("X", "Y")
#'   stations.coords = c("x", "y")
#'   crs = 3812
#'
#'   # Find the positions
#'   spatialized = dplyr::inner_join(spatialized, grid.df, by = key)
#'
#'   # make it a spatial object
#'   spatialized = sf::st_set_crs(sf::st_as_sf(spatialized, coords = grid.coords), crs)
#'   stations = sf::st_set_crs(sf::st_as_sf(stations, coords = stations.coords), crs)
#'
#'   # create polygonized prediction data based on the centroids of the grid
#'   bimgrid = sf::st_make_grid(x = spatialized, cellsize = 1000, what = "polygons", offset = min(spatialized[grid.coords[1]]))
#'
#'   # be sure we are in the proper 4326 EPSG
#'   spatialized = sf::st_transform(spatialized, 4326)
#'   stations = sf::st_transform(stations, 4326)
#'
#'   # to make the map responsive
#'   responsiveness = "\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'"
#'
#'   # Sometimes the interpolation and the stations don't have values in the same domain.
#'   # this lead to mapping inconsistency (transparent color for stations)
#'   # Thus we create a fullDomain which is a rowbinding of interpolated and original data
#'   fullDomain = c(spatialized$response, stations$target)
#'
#'   # defining the color palette for the response
#'   varPal <- leaflet::colorNumeric(
#'     palette = "RdYlBu", #"RdBl",
#'     reverse = TRUE,
#'     domain = fullDomain, #data.sf$response,
#'     na.color = "transparent"
#'   )
#'
#'   # Definition of the function to create whitening
#'   alphaPal <- function(color) {
#'     alpha <- seq(0,1,0.1)
#'     r <- col2rgb(color, alpha = T)
#'     r <- t(apply(r, 1, rep, length(alpha)))
#'     # Apply alpha
#'     r[4,] <- alpha*255
#'     r <- r/255.0
#'     codes <- (rgb(r[1,], r[2,], r[3,], r[4,]))
#'     return(codes)
#'   }
#'
#'   # actually building the map
#'   prediction.map = leaflet::leaflet(spatialized) %>%
#'     # basemaps
#'     addProviderTiles(group = "Stamen",
#'       providers$Stamen.Toner,
#'       options = providerTileOptions(opacity = 0.25)
#'     ) %>%
#'     addProviderTiles(group = "Satellite",
#'       providers$Esri.WorldImagery,
#'       options = providerTileOptions(opacity = 1)
#'     ) %>%
#'     # centering the map
#'     fitBounds(sf::st_bbox(spatialized)[[1]],
#'       sf::st_bbox(spatialized)[[2]],
#'       sf::st_bbox(spatialized)[[3]],
#'       sf::st_bbox(spatialized)[[4]]
#'     ) %>%
#'     # adding layer control button
#'     addLayersControl(baseGroups = c("Stamen", "Satellite"),
#'       overlayGroups = c("prediction", "se", "Stations", "Admin"),
#'       options = layersControlOptions(collapsed = TRUE)
#'     ) %>%
#'     # fullscreen button
#'     leaflet.extras::addFullscreenControl() %>%
#'     # location button
#'     addEasyButton(easyButton(
#'       icon = "fa-crosshairs", title = "Locate Me",
#'       onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
#'     htmlwidgets::onRender(paste0("
#'       function(el, x) {
#'       $('head').append(",responsiveness,");
#'       }")
#'     ) %>%
#'     # predictions
#'     addPolygons(
#'       group = "prediction",
#'       color = "#444444", stroke = FALSE, weight = 1, smoothFactor = 0.8,
#'       opacity = 1.0, fillOpacity = 0.9,
#'       fillColor = ~varPal(response),
#'       highlightOptions = highlightOptions(color = "white", weight = 2,
#'         bringToFront = TRUE),
#'       label = ~htmltools::htmlEscape(as.character(response))
#'     ) %>%
#'     addLegend(
#'       position = "bottomright", pal = varPal, values = ~response,
#'       title = "prediction",
#'       group = "prediction",
#'       opacity = 1
#'     )
#'
#'   # if se.bool = TRUE
#'   if (!is.null(data.sf$se)) {
#'     uncPal <- leaflet::colorNumeric(
#'       palette = alphaPal("#5af602"),
#'       domain = data.sf$se,
#'       alpha = TRUE
#'     )
#'
#'     prediction.map = prediction.map %>%
#'       addPolygons(
#'         group = "se",
#'         color = "#444444", stroke = FALSE, weight = 1, smoothFactor = 0.5,
#'         opacity = 1.0, fillOpacity = 1,
#'         fillColor = ~uncPal(se),
#'         highlightOptions = highlightOptions(color = "white", weight = 2,
#'           bringToFront = TRUE),
#'         label = ~ paste("prediction:", signif(data.sf$response, 2), "\n","se: ", signif(data.sf$se, 2))
#'       ) %>%
#'       addLegend(
#'         group = "se",
#'         position = "bottomleft", pal = uncPal, values = ~se,
#'         title = "se",
#'         opacity = 1
#'       )
#'   }
#'
#'   prediction.map = prediction.map %>%
#'     # admin boundaries
#'     addPolygons(
#'       data = borders,
#'       group = "Admin",
#'       color = "#444444", weight = 1, smoothFactor = 0.5,
#'       opacity = 1, fillOpacity = 0, fillColor = FALSE) %>%
#'     # stations location
#'     addCircleMarkers(
#'       data = stations,
#'       group = "Stations",
#'       color = "black",
#'       weight = 2,
#'       fillColor = ~varPal(tsa),
#'       stroke = TRUE,
#'       fillOpacity = 1,
#'       label = ~htmltools::htmlEscape(as.character(tsa)))
#'
#'   return(prediction.map)
#'
