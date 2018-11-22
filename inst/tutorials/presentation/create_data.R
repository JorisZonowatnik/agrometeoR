# This script is used to generate the FOSS4GBXL2018.RData file which is loaded at app launch

library(devtools)
library(learnr)
library(rgdal)
library(sp)
library(raster)
library(sf)
library(rgeos)
library(fontawesome)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(ggplot2)
library(FNN)
library(agrometAPI)
library(elevatr)
library(rnaturalearth)
library(rnaturalearthhires)
library(kknn)
library(nnet)
library(DiceKriging)
library(bst)
library(mlr)
library(geojsonsf)


# packrat creation trouble https://github.com/rstudio/packrat/issues/236
# Error: package ‘lattice’ is required by ‘gbm’ so will not be detached

# downloading data
rawdata = agrometAPI::get_data(dfrom = "2018-10-01", dto = "2018-10-01")
rawdata = agrometAPI::type_data(rawdata)

# filtering
ourdataset = rawdata %>%
  dplyr::filter(!is.na(mtime)) %>%
  dplyr::filter(sid != 38 & sid != 41) %>%
  dplyr::filter(!is.na(from)) %>%
  dplyr::filter(!is.na(to)) %>%
  dplyr::filter(!is.na(tsa)) %>%
  dplyr::filter(poste != "China") %>%
  dplyr::filter(!type_name %in% c("PS2000","PESSL","BODATA","Sencrop","netdl1000","SYNOP")) %>%
  dplyr::select(c(sid, poste, longitude, latitude, altitude, mtime, tsa))

# making it spatial
ourdataset = sf::st_as_sf(ourdataset,
  coords = c("longitude", "latitude"),
  crs = 4326)
sf::st_crs(ourdataset)


# downloading admin boundaries of Belgium
belgium = sf::st_as_sf((ne_states(country = 'belgium')))
wallonia = belgium %>% dplyr::filter(region == "Walloon")
class(wallonia)
# checking CRS
sf::st_crs(wallonia)

# downloading DEM
# for correspondance between zoom & res : https://mapzen.com/documentation/terrain-tiles/data-sources/#what-is-the-ground-resolution
#load("elevation.RData")
elevation = elevatr::get_elev_raster(as(wallonia, "Spatial"), z = 5, src = "aws")
class(elevation)
# checking CRS
raster::crs(elevation, asText = TRUE)

# croping DEM to wallonia
elevation = raster::mask(elevation, as(wallonia, "Spatial"))
# ploting the elevation raster
plot(elevation)

#  cropping
grid = sf::st_sf(sf::st_make_grid(x = sf::st_transform(wallonia, crs = 3812),  cellsize = 5000, what = "centers"))
grid = sf::st_intersection(grid, sf::st_transform(wallonia, crs = 3812))
grid = sf::st_transform(grid, crs = 4326)
wallonia = sf::st_transform(wallonia, crs = 4326)
plot(grid)
nrow(grid)

# elevation ==> grid extraction
extracted <- raster::extract(
  elevation,
  as(grid,"Spatial"),
  fun = mean,
  na.rm = FALSE,
  df = TRUE
)
colnames(extracted) = c("ID", "altitude")

grid$altitude = extracted$altitude
grid = dplyr::select(grid, altitude)
# grid = dplyr::filter(grid, !is.na(altitude))
plot(grid)
sf::st_crs(grid)

# leaflet
elevation.pal <- colorNumeric(reverse = TRUE, "RdYlGn", values(elevation),
  na.color = "transparent")
temperature.pal <- colorNumeric(reverse = TRUE, "RdBu", domain=ourdataset$tsa,
  na.color = "transparent")
responsiveness = "\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'"

map <- leaflet() %>%
  addProviderTiles(
    providers$OpenStreetMap.BlackAndWhite, group = "B&W") %>%
  addProviderTiles(
    providers$Esri.WorldImagery, group = "Satelitte") %>%
  addRasterImage(
    elevation, group = "Elevation", colors = elevation.pal, opacity = 0.8) %>%
  addPolygons(
    data = wallonia, group = "Admin", color = "#444444", weight = 1, smoothFactor = 0.5,
    opacity = 1, fillOpacity = 0.1, fillColor = "grey") %>%
  addCircleMarkers(
    data = ourdataset,
    group = "Stations",
    color = ~temperature.pal(tsa),
    stroke = FALSE,
    fillOpacity = 0.8,
    label = ~htmltools::htmlEscape(as.character(tsa))) %>%
  addCircleMarkers(
    data = grid,
    group = "Grid",
    radius = 1,
    color = "orange",
    stroke = FALSE, fillOpacity = 1) %>%
  addLegend(
    values = values(elevation), group = "Elevation",
    position = "bottomright", pal = elevation.pal,
    title = "Elevation (m)") %>%
  addLayersControl(
    baseGroups = c("B&W", "Satelitte"),
    overlayGroups = c("Elevation", "Admin", "Stations", "Grid"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  addFullscreenControl()%>%
  hideGroup(c("Slope", "Aspect")) %>%
  addEasyButton(easyButton(
    icon = "fa-crosshairs", title = "Locate Me",
    onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
  htmlwidgets::onRender(paste0("
    function(el, x) {
    $('head').append(",responsiveness,");
    }"))
map


# extract the lon and lat from the sf geometry
coords = data.frame(st_coordinates(ourdataset))
# attributing our original dataset to another var (to avoid overwriting)
ourTask = ourdataset
# converting our dataset from sf to simple df
st_geometry(ourTask) <- NULL
# joining the coords
ourTask = dplyr::bind_cols(ourTask, coords)
# Dropping the non-explanatory features
ourTask = dplyr::select(ourTask, -c(sid, poste, mtime))
# defining our taks
ourTask = mlr::makeRegrTask(id = "FOSS4G_example", data = ourTask, target = "tsa")
# checking our data
head(mlr::getTaskData(ourTask))

# Defining our learners
ourLearners = list(
  l1 = mlr::makeLearner(cl = "regr.lm", id = "linearRegression"),
  l2 = mlr::makeLearner(cl = "regr.fnn", id = "FastNearestNeighbours"),
  l3 = mlr::makeLearner(cl = "regr.nnet", id = "NeuralNetwork", par.vals = list(size = 10)),
  l4 = mlr::makeLearner(cl = "regr.km", id = "Kriging"),
  l5 = mlr::makeLearner(cl = "regr.kknn", id = "KNearestNeighborRegression")
)

# Defining our learners
ourResamplingStrategy = mlr::makeResampleDesc("LOO")

#seting seed to make benchmark reprod
# https://mlr-org.github.io/mlr/articles/tutorial/benchmark_experiments.html
set.seed(5030)
# performing the benchmark of our learners on our task
ourbenchmark = mlr::benchmark(
  learners = ourLearners,
  tasks = ourTask,
  resamplings = ourResamplingStrategy,
  measures = list(rmse)
)

performances = mlr::getBMRAggrPerformances(bmr = ourbenchmark, as.df = TRUE)
performances

# Vizualizing the benchamrk result

plotBMRBoxplots(
  bmr = ourbenchmark,
  measure = rmse,
  order.lrns = mlr::getBMRLearnerIds(ourbenchmark)) +
  aes(color = learner.id)

best.learner = data.frame(performances %>%
    slice(which.min(rmse.test.rmse)))
best.learner

# extract the lon and lat from the sf geometry
coords = data.frame(st_coordinates(grid))
# attributing our original dataset to another var (to avoid overwriting)
ourPredictionGrid = grid
# remove NA values
# ourPredictionGrid = dplyr::filter(!is.na(altitude))
# converting our dataset from sf to simple df
st_geometry(ourPredictionGrid) <- NULL
# joining the coords
ourPredictionGrid = dplyr::bind_cols(ourPredictionGrid, coords)
# removing NA rows from ourPredictionGrid (to avoid mlr bug : https://github.com/mlr-org/mlr/issues/1515)
ourPredictionGrid = dplyr::filter(ourPredictionGrid, !is.na(altitude))
# training the best learner on the dataset
ourModel = mlr::train(
  learner = mlr::getBMRLearners(bmr = ourbenchmark)[[as.character(best.learner$learner.id)]],
  task = ourTask)

# using our model to make the prediction
ourPrediction = predict(
  object = ourModel,
  newdata = ourPredictionGrid
)$data

# injecting the predicted values in the prediction grid
ourPredictedGrid = dplyr::bind_cols(ourPredictionGrid, ourPrediction)

# making the predicted grid a spatial object again
ourPredictedGrid = sf::st_as_sf(ourPredictedGrid, coords = c("X", "Y"), crs = 4326)
plot(ourPredictedGrid)

# injecting data in polygons for better rendering
# https://r-spatial.github.io/sf/reference/st_make_grid.html

sfgrid = st_sf(sf::st_make_grid(x = sf::st_transform(wallonia, 3812),  cellsize = 5000, what = "polygons"))
ourPredictedGrid = sf::st_transform(ourPredictedGrid, crs = 3812)
ourPredictedGrid = sf::st_join(sfgrid, ourPredictedGrid)
ourPredictedGrid = ourPredictedGrid %>%
  dplyr::filter(!is.na(response))

# exporting as jSON object
spatialized = sf::st_transform(ourPredictedGrid, crs = 3812)
spatialized$geometry <- spatialized %>%
  st_centroid() %>%
  st_geometry()

# back to 4326 for geojson standard
spatialized = sf::st_transform(spatialized, crs = 4326)
sf::st_write(obj = spatialized, dsn = "spatialized.geojson")
cat(readLines("spatialized.geojson"), sep = '\n')
file.remove("spatialized.geojson")


# back to 4326 for leaflet
ourPredictedGrid = sf::st_transform(ourPredictedGrid, 4326)

# adding to our map
map2 = map %>%
  addPolygons(
    data = ourPredictedGrid,
    group = "Predictions",
    color = ~temperature.pal(response),
    stroke = FALSE,
    fillOpacity = 0.9,
    label = ~htmltools::htmlEscape(as.character(response))) %>%
  addLegend(
    values = ourPredictedGrid$response,
    group = "Predictions",
    position = "bottomleft", pal = temperature.pal,
    title = "predicted T (°C)") %>%
  addLayersControl(
    baseGroups = c("B&W", "Satelitte"),
    overlayGroups = c("Stations", "Predictions", "Elevation", "Admin", "Grid"),
    options = layersControlOptions(collapsed = TRUE)
  )
map2
