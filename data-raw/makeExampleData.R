# creating function output example datasets for both users and unit_tests + bad_examples for unit_tests only
devtools::load_all()
data("learners")

# makeDataset inputs
ex_user_token = Sys.getenv("AGROMET_API_V1_KEY")
ex_bad_user_token = NULL
ex_dfrom = "2016-02-04T15:00:00Z"
ex_bad_dfrom = "timea"
ex_dto = "2016-02-04T18:00:00Z"
ex_bad_dto = "timeb"

ex_stations = stations.df # created in makeData
ex_stations = unique(ex_stations$sid)
ex_stations_pameseb = ex_stations[1:30]
ex_stations_irm = ex_stations[31:46]
ex_stations = paste0(as.character(ex_stations), collapse = ",")
ex_stations_pameseb = paste0(as.character(ex_stations_pameseb), collapse = ",")
ex_stations_irm = paste0(as.character(ex_stations_irm), collapse = ",")

ex_bad_stations = "1,2,3,4,5,8"
ex_sensor = "tsa"
ex_bad_sensor = "foo"
ex_staticExpl = "elevation"
ex_bad_staticExpl = "blah"
ex_grid = grid.df
ex_bad_grid = ex_grid[-3] # elevation column

# makeTasks inputs
ex_makeDataset = makeDataset(
  stations = ex_stations,
  user_token = ex_user_token,
  dfrom = ex_dfrom,
  dto = ex_dto,
  sensors = ex_sensor,
  staticExpl = ex_staticExpl,
  json = NULL,
  dynExpl = NULL)

# dataset with wrong target variable name
ex_bad_makeDataset = lapply(ex_makeDataset$output$value, function(x){
  x = dplyr::rename(x, "foo" = "tsa")})

# dataset with some NA values
ex_makeDataset_with_NA = lapply(ex_makeDataset$output$value, function(x){
  x$tsa[3] = NA
  return(x)})

# makeModel inputs
ex_makeTask = makeTask(
  dataset = ex_makeDataset$output$value[[1]],
  target = ex_sensor
)

ex_bad_makeTask = NULL

# makeSpatialization inputs
ex_makeModel = makeModel(
  task = ex_makeTask$output$value$task,
  learner = learners$baseLearners$lrn.lm.alt)

# exportSpatialization inputs
ex_makeSpatialization = makeSpatialization(
  model = ex_makeModel$output$value$trained,
  pred.grid = ex_grid)

ex_bad_makeSpatialization = lapply(ex_makeSpatialization,
  function(x){
    x[-2]
  })

# makeBatchOfBenchExp inputs
ex_makeTasks = lapply(
  ex_makeDataset$output$value,
  function(x){
    makeTask(x, target = ex_sensor)$output$value
  })


# saving all the good data object as INTERNAL = FALSE
devtools::use_data(
  ex_user_token,
  ex_dfrom,
  ex_dto,
  ex_stations,
  ex_sensor,
  ex_staticExpl,
  ex_grid = grid.df,
  ex_makeDataset,
  ex_makeTask,
  ex_makeModel,
  ex_makeSpatialization,
  overwrite = TRUE,
  internal = FALSE)

devtools::use_data(
  ex_bad_user_token,
  ex_bad_dfrom,
  ex_bad_dto,
  ex_bad_stations,
  ex_bad_sensor,
  ex_bad_staticExpl,
  ex_bad_grid,
  ex_bad_makeDataset,
  ex_makeDataset_with_NA,
  ex_bad_makeTask,
  ex_bad_makeSpatialization,
  ex_makeTasks,
  overwrite = TRUE,
  internal = TRUE)

# makeBenchmark inputs
# makeModel inputs
# ex_makeTasks = makeTasks(
#   dataset = ex_makeDataset,
#   drop = NULL,
#   target = ex_sensor)$output$value
# ex_bad_makeTask = NULL


