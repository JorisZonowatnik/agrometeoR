data("learners")

# makeDataset inputs
test_user_token = Sys.getenv("AGROMET_API_V1_KEY")
test_bad_user_token = NULL
test_dfrom = "2017-03-04T15:00:00Z"
test_bad_dfrom = "timea"
test_dto = "2017-03-04T18:00:00Z"
test_bad_dto = "timeb"
test_stations = paste0(as.character(stations.df$sid), collapse = ",")
test_bad_stations = "1,2,3,4,5,8"
test_sensor = "tsa"
test_bad_sensor = "foo"
test_staticExpl = "elevation"
test_bad_staticExpl = "blah"
test_grid = grid.df
test_bad_grid = test_grid[-2]

# makeTasks inputs
test_dataset = makeDataset(
  stations = test_stations,
  user_token = test_user_token,
  dfrom = test_dfrom,
  dto = test_dto,
  sensor = test_sensor,
  staticExpl = test_staticExpl,
  json = NULL,
  dynExpl = NULL)$output$value

# dataset with wrong target variable name
test_bad_dataset = lapply(test_dataset, function(x){
  x = dplyr::rename(x, "foo" = "tsa")})

# dataset with some NA values
test_dataset_with_NA = lapply(test_dataset, function(x){
  x$tsa[3] = NA
  return(x)})

# makeModel inputs
test_task = makeTask(
  dataset = test_dataset[[1]],
  drop = NULL,
  target = test_sensor
)$output$value

test_bad_task = NULL

# makeSpatialization inputs
test_model = makeModel(
  task = test_task,
  learner = learners$baseLearners$lrn.lm.alt)$output$value$trained

# exportSpatialization inputs
test_spatialized = makeSpatialization(
  model = test_models,
  pred.grid = test_grid)$output$value

test_bad_spatialized = lapply(test_spatialized,
  function(x){
    x[-2]
  })

devtools::use_data(
  test_user_token,
  test_bad_user_token,
  test_dfrom,
  test_bad_dfrom,
  test_dto,
  test_bad_dto,
  test_stations,
  test_bad_stations,
  test_sensor,
  test_bad_sensor,
  test_staticExpl,
  test_bad_staticExpl,
  test_grid = grid.df,
  test_bad_grid,
  test_dataset,
  test_bad_dataset,
  test_dataset_with_NA,
  test_task,
  test_bad_task,
  test_model,
  test_spatialized,
  test_bad_spatialized,
  overwrite = TRUE,
  internal = TRUE)

# makeBenchmark inputs
# makeModel inputs
# test_tasks = makeTasks(
#   dataset = test_dataset,
#   drop = NULL,
#   target = test_sensor)$output$value
# test_bad_task = NULL


