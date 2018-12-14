# Creation of the objects that will be used in the tests

#####
## definition of the parameters used in the tests
#####

library(tidyverse)

# makeDataset inputs
test_user_token = Sys.getenv("AGROMET_API_V1_KEY")
test_bad_user_token = "mysterious_token"
test_dfrom = "2017-03-04T15:00:00Z"
test_bad_dfrom = "timea"
test_dto = "2017-03-04T15:00:00Z"
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
test_bad_dataset = dplyr::rename(test_dataset, "foo" = "tsa")

# makeModel inputs
test_task = makeTasks(
  dataset = test_dataset,
  drop = NULL,
  target = test_sensor)$output$value[[1]]
test_bad_task = NULL

# makeSpatialization inputs
test_model = makeModel(
  task = test_task,
  learner = learners$lrn.lm.alt_x_y)$output$value$trained

# exportSpatialization inputs
test_spatialized = makeSpatialization(
  model = test_model,
  pred.grid = test_grid)$output$value
test_bad_spatialized = test_spatialized[-2]
