# Creation of the mock objects that will be used in the tests.
# https://stackoverflow.com/questions/32328802/where-to-put-data-for-automated-tests-with-testthat

#####
## definition of the parameters used in the tests
#####

data("learners")
# data("unit_testing")
#
# # makeDataset inputs
# test_user_token = Sys.getenv("AGROMET_API_V1_KEY")
# test_bad_user_token = "mysterious_token"
# test_dfrom = "2017-03-04T15:00:00Z"
# test_bad_dfrom = "timea"
# test_dto = "2017-03-04T18:00:00Z"
# test_bad_dto = "timeb"
# test_stations = paste0(as.character(stations.df$sid), collapse = ",")
# test_bad_stations = "1,2,3,4,5,8"
# test_sensor = "tsa"
# test_bad_sensor = "foo"
# test_staticExpl = "elevation"
# test_bad_staticExpl = "blah"
# test_grid = grid.df
# test_bad_grid = test_grid[-2]
#
# # makeTasks inputs
# test_dataset = makeDataset(
#   stations = test_stations,
#   user_token = test_user_token,
#   dfrom = test_dfrom,
#   dto = test_dto,
#   sensor = test_sensor,
#   staticExpl = test_staticExpl,
#   json = NULL,
#   dynExpl = NULL)$output$value
#
# # dataset with wrong target variable name
# test_bad_dataset = lapply(test_dataset, function(x){
#   x = dplyr::rename(x, "foo" = "tsa")})
#
# # dataset with some NA values
# test_dataset_with_NA = lapply(test_dataset, function(x){
#   x$tsa[3] = NA
#   return(x)})
#
# # makeModel inputs
# test_tasks = lapply(test_dataset,
#   function(x){
#     makeTask(
#       dataset = x,
#       drop = NULL,
#       target = test_sensor
#     )$output$value
#   })
#
# test_bad_task = NULL
#
# # makeSpatialization inputs
# test_models = lapply(test_tasks,
#   function(x){
#     makeModel(
#       task = x,
#       learner = learners$baseLearners$lrn.lm.alt)$output$value$trained
#   })
#
# # exportSpatialization inputs
# test_spatialized = lapply(test_models,
#   function(x){
#     makeSpatialization(
#       model = x,
#       pred.grid = test_grid)$output$value
#   })
#
# test_bad_spatialized = lapply(test_spatialized,
#   function(x){
#     x[-2]
#   })
#
# # makeBenchmark inputs
# # makeModel inputs
# # test_tasks = makeTasks(
# #   dataset = test_dataset,
# #   drop = NULL,
# #   target = test_sensor)$output$value
# # test_bad_task = NULL
