# creating the objects that will be used in the tests
# all the objects can be loaded with data() as their were created in the data-raw folder of the present package

# definition of the parameters used in the tests
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