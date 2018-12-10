library(parallelMap)
devtools::load_all()

# definition of the tests parameters
user_token = Sys.getenv("AGROMET_API_V1_KEY")
bad_user_token = "ahahaha"
dfrom = ex_dfrom
bad_dfrom = "timea"
dto = ex_dto
bad_dto = "timeb"
stations = paste0(as.character(stations.df$sid), collapse = ",")
bad_stations = "1,2,3,4,5,8"
sensor = "tsa"
bad_sensor = "boum"
staticExpl = "elevation"
bad_staticExpl = "blah"

# makeDataset test
makeDataset.test = makeDataset(
  user_token = user_token,
  stations = stations,
  dfrom = dfrom,
  dto = dto,
  sensor = sensor,
  staticExpl = staticExpl
)

dataset = makeDataset.test$output$value

# makeTasks test
makeTasks.test = makeTasks(
  dataset = dataset,
  target = "tsa"
)

task = makeTasks.test$output$value

# makeBenchmark test
makeBenchmark.test = makeBenchmark(
  tasks = task[[1]],
  learners = learners)

benchmark = makeBenchmark.test$output$value

# makeModel test
makeModel.test = makeModel(
  task = task[[1]],
  learner = learners$lrn.lm.alt_x_y)

response = makeModel.test$output$value$stations_pred
