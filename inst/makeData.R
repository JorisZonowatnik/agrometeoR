#####
## EXAMPLATIVE DATA & PARAMS TO USE WITH THE FUNCTION
#####

library(agrometeoR)
date = "2017-03-25"
time = 3
learner = mlr::makeLearner(cl = "regr.lm", id = "multiple-linear-reg", predict.type = "se")
makeTaskOutput = makeTask(date = date)
makeModelOutput = makeModel(task = makeTaskOutput$task, learner = learner)
makeSpatialization(
  model = makeModelOutput$model,
  path = paste0(getwd(),"/data-raw/spatialized_example"),
  name = makeTaskOutput$task$task.desc$id)
