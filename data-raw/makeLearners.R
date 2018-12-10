library(mlr)

learners = list(
  # defining the simple learners
  lrn.lm.alt_x_y = makeFilterWrapper(
    learner = makeLearner(
      cl = "regr.lm",
      id = "multiReg.alt_x_y",
      predict.type = 'se'),
    fw.method = "linear.correlation",
    fw.mandatory.feat = c("elevation", "y", "x"),
    fw.abs = 3),

  # this learner does not support missing values. So we input these
  # lrn.lm.alt_x_y = makeImputeWrapper(
  #   lrn.lm.alt_x_y,
  #   cols = list(tsa_hp1 = imputeMedian()))

  lrn.gstat.idw = makeLearner(
    cl = "regr.gstat",
    id = "idw",
    predict.type = "se"),

  lrn.gstat.ts1 = makeLearner(
    cl = "regr.gstat",
    id = "ts1",
    par.vals = list(degree = 1, debug.level = 0),
    predict.type = "se"),

  lrn.gstat.ts2 = makeLearner(
    cl = "regr.gstat",
    id = "ts2",
    par.vals = list(degree = 2, debug.level = 0),
    predict.type = "se"),

  lrn.gstat.ok = makeFilterWrapper(
    learner = makeLearner(
      cl = "regr.gstat",
      id = "ok",
      par.vals = list(
        range = 800,
        psill = 200000,
        model.manual = "Sph",
        nugget = 0,
        debug.level = 0),
      predict.type = "se"),
    fw.method = "linear.correlation",
    fw.mandatory.feat = c("y", "x"),
    fw.abs = 2),

  lrn.gstat.ked = makeFilterWrapper(
    learner = makeLearner(
      cl = "regr.gstat",
      id = "ked",
      par.vals = list(
        range = 800,
        psill = 200000,
        model.manual = "Sph",
        nugget = 0,
        debug.level = 0),
      predict.type = "se"),
    fw.method = "linear.correlation",
    fw.mandatory.feat = c("y", "x", "elevation"),
    fw.abs = 3),

  lrn.gstat.2nn = makeFilterWrapper(
    learner = makeLearner(
      cl = "regr.gstat",
      id = "nn2",
      par.vals = list(
        set = list(idp = 0),
        nmax = 2,
        debug.level = 0),
      predict.type = "se"),
    fw.method = "linear.correlation",
    fw.mandatory.feat = c("y", "x"),
    fw.abs = 2),

  lrn.gstat.1nn = makeFilterWrapper(
    learner = makeLearner(
      cl = "regr.gstat",
      id = "nn1",
      par.vals = list(
        set = list(idp = 0),
        nmax = 1,
        debug.level = 0),
      predict.type = "se"),
    fw.method = "linear.correlation",
    fw.mandatory.feat = c("y", "x"),
    fw.abs = 2),

  lrn.gstat.5nn = makeFilterWrapper(
    learner = makeLearner(
      cl = "regr.gstat",
      id = "nn5",
      par.vals = list(
        set = list(idp = 0),
        nmax = 5,
        debug.level = 0),
      predict.type = "se"),
    fw.method = "linear.correlation",
    fw.mandatory.feat = c("y", "x"),
    fw.abs = 2)
)


devtools::use_data(learners, overwrite = TRUE)
