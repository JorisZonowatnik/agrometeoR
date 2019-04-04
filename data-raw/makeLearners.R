library(mlr)


######
## smart learners names
######

agrometeorLearners = list(
  mulLR_lonLatAlt_NA = makeFilterWrapper(
    learner = makeLearner(
      cl = "regr.lm",
      id = "multiReg.alt_x_y",
      predict.type = 'se'),
    fw.method = "linear.correlation",
    fw.mandatory.feat = c("elevation", "y", "x"),
    fw.abs = 3),
  IDW_lonLat_NA = makeLearner(
    cl = "regr.gstat",
    id = "idw",
    predict.type = "se"),
  NN1_lonLAt_NA = makeFilterWrapper(
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
  OK_lonLat_Range800Psill200000ModelSphNugget0 = makeFilterWrapper(
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
  KED_lonLatAlt_Range800Psill200000ModelSphNugget0 = makeFilterWrapper(
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
    fw.mandatory.feat = c("elevation", "y", "x"),
    fw.abs = 2)
)



######
## base learners
#####

baseLearners = list(
  # multiple linear regression with alt, lat, lon
  lrn.lm.alt = makeFilterWrapper(
    learner = makeLearner(
      cl = "regr.lm",
      id = "multiReg.alt_x_y",
      predict.type = 'se'),
    fw.method = "linear.correlation",
    fw.mandatory.feat = c("elevation", "y", "x"),
    fw.abs = 3),

  # inverse distance weighted
  lrn.gstat.idw = makeLearner(
    cl = "regr.gstat",
    id = "idw",
    predict.type = "se"),

  # ordinary kriging
  lrn.gstat.krige = makeFilterWrapper(
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

  # kriging with alt as external drift
  lrn.gstat.krige.alt = makeFilterWrapper(
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

  # 1 nearest neighbour based on lon lat
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
    fw.abs = 2)
)

#####
## other learners
#####

otherLearners = list(
  # trends order 1
  lrn.gstat.ts1 = makeLearner(
    cl = "regr.gstat",
    id = "ts1",
    par.vals = list(degree = 1, debug.level = 0),
    predict.type = "se"),

  # trends order 2
  lrn.gstat.ts2 = makeLearner(
    cl = "regr.gstat",
    id = "ts2",
    par.vals = list(degree = 2, debug.level = 0),
    predict.type = "se"),

  lrn.gstat.ts3 = makeLearner(
    cl = "regr.gstat",
    id = "ts2",
    par.vals = list(degree = 3, debug.level = 0),
    predict.type = "se"),

  # 2 nearest neighbours based on lon lat
  lrn.gstat.2nn = makeFilterWrapper(
    learner = makeLearner(
      cl = "regr.gstat",
      id = "nn2",
      par.vals = list(
        nmax = 2,
        debug.level = 0),
      predict.type = "se"),
    fw.method = "linear.correlation",
    fw.mandatory.feat = c("y", "x"),
    fw.abs = 2),

  # 5 nearest neighbours based on lon lat
  lrn.gstat.5nn = makeFilterWrapper(
    learner = makeLearner(
      cl = "regr.gstat",
      id = "nn5",
      par.vals = list(
        nmax = 5,
        debug.level = 0),
      predict.type = "se"),
    fw.method = "linear.correlation",
    fw.mandatory.feat = c("y", "x"),
    fw.abs = 2),

  # 5 nearest neighbours multivariate with elevation
  lrn.gstat.5nn.alt = makeFilterWrapper(
    learner = makeLearner(
      cl = "regr.gstat",
      id = "nn5.alt",
      par.vals = list(
        nmax = 5,
        debug.level = 0),
      predict.type = "se"),
    fw.method = "linear.correlation",
    fw.mandatory.feat = c("y", "x", "elevation"),
    fw.abs = 3),

  #  k-nearest neighbours multivariate with elevation for tuning on k
  lrn.gstat.knn.alt = makeFilterWrapper(
    learner = makeLearner(
      cl = "regr.gstat",
      id = "knn.alt",
      par.vals = list(
        debug.level = 0),
      predict.type = "se"),
    fw.method = "linear.correlation",
    fw.mandatory.feat = c("y", "x", "elevation"),
    fw.abs = 3)
)

#####
## Tuning preparation
#####

# parameters set for tuning on OK and KED
ps.gstat.krige = ParamHelpers::makeParamSet(
  ParamHelpers::makeDiscreteParam("range", values = c(800, 1600)),
  ParamHelpers::makeDiscreteParam("psill", values = c(200000)),
  ParamHelpers::makeDiscreteParam("model.manual", values = c("Sph")),
  ParamHelpers::makeDiscreteParam("nugget", values = c(0))
)

# parameters set for tuning on knn
ps.gstat.knn = ParamHelpers::makeParamSet(
  ParamHelpers::makeDiscreteParam("nmax", values = c(1,2,3,4,5,6))
)

ctrl = makeTuneControlGrid()

inner = makeResampleDesc("Holdout")

#####
## tuned learners
#####
tunedLearners = list(
  # ked tuning
  lrn.gstat.krige.alt.tuning = makeTuneWrapper(
    learner = makeFilterWrapper(
        learner = makeLearner(
          cl = "regr.gstat",
          id = "ked",
          predict.type = "se"),
        fw.method = "linear.correlation",
        fw.mandatory.feat = c("y", "x", "elevation"),
        fw.abs = 3),
    resampling = inner,
    measures = rmse,
    par.set = ps.gstat.krige, control = ctrl, show.info = FALSE),
  # ok tuning
  lrn.gstat.krige.tuning = makeTuneWrapper(
    learner = makeFilterWrapper(
      learner = makeLearner(
        cl = "regr.gstat",
        id = "ok",
        predict.type = "se"),
      fw.method = "linear.correlation",
      fw.mandatory.feat = c("y", "x"),
      fw.abs = 2),
    resampling = inner,
    measures = rmse,
    par.set = ps.gstat.krige, control = ctrl, show.info = FALSE),
  #knn tuning
  lrn.gstat.knn.alt.tuning = makeTuneWrapper(
    learner = setLearnerId(otherLearners$lrn.gstat.knn.alt, "knn.alt.tuning"),
    resampling = inner,
    measures = rmse,
    par.set = ps.gstat.knn, control = ctrl, show.info = FALSE)
)

learners = list(baseLearners = baseLearners, otherLearners = otherLearners, tunedLearners = tunedLearners)

devtools::use_data(learners, overwrite = TRUE)
devtools::use_data(agrometeorLearners, overwrite = TRUE)

