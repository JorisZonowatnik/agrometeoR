
library(testthat)
context("Testing makeModel")

groups = list(
  good = list(
    list(
      id = "good.lm",
      task = test_task,
      learner = learners$lrn.lm.alt_x_y
    ),
    list(
      id = "good.gstat",
      task = test_task,
      learner = learners$lrn.gstat.5nn
    )),
  bad = list(
    list(
      id = "bad_task",
      task = test_bad_task,
      learner = learners$lrn.gstat.5nn
    ),
    list(
      id = "bad_learner",
      task = test_task,
      learner = "a_bad_learner"
    )
  ))

test_that("Output has the good structure, whatever error or not", {
  for (group in 1:length(groups)) {
    for (case in 1:length(group)) {
      object = do.call(what = makeModel, args = groups[[group]][[case]][-1])
      expect_is(object, class = "list")
      expect_length(object, 2)
      expect_equal(names(object), c("bool", "output"))
    }
  }
})

test_that("Error are thrown when a bad parameter is passed", {
  for (group in 1:length(groups)) {
    if (names(groups[1]) == "bad") {
      for (case in 1:length(group)) {
        object = do.call(what = makeModel, args = groups[[group]][[case]][-1])
        expect_error(object)
      }
    }
  }
})

test_that("When bool isTRUE (no error), output has class WrappedModel and when bool isFALSE, output has class NULL", {
  for (group in 1:length(groups)) {
    for (case in 1:length(group)) {
      object = do.call(what = makeModel, args = groups[[group]][[case]][-1])
      if (isTRUE(object$bool)) {
        #browser()
        expect_is(object$output$value$trained, "WrappedModel")
      } else {
        expect_null(object$output$value)
      }
    }
  }
})


