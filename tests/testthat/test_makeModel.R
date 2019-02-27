#####
## unit tests for makeModel
## to run these tests, use : devtools::test(filter= "makeModel")
## in case you want to debug on of these tests, place browser() where yo uwant the code execution to stop

#####
## library and context.
library(testthat)
context("Testing makeModel")

#####
## definition of the various function inputs that are tested.
## the objects used in these function inputs definition comes precompiled. See folder data-raw for their source file

groups = list(
  good = list(
    good.lm = list(
      task = ex_makeTask$output$value$task,
      learner = learners$baseLearners$lrn.lm.alt
    ),
    good.gstat = list(
      task = ex_makeTask$output$value$task,
      learner = learners$baseLearners$lrn.gstat.krige
    )),
  bad = list(
    bad_task = list(
      task = ex_bad_makeTask,
      learner = learners$baseLearners$lrn.gstat.krige
    ),
    bad_learner = list(
      task = ex_makeTask$output$value$task,
      learner = "a_bad_learner"
    )
  ))

#####
## definition of the unit tests

# test1
test_outputStrucure = function(){test_that("Output has the good structure whatever the inputs", {
  for (group in 1:length(groups)) {
    for (case in 1:length(group)) {
      object = do.call(what = makeModel, args = groups[[group]][[case]])

      expect_is(object, class = "list")
      expect_named(object, c("snitch", "output"))
      expect_named(object$output, c("value", "condition"))
    }
  }
})
}

# test2
test_badInput = function(){test_that("Good behaviour in case of bad parameter", {
  for (group in 1:length(groups)) {
    if (names(groups[group]) == "bad") {
      for (case in 1:length(group)) {
        object = do.call(what = makeModel, args = groups[[group]][[case]])

        expect_false(object$snitch)
        expect_equal(object$output$condition$type, "error")
        expect_null(object$output$value)
      }
    }
  }
})}

# test3
test_goodInput = function(){test_that("Good behaviour in case of good parameters", {
  for (group in 1:length(groups)) {
    if (names(groups[group]) == "good") {
      for (case in 1:length(group)) {
        object = do.call(what = makeModel, args = groups[[group]][[case]])

        # the snitch is at TRUE
        expect_true(object$snitch)
        # the returned object at slot value is a named list
        expect_is(object$output$value, class = "list")
        # with names "trained","predictions","perfs"
        expect_named(object$output$value, c("trained","predictions","perfs"))
        # and its trained slot is of an object of class WrappedModel
        expect_is(object$output$value$trained, class = "WrappedModel")
        # the object at slot value predictions is of class data.frame
        expect_is(object$output$value$predictions, class = "data.frame")
        # the colnames of the predictions dataframe are truth, response, se and residuals
        expect_named(object$output$value$predictions, c("truth", "response", "se", "residuals"))
        # the size of the predictions dataframe is equal to the size of the input task
        expect_equal(mlr::getTaskSize(ex_makeTask$output$value$task), nrow(object$output$value$predictions))
      }
    }
  }
})}


#####
## execution of the tests. If you want to skip a test, simply comment it :)

test_outputStrucure()
test_badInput()
test_goodInput()
