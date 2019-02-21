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
      task = test_task,
      learner = learners$baseLearners$lrn.lm.alt
    ),
    good.gstat = list(
      task = test_task,
      learner = learners$baseLearners$lrn.gstat.krige
    )),
  bad = list(
    bad_task = list(
      task = test_bad_task,
      learner = learners$baseLearners$lrn.gstat.krige
    ),
    bad_learner = list(
      task = test_task,
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
      expect_named(object, c("bool", "output"))
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
        expect_false(object$bool)
        expect_equal(object$output$condition$type, "error")
        expect_null(object$output$value)
      }
    }
  }
})}

# test3
test_goodInput = function(){test_that("Good behaviour in case of good parameter", {
  for (group in 1:length(groups)) {
    if (names(groups[group]) == "good") {
      for (case in 1:length(group)) {
        object = do.call(what = makeModel, args = groups[[group]][[case]])
        expect_true(object$bool)
        expect_named(object$output$value, c("trained","predictions","perfs"))
        expect_is(object$output$value$trained, class = "WrappedModel")
      }
    }
  }
})}


#####
## execution of the tests. If you want to skip a test, simply comment it :)

test_outputStrucure()
test_badInput()
test_goodInput()
