#####
## unit tests for makeTask
## to run these tests, use : devtools::test(filter= "makeTask")
## in case you want to debug on of these tests, place browser() where yo uwant the code execution to stop

#####
## library and context.
library(testthat)
context("Testing makeTask")

#####
## definition of the various function inputs that are tested.
## the objects used in these function inputs definition comes precompiled. See folder data-raw for their source file

groups = list(
  good = list(
    all_good = list(
      dataset = ex_makeDataset$output$value[[1]],
      drop = NULL,
      target = "tsa"
      )),
  bad = list(
    bad_colname = list(
      dataset = ex_bad_makeDataset[[1]],
      drop = NULL,
      target = "tsa"
      ),
    bad_param = list(
      dataset = ex_makeDataset$output$value[[1]],
      drop = NULL,
      target = "foo"
      )),
  warning = list(
    dataset_with_NA = list(
      dataset = ex_makeDataset_with_NA[[1]],
      drop = NULL,
      target = "tsa"
    ))
)

#####
## definition of the unit tests

# test1
test_outputStrucure = function(){test_that("Output has the good structure whatever the inputs", {
  for (group in 1:length(groups)) {
    for (case in 1:length(group)) {
      object = do.call(what = makeTask, args = groups[[group]][[case]])
      expect_is(object, class = "list")
      expect_named(object, c("snitch", "output"))
      expect_named(object$output, c("value", "condition", "stations"))
      expect_named(object$output$stations, c("used", "ignored"))
    }
  }
})
}

# test2
test_badInput = function(){test_that("Expected behaviour in case of bad parameters", {
  for (group in 1:length(groups)) {
    if (names(groups[group]) == "bad") {
      for (case in 1:length(group)) {
        object = do.call(what = makeTask, args = groups[[group]][[case]])
        expect_false(object$snitch)
        expect_equal(object$output$condition$type, "error")
        expect_null(object$output$value)
      }
    }
  }
})}

# test3
test_goodInput = function(){test_that("Expected behaviour in case of good parameter", {
  for (group in 1:length(groups)) {
    if (names(groups[group]) == "good") {
      for (case in 1:length(group)) {
        object = do.call(what = makeTask, args = groups[[group]][[case]])

        browser()
        # the snitch is at TRUE
        expect_true(object$snitch)
        # the returned object at slot output value as an element of class RegrTask
        expect_is(object$output$value$task, class = "RegrTask")
        # the returned object at slot output stations$used is not NULL and has length >= 1
        expect_is(object$output$stations$used, class = "integer")
        expect_gte(length(object$output$stations$used), 1)
      }
    }
  }
})}

# test4
test_NA_values = function(){test_that("Expected behaviour in case of  NA values", {
  for (group in 1:length(groups)) {
    if (names(groups[group]) == "warning") {
      for (case in 1:length(group)) {
        object = do.call(what = makeTask, args = groups[[group]][[case]])

        # the snitch is at TRUE
        expect_true(object$snitch)
        # the slot condition type is at warning
        expect_equal(object$output$condition$type, "warning")
        # the returned object at slot value is of class list  list
        expect_is(object$output$value, class = "list")
        # the returned object at slot value task is of class RegrTask
        expect_is(object$output$value$task, class = "RegrTask")
        # the returned object at slot value summary is of class data.frame
        expect_is(object$output$value$summary, class = "data.frame")
        # the returned object at slot output stations used is not NULL and has length >= 1
        expect_is(object$output$stations$used, class = "integer")
        expect_gte(length(object$output$stations$used), 1)
        # the returned object at slot output stations ignored is not NULL and has length >= 1
        expect_is(object$output$stations$ignored, class = "integer")
        expect_gte(length(object$output$stations$ignored), 1)
      }
    }
  }
})}


#####
## execution of the tests. If you want to skip a test, simply comment it :)
#
test_outputStrucure()
test_badInput()
test_goodInput()
test_NA_values()
