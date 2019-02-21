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
      dataset = test_dataset[[1]],
      drop = NULL,
      target = "tsa"
      )),
  bad = list(
    bad_colname = list(
      dataset = test_bad_dataset[[1]],
      drop = NULL,
      target = "tsa"
      ),
    bad_param = list(
      dataset = test_dataset[[1]],
      drop = NULL,
      target = "foo"
      )),
  warning = list(
    dataset_with_NA = list(
      dataset = test_dataset_with_NA[[1]],
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
      expect_named(object, c("bool", "output"))
      expect_named(object$output, c("value", "condition", "stations"))
    }
  }
})
}

# test2
test_badInput = function(){test_that("Expected behaviour in case of bad parameter", {
  for (group in 1:length(groups)) {
    if (names(groups[group]) == "bad") {
      for (case in 1:length(group)) {
        object = do.call(what = makeTask, args = groups[[group]][[case]])
        expect_false(object$bool)
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
        expect_true(object$bool)
        expect_is(object$output$value, class = "RegrTask")
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
        expect_equal(object$output$condition$type, "warning")
        expect_equal(class(object$output$condition$message), "character")
        expect_true(object$bool)
        expect_is(object$output$value, class = "RegrTask")
        expect_gte(length(object$output$stations), 1)
      }
    }
  }
})
}


#####
## execution of the tests. If you want to skip a test, simply comment it :)

test_outputStrucure()
test_badInput()
test_goodInput()
test_NA_values()
