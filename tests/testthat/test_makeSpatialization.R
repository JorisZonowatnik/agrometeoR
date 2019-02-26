#####
## unit tests for makeSpatialization
## to run these tests, use : devtools::test(filter= "makeSpatialization")
## in case you want to debug on of these tests, place browser() where yo uwant the code execution to stop

#####
## library and context.
library(testthat)
context("Testing makeSpatialization")

#####
## definition of the various function inputs that are tested.
## the objects used in these function inputs definition comes precompiled. See folder data-raw for their source file

groups = list(
  good = list(
    all_good = list(
      model = test_model,
      pred.grid = test_grid
    )),
  bad = list(
    bad_grid = list(
      model = test_model,
      pred.grid = test_bad_grid
    ),
    bad_model = list(
      model = "bad_model",
      pred.grid = test_grid
    )
  ))

#####
## definition of the unit tests

# test1
test_outputStrucure = function(){test_that("Output has the good structure whatever the inputs", {
  for (group in 1:length(groups)) {
    for (case in 1:length(group)) {
      object = do.call(what = makeSpatialization, args = groups[[group]][[case]])
      expect_is(object, class = "list")
      expect_named(object, c("snitch", "output"))
      expect_named(object$output, c("value", "condition"))
    }
  }
})
}

# test2
test_badInput = function(){test_that("Good behaviour in case of bad parameters", {
  for (group in 1:length(groups)) {
    if (names(groups[group]) == "bad") {
      for (case in 1:length(group)) {
        object = do.call(what = makeSpatialization, args = groups[[group]][[case]])
        expect_false(object$snitch)
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
        object = do.call(what = makeSpatialization, args = groups[[group]][[case]])
        expect_true(object$snitch)
        expect_is(object$output$value, "data.frame")
        expect_identical(nrow(object$output$value), nrow(test_grid))
        expect_identical(unique(object$output$value$px), unique(test_grid$px))
      }
    }
  }
})}


#####
## execution of the tests. If you want to skip a test, simply comment it :)

test_outputStrucure()
test_badInput()
test_goodInput()
