#####
## unit tests for exportSpatialization
## to run these tests, use : devtools::test(filter= "exportSpatialization")
## in case you want to debug on of these tests, place browser() where yo uwant the code execution to stop

#####
## library and context.
library(testthat)
context("Testing exportSpatialization")

#####
## definition of the various function inputs that are tested.
## the objects used in these function inputs definition comes precompiled. See folder data-raw for their source file

groups = list(
  good = list(
    all_good = list(
      spatialized = ex_makeSpatialization$output$value$spatialized,
      path = paste0(getwd(), "/test/testthat/exports/"),
      filename = NULL,
      format = "csv"
    )),
  bad = list(
    bad_spatialized = list(
      spatialized = ex_bad_makeSpatialization,
      path = paste0(getwd(), "/test/testthat/exports/"),
      filename = NULL,
      format = "csv"
    ),
    bad_format = list(
      spatialized = ex_makeSpatialization$output$value$spatialized,
      path = paste0(getwd(), "/test/testthat/exports/"),
      filename = NULL,
      format = "xls"
    )
  ))

#####
## definition of the unit tests

# test1
test_outputStrucure = function(){test_that("Output has the good structure whatever the inputs", {
  for (group in 1:length(groups)) {
    for (case in 1:length(group)) {

      object = do.call(what = exportSpatialization, args = groups[[group]][[case]])
      expect_is(object, class = "list")
      expect_named(object, c("snitch", "output"))
      expect_named(object$output, c("value", "condition"))
    }
  }
})}

# test2
test_badInput = function(){test_that("Good behaviour in case of bad parameter", {
  for (group in 1:length(groups)) {
    if (names(groups[group]) == "bad") {
      for (case in 1:length(group)) {
        object = do.call(what = exportSpatialization, args = groups[[group]][[case]])
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

        object = do.call(what = exportSpatialization, args = groups[[group]][[case]])
        expect_true(object$snitch)
        expect_is(object$output$value, "character")
      }
    }
  }
})}


#####
## execution of the tests. If you want to skip a test, simply comment it :)

test_outputStrucure()
test_badInput()
test_goodInput()






