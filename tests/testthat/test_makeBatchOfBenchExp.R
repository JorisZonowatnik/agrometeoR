#####
## unit tests for makeBatchOfBenchExp
## to run these tests, use : devtools::test(filter= "makeBatchOfBenchExp")
## in case you want to debug on of these tests, place browser() where yo uwant the code execution to stop

#####
## library and context.
library(testthat)
context("Testing makeBatchOfBenchExp")

#####
## definition of the various function inputs that are tested.
## the objects used in these function inputs definition comes precompiled. See folder data-raw for their source file

groups = list(
  good = list(
    all_good = list(
      cpus = 2,
      tasks = test_task,
      learners = learners[c(1, 2, 6, 8)],
      resamplings = "LOO")
  ),
  bad = list(
    bad_ = list(
      cpus = NULL,
      tasks = test_task,
      learners = "learners",
      resamplings = "LOO")))

#####
## definition of the unit tests

# test1
test_outputStrucure = function(){test_that("Output has the good structure whatever the inputs", {
  for (group in 1:length(groups)) {
    for (case in 1:length(group)) {
      object = do.call(what = makeBatchOfBenchExp, args = groups[[group]][[case]])
      expect_is(object, class = "list")
      expect_named(object, c("bool", "output"))
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
        object = do.call(what = makeBatchOfBenchExp, args = groups[[group]][[case]])
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
        object = do.call(what = makeBatchOfBenchExp, args = groups[[group]][[case]])
        expect_true(object$bool)
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


# test_that("Output has the good structure, whatever error or not", {
#   for (group in 1:length(groups)) {
#     for (case in 1:length(group)) {
#       object = do.call(what = makeBenchmark, args = groups[[group]][[case]][-1])
#       expect_is(object, class = "list")
#       expect_length(object, 2)
#       expect_equal(names(object), c("bool", "output"))
#     }
#   }
# })
#
# test_that("Error are thrown when a bad parameter is passed", {
#   for (group in 1:length(groups)) {
#     if (names(groups[1]) == "bad") {
#       for (case in 1:length(group)) {
#         object = do.call(what = makeBenchmark, args = groups[[group]][[case]][-1])
#         expect_error(object)
#       }
#     }
#   }
# })
#
# test_that("When bool isTRUE (no error), output has class list and when bool isFALSE, output has class NULL", {
#   for (group in 1:length(groups)) {
#     for (case in 1:length(group)) {
#       args = groups[[group]][[case]][-1]
#       object = do.call(what = makeBenchmark, args = args)
#       if (isTRUE(object$bool)) {
#         expect_is(object$output$value, "list")
#       } else {
#         expect_null(object$output$value)
#       }
#     }
#   }
# })
#
#
