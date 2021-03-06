% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeBmrsBatch.R
\name{makeBmrsBatch}
\alias{makeBmrsBatch}
\title{make a (multicore) mlr benchmark experiment of your learner(s) on your task(s)}
\usage{
makeBmrsBatch(tasks, learners, measures = list(rmse), keep.pred = TRUE,
  models = FALSE, level = "mlr.benchmark", resamplings = "LOO",
  cpus = 4, temp_dir = NULL, prefix = NULL, groupSize = NULL,
  removeTemp = FALSE, crash = FALSE)
}
\arguments{
\item{tasks}{a list which elements are object of class \code{mlr::makeRegrTask()}}

\item{learners}{a list which elements are object of class \code{mlr::makeLearner()}}

\item{measures}{a list of the mlr performance metrics you want to get. Default is \code{mlr::list(rmse)}}

\item{keep.pred}{a boolean specifying if you want to keep the bmr preds. defaut = \code{TRUE}. Necessary to further analyze benchamrk performances}

\item{models}{a boolean specifying if you want to keep the bmr models. defaut = \code{FALSE}}

\item{level}{a character specifying the paralelllization level. Default  = "mlr.benchmark"}

\item{cpus}{an integer specifying the number of cpus to use for the benchamrk. Default is 4}

\item{temp_dir}{a character specifying the path of an exising directory where you want to save the bmr temporary outpus.}

\item{prefix}{a character specifying the prefix you want to add to each bmr temporary file name.}

\item{groupSize}{a numeric specifying the number of tasks you want to benchamrk in a single batch. If \code{NULL} the value will be set to the length of \code{tasks} argument. Default is \code{NULL}}

\item{removeTemp}{a boolean specifying if the temporary .rds generated by the function must be deleted at the end of the process.}

\item{crash}{a boolean. \code{TRUE} if you want the function crash and stop. \code{FALSE} if yo uwant the function to handle the error and continue. This is based on the \code{mlr::configureMlr(on.learner.error = "warn")}
Default to FALSE}

\item{resampling}{a character specifying the type of mlr's Cross-Validation strategy. Default = \code{"LOO"}}
}
\value{
A 2 elements named list
\itemize{
  \item \code{snitch} : a boolean. Is \code{TRUE} if function has provided the expected result. Is \code{FALSE} is function throws an error
  \item \code{output} : a named list which elements are :\itemize{
    \item \code{value} : an element of class \code{mlr::benchmark()}
    \item \code{condition} : a character specifying the condition encountered by the function : success, warning, or error.
    \item \code{message} : a character specifying the message relative to the condition.
    }
 }
}
\description{
make a (multicore) mlr benchmark experiment of your learner(s) on your task(s)
}
\details{
The function handles learners error. See \code{mlr::configureMlr()}.
}
\examples{
\dontrun{
# load magrittr for pipe use : \%>\%
library(magrittr)

# create the dataset
myDataset = makeDataset(
  dfrom = "2017-03-04T15:00:00Z",
  dto = "2017-03-04T18:00:00Z",
  sensor = "tsa")

# extract the list of hourly sets of records
myDataset = myDataset$output$value

# create the tasks
myTasks = purrr::map(myDataset, makeTask, target = "tsa")

# extract the used sids of each task from the outputs
myUsedSids = myTasks \%>\% purrr::modify_depth(1, ~.$output$stations$used)

# extract the tasks from the outputs
myTasks = myTasks \%>\% purrr::modify_depth(1, ~.$output$value$task)

# Conduct a batch of benchmarks experiments without saving temp files
myBmrsBatch = makeBmrsBatch(
tasks = myTasks,
learners = agrometeorLearners,
measures = list(mlr::rmse),
keep.pred = TRUE,
models = FALSE,
groupSize  = NULL,
level = "mlr.benchmark",
resamplings = "LOO",
cpus = 1,
prefix = NULL,
temp_dir = NULL,
removeTemp = FALSE,
crash = FALSE)

# Keep the relevant information
myBmrsBatch = myBmrsBatch$output$value

# make a plot from the myBmrsBatch
mlr::plotBMRBoxplots(myBmrsBatch,
  measure = mlr::rmse,
  order.lrn = getBMRLearnerIds(myBmrsBatch),
  pretty.names = FALSE)
}
}
\author{
Thomas Goossens
}
