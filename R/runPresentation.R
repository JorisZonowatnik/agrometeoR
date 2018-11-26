#' @export
runPresentation <- function() {
  presentationDir <- system.file("tutorials", "presentation", package = "agrometeoR")
  if (presentationDir == "") {
    stop("Could not find presentation directory. Try re-installing `FOSS4GBXL2018`.", call. = FALSE)
  }
  learnr::run_tutorial("presentation", package = "FOSS4GBXL2018")
}
