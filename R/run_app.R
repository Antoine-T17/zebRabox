#' Run ZebRabox
#' @export
run_app <- function(..., launch.browser = TRUE) {
  app_dir <- system.file("app", package = "zebRabox")
  if (app_dir == "") stop("App directory not found. Is 'zebRabox' installed?")

  tryCatch(
    withr::with_dir(app_dir, shiny::runApp(app_dir, launch.browser = launch.browser, ...)),
    interrupt = function(e) invisible(NULL)
  )
}

