#' Run the zebRabox Shiny application
#'
#' Launches the graphical interface covering the full workflow: plate plan
#' creation or import, raw data import, processing, visualization, percentage
#' tables and statistics.
#'
#' @param ... Further arguments passed to [shiny::runApp()] (e.g. `port`).
#' @param launch.browser Logical. Open the app in the default web browser.
#'   Default: `TRUE`.
#' @return Called for its side effect; returns `invisible(NULL)` when the app
#'   is closed.
#' @seealso [zebrabox_example()] for a demo dataset to try the app.
#' @export
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(..., launch.browser = TRUE) {
  app_dir <- system.file("app", package = "zebRabox")
  if (app_dir == "") stop("App directory not found. Is 'zebRabox' installed?")

  # When called via zebRabox::run_app(), the namespace is loaded but not
  # attached to the search path. Shiny sources inst/app/ scripts into
  # .GlobalEnv, so package functions (get_processing_config, etc.) would
  # not be found without this explicit attach.
  if (!"package:zebRabox" %in% search()) attachNamespace("zebRabox")

  tryCatch(
    withr::with_dir(app_dir, shiny::runApp(app_dir, launch.browser = launch.browser, ...)),
    interrupt = function(e) invisible(NULL)
  )
}

