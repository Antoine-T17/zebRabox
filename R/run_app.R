#' Run ZebRabox
#' @export
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

