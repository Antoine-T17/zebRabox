# ======================================================================
# R/example.R
# Access to the demo dataset shipped in inst/extdata/demo
# ======================================================================

#' Paths to the zebRabox demo dataset
#'
#' zebRabox ships a small, real demo dataset so that the full workflow
#' (plate plans, raw data, processing, visualization) can be tried without
#' your own experiment. Two independent sessions are provided, each recorded
#' on two 24-well plates of *Daphnia* in tracking mode:
#' \describe{
#'   \item{`"light_dark"`}{Light/Dark protocol (acclimatation followed by
#'     alternating light and dark periods).}
#'   \item{`"vibration"`}{Vibration/Rest protocol (acclimatation followed by
#'     short vibration stimuli separated by rest periods).}
#' }
#'
#' Each session folder contains the raw ZebraBox exports
#' (`raw_plate_4.xlsx`, `raw_plate_5.xlsx`), the matching plate plans
#' (`plate_plan_plate_4.xlsx`, `plate_plan_plate_5.xlsx`), a period
#' transition file and a removal specification file. The same files can be
#' uploaded in the Shiny application (see [run_app()]).
#'
#' @param mode Character. Demo session to use: `"light_dark"` (default) or
#'   `"vibration"`.
#' @return A named list of absolute file paths:
#'   \describe{
#'     \item{dir}{Folder containing the session files.}
#'     \item{raw}{Raw data files, one per plate (for [read_raw_files()]).}
#'     \item{plate_plans}{Plate plan files, in the same plate order (for
#'       [read_plate_plan_files()]).}
#'     \item{period_transition}{Period transition file.}
#'     \item{removal}{Removal specification file.}
#'   }
#' @seealso [set_mode()], [read_raw_files()], [read_plate_plan_files()],
#'   [run_processing()]
#' @export
#' @examples
#' demo <- zebrabox_example("light_dark")
#' basename(demo$raw)
#'
#' \donttest{
#' cfg        <- set_mode("tracking", "light_dark")
#' plates     <- read_plate_plan_files(demo$plate_plans)
#' raw_data   <- read_raw_files(demo$raw)
#' period_df  <- as.data.frame(readxl::read_excel(demo$period_transition))
#' removal_df <- as.data.frame(readxl::read_excel(demo$removal))
#' result     <- run_processing(raw_data, plates, period_df, removal_df,
#'                              cfg$processing)
#' }
zebrabox_example <- function(mode = c("light_dark", "vibration")) {
  mode <- match.arg(mode)
  dir  <- system.file("extdata", "demo", mode, package = "zebRabox")
  if (dir == "") stop("Demo dataset not found. Is 'zebRabox' installed?")

  list(
    dir               = dir,
    raw               = sort(list.files(dir, "^raw_plate_.*\\.xlsx$", full.names = TRUE)),
    plate_plans       = sort(list.files(dir, "^plate_plan_.*\\.xlsx$", full.names = TRUE)),
    period_transition = file.path(dir, "period_transition.xlsx"),
    removal           = file.path(dir, "removal_specification.xlsx")
  )
}
