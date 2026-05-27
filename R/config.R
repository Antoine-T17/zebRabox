# ======================================================================
# R/config.R
# Configuration factories for ZebraBox analytical modes
# ======================================================================

#' Get processing configuration for a ZebraBox analytical mode
#'
#' Returns a named list of parameters used by [run_processing()] and its
#' component functions for a given analytical mode.
#'
#' @param mode Character string, one of `"tm_ldm"`, `"tm_vm"`, `"qm_ldm"`,
#'   `"qm_vm"`. Defaults to `"tm_ldm"` for any unrecognised value.
#' @return A named list with the following elements:
#'   \describe{
#'     \item{`ui_title`}{Character. Human-readable mode label.}
#'     \item{`period_map`}{Function. Maps `period_with_numbers` values to
#'       `period_without_numbers` labels (e.g. `"light1"` → `"light"`).}
#'     \item{`period_keys`}{Character vector. Base period names (e.g.
#'       `c("light", "dark")`).}
#'     \item{`convert_cols`}{Character vector. Column names to convert from
#'       locale-decimal format to numeric.}
#'     \item{`zone_num_cols`}{Character vector. Numeric metric column names
#'       used by [process_zones()].}
#'     \item{`filter_fn`}{Function or `NULL`. Pre-processing row filter
#'       applied before [generate_conditions()] (Quantization mode only).}
#'   }
#' @seealso [get_visualization_config()], [run_processing()]
#' @export
get_processing_config <- function(mode) {
  ldm_period_map <- function(x) {
    dplyr::case_when(
      stringr::str_detect(x, "^light") ~ "light",
      stringr::str_detect(x, "^dark")  ~ "dark",
      TRUE ~ x
    )
  }

  vm_period_map <- function(x) {
    dplyr::case_when(
      stringr::str_detect(x, "^vibration") ~ "vibration",
      stringr::str_detect(x, "^rest")      ~ "rest",
      TRUE ~ x
    )
  }

  qm_filter_fn <- function(df) {
    if ("datatype" %in% colnames(df) &&
        any(stringr::str_detect(df$datatype, "quantauc"))) {
      dplyr::filter(df, stringr::str_detect(datatype, "quantauc"))
    } else {
      df
    }
  }

  tm_convert_cols  <- c("inact","inadur","inadist","smlct","smldist","smldur",
                        "larct","lardur","lardist","emptyct","emptydur","period")
  tm_zone_num_cols <- c("inact","inadur","inadist","smlct","smldist","smldur",
                        "larct","lardur","lardist","emptyct","emptydur")
  qm_convert_cols  <- c("frect","fredur","midct","middur","burct","burdur",
                        "zerct","zerdur","actinteg","period")
  qm_zone_num_cols <- c("frect","fredur","midct","middur","burct","burdur",
                        "zerct","zerdur","actinteg")

  switch(mode,
    "tm_ldm" = list(
      ui_title      = "Tracking Mode, Light-Dark Mode",
      period_map    = ldm_period_map,
      period_keys   = c("light", "dark"),
      convert_cols  = tm_convert_cols,
      zone_num_cols = tm_zone_num_cols,
      filter_fn     = NULL
    ),
    "tm_vm" = list(
      ui_title      = "Tracking Mode, Vibration-Rest Mode",
      period_map    = vm_period_map,
      period_keys   = c("vibration", "rest"),
      convert_cols  = tm_convert_cols,
      zone_num_cols = tm_zone_num_cols,
      filter_fn     = NULL
    ),
    "qm_ldm" = list(
      ui_title      = "Quantization Mode, Light-Dark Mode",
      period_map    = ldm_period_map,
      period_keys   = c("light", "dark"),
      convert_cols  = qm_convert_cols,
      zone_num_cols = qm_zone_num_cols,
      filter_fn     = qm_filter_fn
    ),
    "qm_vm" = list(
      ui_title      = "Quantization Mode, Vibration-Rest Mode",
      period_map    = vm_period_map,
      period_keys   = c("vibration", "rest"),
      convert_cols  = qm_convert_cols,
      zone_num_cols = qm_zone_num_cols,
      filter_fn     = qm_filter_fn
    ),
    list(
      ui_title      = "Tracking Mode, Light-Dark Mode",
      period_map    = ldm_period_map,
      period_keys   = c("light", "dark"),
      convert_cols  = tm_convert_cols,
      zone_num_cols = tm_zone_num_cols,
      filter_fn     = NULL
    )
  )
}

#' Get visualization configuration for a ZebraBox analytical mode
#'
#' Returns a named list of parameters used by [prepare_all_zone()],
#' [build_periods_df()], and the visualization module UI for a given
#' analytical mode.
#'
#' @param mode Character string, one of `"tm_ldm"`, `"tm_vm"`, `"qm_ldm"`,
#'   `"qm_vm"`. Defaults to `"tm_ldm"` for any unrecognised value.
#' @return A named list with the following elements:
#'   \describe{
#'     \item{`ui_title`}{Character. Human-readable mode label.}
#'     \item{`expected_vars`}{Character vector. Response variable names
#'       available for this mode.}
#'     \item{`period_keys`}{Character vector. Base period names matched
#'       against `period_without_numbers` in the data.}
#'     \item{`period_labels`}{Character vector. Display labels corresponding
#'       to `period_keys` (used as factor levels in [build_periods_df()]).}
#'     \item{`period_ui_name`}{Character. Short label shown in the app UI.}
#'     \item{`period_default_colors`}{Character. Comma-separated hex colours
#'       used as default period palette in the app.}
#'   }
#' @seealso [get_processing_config()], [build_periods_df()],
#'   [prepare_all_zone()]
#' @export
get_visualization_config <- function(mode) {
  tm_expected <- c(
    "totaldist","totaldur","totalct","totalspeed",
    "lardist","lardur","larct","larspeed",
    "smldist","smldur","smlct","smlspeed",
    "inadist","inadur","inact","emptydur","emptyct"
  )
  qm_expected <- c(
    "frect","fredur","midct","middur",
    "burct","burdur","zerct","zerdur","actinteg"
  )

  ldm_keys   <- c("light", "dark")
  ldm_labels <- c("Light period", "Dark period")
  vm_keys    <- c("vibration", "rest")
  vm_labels  <- c("Vibration period", "Rest period")

  switch(mode,
    "tm_ldm" = list(
      ui_title              = "Tracking Mode, Light-Dark Mode",
      expected_vars         = tm_expected,
      period_keys           = ldm_keys,
      period_labels         = ldm_labels,
      period_ui_name        = "Light/Dark",
      period_default_colors = "#FFC300,#B3AAAA"
    ),
    "tm_vm" = list(
      ui_title              = "Tracking Mode, Vibration-Rest Mode",
      expected_vars         = tm_expected,
      period_keys           = vm_keys,
      period_labels         = vm_labels,
      period_ui_name        = "Vibration/Rest",
      period_default_colors = "#5E81AC,#A3BE8C"
    ),
    "qm_ldm" = list(
      ui_title              = "Quantization Mode, Light-Dark Mode",
      expected_vars         = qm_expected,
      period_keys           = ldm_keys,
      period_labels         = ldm_labels,
      period_ui_name        = "Light/Dark",
      period_default_colors = "#FFC300,#B3AAAA"
    ),
    "qm_vm" = list(
      ui_title              = "Quantization Mode, Vibration-Rest Mode",
      expected_vars         = qm_expected,
      period_keys           = vm_keys,
      period_labels         = vm_labels,
      period_ui_name        = "Vibration/Rest",
      period_default_colors = "#5E81AC,#A3BE8C"
    ),
    list(
      ui_title              = "Tracking Mode, Light-Dark Mode",
      expected_vars         = tm_expected,
      period_keys           = ldm_keys,
      period_labels         = ldm_labels,
      period_ui_name        = "Light/Dark",
      period_default_colors = "#FFC300,#B3AAAA"
    )
  )
}
