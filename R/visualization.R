# ======================================================================
# R/visualization.R
# Standalone visualization utilities and dataset builders (no Shiny dep)
# ======================================================================

# Required so data.table's cedta() check passes when :=  is used inside
# this package's namespace. See vignette('datatable-importing').
.datatable.aware <- TRUE

# ----------------------------------------------------------------------
# Internal helper: well_key column
# ----------------------------------------------------------------------

add_txt_keys <- function(df) {
  dplyr::mutate(df, well_key = paste(plate_id, animal, sep = "__"))
}

# ----------------------------------------------------------------------
# Themes
# ----------------------------------------------------------------------

#' Light ggplot2 theme for ZebraBox figures
#'
#' A white-background theme with large axis/legend text suitable for
#' publication figures.
#'
#' @param base_size Base font size (unused; kept for API compatibility).
#' @param base_family Base font family (unused; kept for API compatibility).
#' @return A [ggplot2::theme()] object.
#' @export
light_theme <- function(base_size = 11, base_family = "") {
  # Avoids calling complete theme functions (theme_bw, theme_grey, etc.) which
  # internally use %+replace% — a call that fails under ggplot2 4.0 S7 dispatch
  # inside Shiny reactive contexts. All needed panel/grid elements are set here.
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    panel.border     = ggplot2::element_rect(color = "black", fill = NA),
    panel.grid.major = ggplot2::element_line(colour = "grey92"),
    panel.grid.minor = ggplot2::element_line(colour = "grey92", linewidth = ggplot2::rel(0.5)),
    strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
    plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
    legend.key       = ggplot2::element_rect(fill = "white", colour = NA),
    plot.title   = ggplot2::element_text(color = "black", size = 14, hjust = .5),
    axis.text    = ggplot2::element_text(color = "black", size = 16),
    axis.title.x = ggplot2::element_text(color = "black", size = 16, margin = ggplot2::margin(t = 5, r = 15)),
    axis.title.y = ggplot2::element_text(color = "black", size = 16, angle = 90, margin = ggplot2::margin(r = 10)),
    legend.position = "right",
    legend.text  = ggplot2::element_text(color = "black", size = 16, face = "italic"),
    legend.title = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_text(size = 16),
    plot.caption = ggplot2::element_text(color = "black", size = 16, hjust = 1, margin = ggplot2::margin(t = 10))
  )
}

#' Dark ggplot2 theme for ZebraBox figures
#'
#' A black-background theme matching the app's dark display mode.
#'
#' @param base_size Base font size (unused; kept for API compatibility).
#' @param base_family Base font family (unused; kept for API compatibility).
#' @return A [ggplot2::theme()] object.
#' @export
dark_theme <- function(base_size = 11, base_family = "") {
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "black", colour = "white"),
    panel.border     = ggplot2::element_rect(color = "white", fill = NA),
    panel.grid.major = ggplot2::element_line(color = "grey30"),
    panel.grid.minor = ggplot2::element_line(color = "grey30"),
    strip.background = ggplot2::element_rect(fill = "black", color = "white"),
    plot.background  = ggplot2::element_rect(fill = "black"),
    legend.background = ggplot2::element_rect(fill = "black"),
    legend.key       = ggplot2::element_rect(fill = "black"),
    plot.title   = ggplot2::element_text(color = "white", size = 16, hjust = .5),
    axis.text    = ggplot2::element_text(color = "white", size = 16),
    axis.title.x = ggplot2::element_text(color = "white", size = 16, margin = ggplot2::margin(t = 5, r = 15)),
    axis.title.y = ggplot2::element_text(color = "white", size = 16, angle = 90, margin = ggplot2::margin(r = 10)),
    legend.position = "right",
    legend.text  = ggplot2::element_text(color = "white", size = 16, face = "italic"),
    legend.title = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_text(color = "white", size = 16),
    plot.caption = ggplot2::element_text(color = "white", size = 16, hjust = 1, margin = ggplot2::margin(t = 10))
  )
}

#' Blank ggplot2 theme for placeholder figures
#'
#' Equivalent to [ggplot2::theme_void()] but implemented without calling
#' complete theme functions, avoiding the ggplot2 4.0 S7/%+replace% issue.
#'
#' @return A [ggplot2::theme()] object.
#' @export
void_theme <- function() {
  ggplot2::theme(
    axis.line        = ggplot2::element_blank(),
    axis.text        = ggplot2::element_blank(),
    axis.ticks       = ggplot2::element_blank(),
    axis.title       = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.border     = ggplot2::element_blank(),
    panel.grid       = ggplot2::element_blank(),
    plot.background  = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    strip.text       = ggplot2::element_blank(),
    legend.key       = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank()
  )
}

# ----------------------------------------------------------------------
# convert_time
# ----------------------------------------------------------------------

#' Convert time values between units
#'
#' @param x Numeric vector of time values.
#' @param from Source unit: one of `"seconds"`, `"minutes"`, `"hours"`,
#'   `"days"`.
#' @param to Target unit (same set as `from`).
#' @return Numeric vector of converted time values.
#' @export
convert_time <- function(x, from, to) {
  if (from == to) return(x)
  f <- c(seconds = 1, minutes = 60, hours = 3600, days = 86400)
  x * f[[from]] / f[[to]]
}

# ----------------------------------------------------------------------
# ensure_colors
# ----------------------------------------------------------------------

#' Ensure exactly n valid color values
#'
#' Returns a character vector of exactly `n` colors. If fewer than `n`
#' colors are provided they are recycled; if more are provided they are
#' truncated. When `cols` is empty a palette is generated automatically.
#'
#' @param n Integer number of colors required.
#' @param cols Optional character vector of hex color codes.
#' @return Character vector of length `n`.
#' @export
ensure_colors <- function(n, cols = character(0)) {
  cols <- cols[!is.na(cols) & nzchar(trimws(cols))]
  if (length(cols) == 0) {
    base <- tryCatch(
      RColorBrewer::brewer.pal(min(8, max(3, n)), "Set1"),
      error = function(e) grDevices::rainbow(min(8, max(3, n)))
    )
    return(grDevices::colorRampPalette(base)(n))
  }
  cols <- trimws(cols)
  if (length(cols) < n) cols <- rep(cols, length.out = n)
  if (length(cols) > n) cols <- cols[seq_len(n)]
  cols
}

# ----------------------------------------------------------------------
# prepare_all_zone
# ----------------------------------------------------------------------

#' Compute derived metrics and combine processed plates into one data frame
#'
#' For Tracking Mode data, derives `totaldist`, `totaldur`, `totalct`,
#' `totalspeed`, `smlspeed`, `larspeed`, and `inaspeed` from the raw
#' activity columns. Binds all plates into a single data frame.
#'
#' @param processed_data_list List of processed XLSX data frames (output
#'   of [run_processing()], slot `processed_data_list`).
#' @param cfg Visualization config list (output of
#'   [get_visualization_config()]). Currently used to detect the
#'   analytical mode via `cfg$mode`; if absent the TM branch is applied.
#' @return A single data frame with all plates row-bound.
#' @export
prepare_all_zone <- function(processed_data_list, cfg) {
  mode  <- if (!is.null(cfg$mode)) cfg$mode else "unknown"
  is_qm <- grepl("^qm", mode)

  dts <- lapply(processed_data_list, function(.x) {
    dt <- data.table::as.data.table(.x)
    dt[, plate_id := as.character(plate_id)]

    if (all(c("smldist", "lardist", "inadist") %in% names(dt))) {
      dt[, `:=`(
        totaldist = smldist + lardist + inadist,
        totaldur  = smldur  + lardur  + inadur,
        totalct   = smlct   + larct   + inact
      )]
      dt[, `:=`(
        totalspeed = totaldist / pmax(totaldur, 1),
        smlspeed   = smldist  / pmax(smldur,   1),
        larspeed   = lardist  / pmax(lardur,   1),
        inaspeed   = inadist  / pmax(inadur,   1)
      )]
    }

    if (is_qm && all(c("frect", "midct", "burct", "zerct") %in% names(dt))) {
      dt[, totaldist  := frect + midct + burct + zerct]
      dt[, totaldur   := fredur + middur + burdur + zerdur]
      dt[, totalct    := 0L]
      dt[, totalspeed := totaldist / pmax(totaldur, 1)]
    }
    dt
  })

  as.data.frame(data.table::rbindlist(dts, fill = TRUE))
}

# ----------------------------------------------------------------------
# build_periods_df
# ----------------------------------------------------------------------

#' Build per-period boxplot dataset
#'
#' Groups the combined data by `period_without_numbers`, `zone`,
#' `condition_tagged`, and `plate_id`, and summarises the mean of the
#' response variable per animal per period.
#'
#' @param az Data frame returned by [prepare_all_zone()].
#' @param v Name of the response variable column.
#' @param cfg Visualization config list. Must contain `period_keys` and
#'   optionally `period_labels`.
#' @param period_indices_keep Optional character string of comma-separated
#'   period indices to keep (e.g. `"1"` or `"1,2"`). `NULL` or `""` keeps
#'   all periods.
#' @return Summarised data frame with `mean_val` column.
#' @export
build_periods_df <- function(az, v, cfg, period_indices_keep = NULL) {
  periods <- unique(az$period_without_numbers)
  keys    <- cfg$period_keys
  labels  <- cfg$period_labels %||% keys
  if (is.null(keys) || length(keys) == 0) stop("Configuration error: 'period_keys' is empty.")

  found_keys   <- character(0)
  found_labels <- character(0)

  for (i in seq_along(keys)) {
    p_match <- grep(keys[i], periods, ignore.case = TRUE, value = TRUE)
    if (length(p_match) > 0) {
      found_keys   <- c(found_keys, p_match[1])
      found_labels <- c(found_labels, labels[i])
    }
  }

  if (length(found_keys) == 0) {
    stop(sprintf(
      "No periods matching any of %s found in data. Available: %s",
      paste0("'", keys, "'", collapse = ", "),
      paste0("'", periods, "'", collapse = ", ")
    ))
  }

  out <- az |>
    dplyr::filter(period_without_numbers %in% found_keys)

  if (!is.null(period_indices_keep) && nzchar(trimws(period_indices_keep))) {
    idx_keep <- trimws(strsplit(period_indices_keep, ",")[[1]])
    idx_keep <- idx_keep[nzchar(idx_keep)]

    out <- out |>
      dplyr::mutate(
        .period_idx = sub(".*_([0-9]+)$", "\\1", as.character(period_with_numbers))
      ) |>
      dplyr::filter(.period_idx %in% idx_keep) |>
      dplyr::select(-.period_idx)
  }

  out |>
    dplyr::group_by(period_without_numbers, zone, condition_tagged, plate_id) |>
    dplyr::summarise(
      plate_id            = dplyr::first(as.character(plate_id)),
      start               = dplyr::first(start),
      period_with_numbers = dplyr::first(period_with_numbers),
      condition_grouped   = dplyr::first(condition_grouped),
      condition           = dplyr::first(condition),
      animal              = dplyr::first(animal),
      mean_val            = mean(.data[[v]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      period_without_numbers = factor(period_without_numbers, levels = found_keys, labels = found_labels)
    ) |>
    dplyr::select(dplyr::any_of(c(
      "zone", "condition_grouped", "condition_tagged", "condition",
      "start", "plate_id", "animal",
      "period_without_numbers", "period_with_numbers", "mean_val"
    )))
}

# ----------------------------------------------------------------------
# build_cumulate_df
# ----------------------------------------------------------------------

#' Build cumulative boxplot dataset
#'
#' Sums the response variable over the entire experiment per animal,
#' grouped by `condition_grouped`, `zone`, `plate_id`, and `animal`.
#'
#' @param az Data frame returned by [prepare_all_zone()].
#' @param v Name of the response variable column.
#' @return Data frame with a `cum` column (cumulative sum per animal).
#' @export
build_cumulate_df <- function(az, v) {
  az |>
    dplyr::group_by(condition_grouped, zone, plate_id, animal) |>
    dplyr::summarise(
      cum              = sum(.data[[v]], na.rm = TRUE),
      condition_tagged = dplyr::first(condition_tagged),
      .groups = "drop"
    ) |>
    dplyr::mutate(plate_id = as.character(plate_id)) |>
    dplyr::select(dplyr::any_of(c(
      "zone", "condition_grouped", "condition_tagged", "plate_id", "animal", "cum"
    )))
}

# ----------------------------------------------------------------------
# build_delta_split
# ----------------------------------------------------------------------

#' Build delta-phase dataset around a transition time point
#'
#' Assigns each row to one of three phases — Before `[t-Δ, t)`,
#' Switch `[t, t+Δ)`, After `[t+Δ, t+2Δ)` — relative to the transition
#' time for each plate, then summarises the mean of each response
#' variable per phase × animal. Returns a named list (one element per
#' variable in `vars`).
#'
#' @param az Data frame from [prepare_all_zone()].
#' @param vars Character vector of response variable names.
#' @param transition Character string identifying the transition (must
#'   match values in `boundaries_df$transition`).
#' @param delta_sec Length of each phase window in seconds.
#' @param boundaries_df Data frame with columns `plate_id`, `time_switch`,
#'   and `transition` (typically
#'   `dplyr::bind_rows(rv$processing_results$boundary_associations_list) |> dplyr::distinct()`).
#' @param round_to Optional numeric; if provided, `start` values are
#'   rounded down to the nearest multiple before phase assignment.
#' @return Named list of data frames (one per variable in `vars`), or
#'   `NULL` if no data matches the transition.
#' @export
build_delta_split <- function(az, vars, transition, delta_sec, boundaries_df,
                               round_to = NULL) {
  if (is.null(boundaries_df) || !nrow(boundaries_df)) return(NULL)

  b_clean <- boundaries_df |>
    dplyr::mutate(plate_id = as.character(plate_id)) |>
    dplyr::arrange(transition, plate_id, time_switch) |>
    dplyr::group_by(transition, plate_id) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::filter(transition == !!transition) |>
    dplyr::select(plate_id, time_switch)

  joined <- az |>
    dplyr::mutate(
      plate_id      = as.character(plate_id),
      start_for_cut = if (!is.null(round_to)) floor(start / round_to) * round_to else start
    ) |>
    dplyr::inner_join(b_clean, by = "plate_id", relationship = "many-to-many") |>
    dplyr::mutate(
      phase_raw = dplyr::case_when(
        start_for_cut >= time_switch - delta_sec & start_for_cut < time_switch             ~ "before",
        start_for_cut >= time_switch             & start_for_cut < time_switch + delta_sec ~ "switch",
        start_for_cut >= time_switch + delta_sec & start_for_cut < time_switch + 2 * delta_sec ~ "after",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(phase_raw)) |>
    dplyr::mutate(
      transition_phase       = paste0(transition, "_", phase_raw),
      period_without_numbers = dplyr::recode(phase_raw, before = "Before", switch = "Switch", after = "After"),
      period_with_numbers    = paste0(transition, "_", period_without_numbers)
    )

  if (!nrow(joined)) return(NULL)

  phased_long <- tidyr::pivot_longer(
    joined,
    cols = dplyr::all_of(vars),
    names_to = "variable",
    values_to = "value"
  ) |>
    dplyr::group_by(
      transition_phase, period_without_numbers, period_with_numbers,
      zone, condition_tagged, plate_id, animal, variable
    ) |>
    dplyr::summarise(
      mean_val          = mean(value, na.rm = TRUE),
      condition_grouped = dplyr::first(condition_grouped),
      start             = dplyr::first(start),
      .groups = "drop"
    ) |>
    dplyr::select(dplyr::any_of(c(
      "zone", "condition_grouped", "condition_tagged",
      "plate_id", "animal", "mean_val",
      "period_without_numbers", "period_with_numbers",
      "transition_phase", "variable", "start"
    )))

  split(phased_long, phased_long$variable)
}

# ----------------------------------------------------------------------
# build_lineplot_df
# ----------------------------------------------------------------------

#' Build lineplot dataset with per-well temporal aggregation
#'
#' Sums the response variable within each time bin per well, then
#' computes cross-well mean, SD, SE, and n per condition group × zone.
#'
#' @param az Data frame from [prepare_all_zone()].
#' @param v Name of the response variable column.
#' @param agg_period Aggregation period (numeric, in `unit_from` or
#'   `unit_to` depending on `convert`).
#' @param unit_from Original time unit of the `start` column.
#' @param unit_to Target time unit (used when `convert == "Yes"`).
#' @param convert `"Yes"` or `"No"` — whether to convert `start` to
#'   `unit_to` before binning.
#' @return Data frame with columns `zone`, `condition_grouped`,
#'   `start_rounded`, `plate_id`, `animal`, `total_val`, `n_wells`,
#'   `val_per_well`, `se_per_well`, `sd_per_well`.
#' @export
build_lineplot_df <- function(az, v, agg_period, unit_from, unit_to, convert) {
  agg_unit <- if (identical(convert, "Yes")) unit_to else unit_from
  agg_s    <- convert_time(as.numeric(agg_period), agg_unit, "seconds")
  gvar     <- "condition_grouped"

  dt <- data.table::as.data.table(az)
  dt[, plate_id      := as.character(plate_id)]
  dt[, start_rounded := floor(start / agg_s) * agg_s]

  by_well <- c(gvar, "zone", "start_rounded", "plate_id", "animal")
  per_well_dt <- dt[, .(var_value_per_well = sum(get(v), na.rm = TRUE)), by = by_well]

  by_cond <- c(gvar, "zone", "start_rounded")
  summary_dt <- per_well_dt[, .(
    total_val    = sum(var_value_per_well, na.rm = TRUE),
    val_per_well = mean(var_value_per_well, na.rm = TRUE),
    sd_per_well  = stats::sd(var_value_per_well, na.rm = TRUE),
    n_wells      = .N
  ), by = by_cond]
  summary_dt[, se_per_well := sd_per_well / sqrt(pmax(n_wells, 1L))]

  result_dt <- summary_dt[per_well_dt, on = by_cond]

  if (!identical(unit_from, agg_unit)) {
    result_dt[, start_rounded := convert_time(start_rounded, "seconds", agg_unit)]
  }

  keep_cols <- intersect(
    c("zone", gvar, "start_rounded", "plate_id", "animal",
      "total_val", "n_wells", "val_per_well", "se_per_well", "sd_per_well"),
    names(result_dt)
  )

  as.data.frame(result_dt[, .SD, .SDcols = keep_cols])
}

# ----------------------------------------------------------------------
# estimate_txt_dt
# ----------------------------------------------------------------------

estimate_txt_dt <- function(df) {
  dt <- df |>
    dplyr::summarise(dt = stats::median(diff(sort(unique(T))), na.rm = TRUE)) |>
    dplyr::pull(dt)
  if (!is.finite(dt) || dt <= 0) dt <- 1 / 25
  dt
}

# ----------------------------------------------------------------------
# resolve_txt_selected_wells
# ----------------------------------------------------------------------

#' Resolve the set of well keys to display from selection inputs
#'
#' Filters a TXT data frame by plate IDs and conditions, then returns
#' the matching set of `well_key` identifiers (`"<plate_id>__<animal>"`).
#'
#' @param df TXT data frame (output of [build_txt_trajectory_df()] or
#'   similar). Must contain `plate_id`, `animal`, and `condition_grouped`.
#' @param plate_ids Character vector of plate IDs to include. `NULL` =
#'   all plates.
#' @param selected_conditions Character vector of `condition_grouped`
#'   values to include.
#' @param selected_wells Character vector of specific `well_key` values to
#'   include (intersected with available wells). If empty and
#'   `selected_conditions` is non-empty, all wells matching the conditions
#'   are returned.
#' @return Character vector of `well_key` values.
#' @export
resolve_txt_selected_wells <- function(df, plate_ids = NULL,
                                       selected_conditions = character(0),
                                       selected_wells = character(0)) {
  df <- add_txt_keys(df)

  if (!is.null(plate_ids) && length(plate_ids)) {
    df <- df |> dplyr::filter(plate_id %in% plate_ids)
  }

  if (!nrow(df)) return(character(0))

  if (!is.null(selected_conditions) && length(selected_conditions)) {
    df <- df |> dplyr::filter(condition_grouped %in% selected_conditions)
  }

  available_wells <- unique(df$well_key)

  if (!is.null(selected_wells) && length(selected_wells)) {
    return(intersect(selected_wells, available_wells))
  }

  if (!is.null(selected_conditions) && length(selected_conditions)) {
    return(available_wells)
  }

  character(0)
}

# ----------------------------------------------------------------------
# resolve_txt_time_range
# ----------------------------------------------------------------------

#' Resolve a time range for TXT trajectory subsetting
#'
#' Returns `c(tmin, tmax)` in seconds, either from explicit numeric
#' bounds (mode `"manual"`) or from the actual time range of a named
#' period (mode `"period"`).
#'
#' @param df TXT data frame with `T` and `period_with_numbers` columns.
#' @param well_keys Character vector of `well_key` values (see
#'   [resolve_txt_selected_wells()]).
#' @param mode `"manual"` or `"period"`.
#' @param time_start Numeric start time (seconds); used when
#'   `mode == "manual"`.
#' @param time_end Numeric end time (seconds); used when
#'   `mode == "manual"`.
#' @param period_value Character period label; used when
#'   `mode == "period"`.
#' @return Numeric vector of length 2: `c(tmin, tmax)`.
#' @export
resolve_txt_time_range <- function(df, well_keys, mode,
                                   time_start = NULL, time_end = NULL,
                                   period_value = NULL) {
  df <- add_txt_keys(df)
  sub <- df |> dplyr::filter(well_key %in% well_keys)
  if (!nrow(sub)) stop("No TXT data for selected wells.")

  if (identical(mode, "manual")) {
    tmin <- suppressWarnings(as.numeric(time_start))
    tmax <- suppressWarnings(as.numeric(time_end))
    if (!is.finite(tmin) || !is.finite(tmax) || tmax <= tmin) {
      stop("Invalid manual time window. End time must be greater than start time.")
    }
    return(c(tmin, tmax))
  }

  if (identical(mode, "period")) {
    if (is.null(period_value) || !nzchar(period_value)) stop("Please select a period.")
    subp <- sub |> dplyr::filter(period_with_numbers == period_value)
    if (!nrow(subp)) stop("No TXT data found for the selected period.")
    return(range(subp$T, na.rm = TRUE))
  }

  stop("Unknown TXT time mode.")
}

# ----------------------------------------------------------------------
# build_txt_trajectory_df
# ----------------------------------------------------------------------

#' Build a binned XY trajectory dataset for TXT visualization
#'
#' Filters the TXT data to the selected wells and time window, removes
#' `(0,0)` origin points, bins the remaining rows into temporal bins
#' sized to keep at most `target_points` per well, and averages X/Y
#' within each bin.
#'
#' @param df TXT data frame with columns `T`, `X`, `Y`, `plate_id`,
#'   `animal`, `condition_grouped`, `condition_tagged`, and `zone`.
#' @param well_keys Character vector of `well_key` values.
#' @param time_range Numeric vector `c(tmin, tmax)` in seconds.
#' @param target_points Maximum number of displayed points per well
#'   after binning (default 100).
#' @return Data frame with binned trajectory points. Attributes
#'   `bin_s`, `native_dt`, and `time_range` are attached.
#' @export
build_txt_trajectory_df <- function(df, well_keys, time_range,
                                    target_points = 100) {
  if (length(well_keys) == 0) stop("well_keys must contain at least one key.")
  if (length(time_range) != 2) stop("time_range must be a numeric vector of length 2.")

  if (length(unique(well_keys)) > 96) {
    stop("Please select at most 96 wells for TXT trajectory visualization.")
  }

  df <- add_txt_keys(df)

  tmin <- min(time_range)
  tmax <- max(time_range)

  sub <- df |>
    dplyr::filter(
      well_key %in% well_keys,
      T >= tmin,
      T <= tmax
    ) |>
    dplyr::filter(!(X == 0 & Y == 0))

  if (!nrow(sub)) stop("No TXT data available for the selected wells / time window.")

  native_dt  <- estimate_txt_dt(sub)
  window_len <- max(tmax - tmin, native_dt)

  target_points <- max(25, suppressWarnings(as.numeric(target_points)))
  if (!is.finite(target_points)) target_points <- 250

  bin_s <- max(native_dt, window_len / target_points)

  out <- sub |>
    dplyr::mutate(
      time_bin = floor((T - tmin) / bin_s),
      well_id  = animal,
      condition_grouped = dplyr::if_else(
        is.na(condition_grouped) | condition_grouped == "",
        "Unknown",
        condition_grouped
      )
    ) |>
    dplyr::group_by(
      plate_id, animal, well_key, well_id,
      condition_grouped, condition_tagged, zone,
      time_bin
    ) |>
    dplyr::summarise(
      T     = mean(T, na.rm = TRUE),
      X     = mean(X, na.rm = TRUE),
      Y     = mean(Y, na.rm = TRUE),
      n_raw = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(
      is.finite(X), is.finite(Y),
      !(X == 0 & Y == 0)
    ) |>
    dplyr::arrange(condition_grouped, plate_id, animal, T) |>
    dplyr::group_by(plate_id, animal) |>
    dplyr::mutate(
      point_id = paste0("traj_", plate_id, "_", animal, "_", dplyr::row_number()),
      path_id  = paste0("path_", plate_id, "_", animal),
      tooltip  = paste0(
        "Plate: ", plate_id,
        "<br>Well: ", animal,
        "<br>Condition: ", condition_grouped,
        "<br>t = ", sprintf("%.2f", T), " s",
        "<br>X = ", sprintf("%.1f", X),
        "<br>Y = ", sprintf("%.1f", Y),
        "<br>Mean of ", n_raw, " raw points"
      )
    ) |>
    dplyr::ungroup()

  attr(out, "bin_s")      <- bin_s
  attr(out, "native_dt")  <- native_dt
  attr(out, "time_range") <- c(tmin, tmax)

  out
}

# Internal pipe alias used only within this file
`%||%` <- function(a, b) if (is.null(a)) b else a
