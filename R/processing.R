# ======================================================================
# R/processing.R
# Standalone processing pipeline functions (no Shiny dependency)
# ======================================================================

# ----------------------------------------------------------------------
# Internal helpers (not exported)
# ----------------------------------------------------------------------

should_remove <- function(x) {
  if (length(x) != 1) return(FALSE)
  if (is.na(x)) return(FALSE)
  nzchar(trimws(as.character(x)))
}

# ----------------------------------------------------------------------
# convert_numeric_cols
# ----------------------------------------------------------------------

#' Convert columns to numeric with locale-safe decimal handling
#'
#' Replaces commas with dots before coercing to numeric, which handles
#' locale formats where decimals are written as commas.
#'
#' @param df A data frame.
#' @param cols Character vector of column names to convert.
#' @return The data frame with specified columns converted to numeric.
#' @keywords internal
convert_numeric_cols <- function(df, cols) {
  for (col in intersect(names(df), cols)) {
    df[[col]] <- as.numeric(gsub(",", ".", as.character(df[[col]])))
  }
  df
}

# ----------------------------------------------------------------------
# generate_conditions
# ----------------------------------------------------------------------

#' Assign condition columns from a plate plan to raw data
#'
#' Joins the plate plan to the raw data frame on the animal identifier
#' (stripping any `_plate_N` suffix) and adds three columns:
#' `condition`, `condition_grouped` (base label without replicate suffix),
#' and `condition_tagged` (base label with a per-group row index).
#'
#' @param current_data Data frame of raw data for one plate. Must contain
#'   an `animal` column.
#' @param current_plan Data frame of the plate plan. Must contain
#'   `animal`, `condition`, and `plate_id` columns.
#' @param i Integer plate index, used in log and error messages.
#' @param log_fn Function accepting a single character string; called for
#'   diagnostic messages. Default: silent.
#' @return A named list with two elements:
#'   \describe{
#'     \item{data}{Updated raw data frame with condition columns added.}
#'     \item{plan}{Updated plate plan with `condition_grouped` and
#'       `condition_tagged` columns added (if they were absent).}
#'   }
#' @keywords internal
generate_conditions <- function(current_data, current_plan, i,
                                log_fn = function(msg) invisible(NULL)) {
  # Clean animal IDs from plan (strip _plate_N suffix) used as lookup keys
  plan_clean <- sub("_.*$", "", current_plan$animal)

  # -- condition: O(n) named-vector lookup replacing O(n*m) vapply --
  cond_map               <- stats::setNames(current_plan$condition, plan_clean)
  current_data$condition <- cond_map[current_data$animal]

  if (all(is.na(current_data$condition))) {
    log_fn(sprintf(
      "❌ Error: Plate %d - No conditions could be assigned. Check animal ID matching between raw data and plate plan.",
      i
    ))
    stop("Condition assignment failed for Plate ", i)
  }

  current_data$plate_id <- current_plan$plate_id[1]

  # -- condition_grouped --
  if (!"condition_grouped" %in% names(current_plan)) {
    current_plan$condition_grouped <- ifelse(
      is.na(current_plan$condition),
      NA_character_,
      sub("_.*$", "", current_plan$condition)
    )
  }
  cg_map                         <- stats::setNames(current_plan$condition_grouped, plan_clean)
  current_data$condition_grouped <- cg_map[current_data$animal]

  # -- condition_tagged --
  if (!"condition_tagged" %in% names(current_plan)) {
    current_plan <- current_plan |>
      dplyr::group_by(.data$condition_grouped) |>
      dplyr::mutate(
        condition_tagged = ifelse(
          .data$condition == "X",
          "X",
          paste0(.data$condition_grouped, "_", dplyr::row_number())
        )
      ) |>
      dplyr::ungroup()
  }
  ct_map                        <- stats::setNames(current_plan$condition_tagged, plan_clean)
  current_data$condition_tagged <- ct_map[current_data$animal]

  list(data = current_data, plan = current_plan)
}

# ----------------------------------------------------------------------
# assign_periods
# ----------------------------------------------------------------------

#' Assign period labels to raw data rows based on time boundaries
#'
#' Uses the period-transition file to label each row with
#' `period_with_numbers` (e.g. `"light1"`, `"dark1"`) and
#' `period_without_numbers` (e.g. `"light"`, `"dark"`) via
#' `cfg$period_map`.
#'
#' @param current_data Data frame with a numeric `start` column
#'   (seconds from experiment start).
#' @param period_df Data frame with columns `start` (numeric) and
#'   `transition` (character, e.g. `"light1-dark1"`), sorted ascending.
#' @param i Integer plate index.
#' @param cfg Processing config list; must contain a `period_map`
#'   function if period-number stripping is desired (see
#'   [get_processing_config()]).
#' @param log_fn Logging function.
#' @return A named list:
#'   \describe{
#'     \item{data}{Updated data frame with `period_with_numbers` and
#'       `period_without_numbers` columns.}
#'     \item{boundaries}{Numeric vector of transition start times.}
#'     \item{transitions}{Character vector of transition labels.}
#'   }
#' @keywords internal
assign_periods <- function(current_data, period_df, i, cfg,
                           log_fn = function(msg) invisible(NULL)) {
  current_data$start <- as.numeric(current_data$start)
  if (any(is.na(current_data$start))) {
    log_fn(sprintf(
      "⚠️ Warning: Plate %d - Some values in 'start' column could not be converted to numeric.", i
    ))
  }

  log_fn(sprintf(
    "Plate %d - Range of current_data$start (s): [%.2f, %.2f]",
    i, min(current_data$start, na.rm = TRUE), max(current_data$start, na.rm = TRUE)
  ))
  log_fn(sprintf("Plate %d - Start time codes (s): %s", i, paste(period_df$start, collapse = ", ")))
  log_fn(sprintf("Plate %d - Transitions: %s", i, paste(period_df$transition, collapse = "; ")))

  boundaries  <- period_df$start
  transitions <- period_df$transition

  split_list <- strsplit(transitions, "-", fixed = TRUE)
  split_list <- lapply(split_list, function(parts) {
    if (length(parts) == 1) c(parts, parts)
    else if (length(parts) == 2) parts
    else stop("Each transition must contain a single hyphen.")
  })

  current_data$period_with_numbers <- NA_character_
  for (j in seq_len(nrow(period_df))) {
    start_time    <- period_df$start[j]
    parts         <- split_list[[j]]
    period_before <- parts[1]
    period_after  <- parts[2]

    log_fn(sprintf(
      "Plate %d - Transition %d: %s -> %s at %.2f s",
      i, j, period_before, period_after, start_time
    ))

    if (j == 1) {
      current_data$period_with_numbers[current_data$start < start_time] <- period_before
      log_fn(sprintf("Plate %d - Assigned '%s' to rows where start < %.2f", i, period_before, start_time))
    }

    if (j < nrow(period_df)) {
      next_start <- period_df$start[j + 1]
      current_data$period_with_numbers[current_data$start >= start_time & current_data$start < next_start] <- period_after
      log_fn(sprintf(
        "Plate %d - Assigned '%s' to rows where %.2f <= start < %.2f",
        i, period_after, start_time, next_start
      ))
    } else {
      current_data$period_with_numbers[current_data$start >= start_time] <- period_after
      log_fn(sprintf("Plate %d - Assigned '%s' to rows where start >= %.2f", i, period_after, start_time))
    }
  }

  if (!is.null(cfg$period_map) && is.function(cfg$period_map)) {
    current_data$period_without_numbers <- cfg$period_map(current_data$period_with_numbers)
  } else {
    current_data$period_without_numbers <- current_data$period_with_numbers
  }

  list(data = current_data, boundaries = boundaries, transitions = transitions)
}

# ----------------------------------------------------------------------
# remove_time_codes
# ----------------------------------------------------------------------

#' Remove rows matching specific time codes
#'
#' Drops rows whose `start` value matches any of the comma-separated
#' numeric codes specified in `removal_row$remove_time_codes`.
#'
#' @param current_data Data frame with a `start` column.
#' @param removal_row Single-row data frame containing a
#'   `remove_time_codes` column (comma-separated numerics, or `NA`/`"no"`
#'   to skip).
#' @param i Integer plate index.
#' @param log_fn Logging function.
#' @return Updated data frame with matching rows removed.
#' @keywords internal
remove_time_codes <- function(current_data, removal_row, i,
                              log_fn = function(msg) invisible(NULL)) {
  log_fn("-")
  log_fn(sprintf(
    "Plate %d - Range of 'start' column: [%.2f, %.2f]",
    i, min(current_data$start, na.rm = TRUE), max(current_data$start, na.rm = TRUE)
  ))

  if (should_remove(removal_row$remove_time_codes)) {
    time_codes_str <- as.character(removal_row$remove_time_codes)
    time_codes_to_remove <- suppressWarnings(as.numeric(unlist(strsplit(time_codes_str, ","))))
    if (length(time_codes_to_remove) > 0 && !any(is.na(time_codes_to_remove))) {
      invalid <- setdiff(time_codes_to_remove, current_data$start)
      if (length(invalid) > 0) {
        log_fn(sprintf(
          "⚠️ Warning: Plate %d - These time codes do not match any 'start': %s",
          i, paste(invalid, collapse = ", ")
        ))
      }
      time_codes_to_remove <- intersect(time_codes_to_remove, current_data$start)
      if (length(time_codes_to_remove) > 0) {
        log_fn(sprintf("Plate %d - Removing time codes: %s", i, paste(time_codes_to_remove, collapse = ", ")))
        current_data <- current_data[!current_data$start %in% time_codes_to_remove, ]
      }
    } else {
      log_fn(sprintf("⚠️ Warning: Plate %d - Invalid/empty 'remove_time_codes': %s", i, time_codes_str))
    }
  } else {
    log_fn(sprintf("Plate %d - No time codes to remove.", i))
  }

  log_fn(sprintf("✅ Plate %d - Done.", i))
  current_data
}

# ----------------------------------------------------------------------
# remove_periods
# ----------------------------------------------------------------------

#' Remove rows belonging to specified periods
#'
#' Drops rows whose `period_with_numbers` value matches any of the
#' comma-separated period names in `removal_row$remove_periods`.
#'
#' @param current_data Data frame with a `period_with_numbers` column.
#' @param removal_row Single-row data frame with a `remove_periods`
#'   column (comma-separated period names, or `NA`/`"no"` to skip).
#' @param i Integer plate index.
#' @param log_fn Logging function.
#' @return Updated data frame.
#' @keywords internal
remove_periods <- function(current_data, removal_row, i,
                           log_fn = function(msg) invisible(NULL)) {
  log_fn("-")
  unique_periods <- unique(current_data$period_with_numbers)
  log_fn(sprintf("Plate %d - Unique periods assigned: %s", i, paste(unique_periods, collapse = ", ")))

  if (should_remove(removal_row$remove_periods)) {
    periods_to_remove <- trimws(unlist(strsplit(as.character(removal_row$remove_periods), ",")))
    if (length(periods_to_remove) > 0) {
      invalid <- setdiff(periods_to_remove, current_data$period_with_numbers)
      if (length(invalid) > 0) {
        log_fn(sprintf(
          "⚠️ Warning: Plate %d - These periods do not match any 'period_with_numbers': %s",
          i, paste(invalid, collapse = ", ")
        ))
      }
      periods_to_remove <- intersect(periods_to_remove, current_data$period_with_numbers)
      if (length(periods_to_remove) > 0) {
        log_fn(sprintf("Plate %d - Removing periods: %s", i, paste(periods_to_remove, collapse = ", ")))
        current_data <- current_data[!current_data$period_with_numbers %in% periods_to_remove, ]
      }
    }
  } else {
    log_fn(sprintf("Plate %d - No periods to remove.", i))
  }

  log_fn(sprintf("✅ Plate %d - Done.", i))
  current_data
}

# ----------------------------------------------------------------------
# remove_wells
# ----------------------------------------------------------------------

#' Remove rows belonging to specified wells
#'
#' Drops rows whose `animal` value matches any of the comma-separated
#' well names in `removal_row$remove_wells`.
#'
#' @param current_data Data frame with an `animal` column.
#' @param removal_row Single-row data frame with a `remove_wells`
#'   column (comma-separated animal IDs, or `NA`/`"no"` to skip).
#' @param i Integer plate index.
#' @param log_fn Logging function.
#' @return Updated data frame.
#' @keywords internal
remove_wells <- function(current_data, removal_row, i,
                         log_fn = function(msg) invisible(NULL)) {
  log_fn("-")

  if (should_remove(removal_row$remove_wells)) {
    wells_str <- as.character(removal_row$remove_wells)
    wells_to_remove <- trimws(unlist(strsplit(wells_str, ",")))
    if (length(wells_to_remove) > 0 && !any(is.na(wells_to_remove))) {
      invalid <- setdiff(wells_to_remove, current_data$animal)
      if (length(invalid) > 0) {
        log_fn(sprintf(
          "⚠️ Warning: Plate %d - These wells do not match any 'animal': %s",
          i, paste(invalid, collapse = ", ")
        ))
      }
      wells_to_remove <- intersect(wells_to_remove, current_data$animal)
      if (length(wells_to_remove) > 0) {
        log_fn(sprintf("Plate %d - Wells to remove: %s", i, paste(wells_to_remove, collapse = ", ")))
        current_data <- current_data[!current_data$animal %in% wells_to_remove, ]
      }
    } else {
      log_fn(sprintf("⚠️ Warning: Plate %d - Invalid/empty 'remove_wells': %s", i, wells_str))
    }
  } else {
    log_fn(sprintf("Plate %d - No wells to remove.", i))
  }

  log_fn(sprintf("✅ Plate %d - Done.", i))
  current_data
}

# ----------------------------------------------------------------------
# remove_conditions
# ----------------------------------------------------------------------

#' Remove rows belonging to specified conditions
#'
#' Drops rows whose `condition` value matches any of the comma-separated
#' condition names in `removal_row$remove_conditions`.
#'
#' @param current_data Data frame with `condition` and
#'   `condition_grouped` columns.
#' @param removal_row Single-row data frame with a `remove_conditions`
#'   column (comma-separated condition names, or `NA`/`"no"` to skip).
#' @param i Integer plate index.
#' @param log_fn Logging function.
#' @return Updated data frame.
#' @keywords internal
remove_conditions <- function(current_data, removal_row, i,
                              log_fn = function(msg) invisible(NULL)) {
  log_fn("-")
  unique_grouped <- unique(current_data$condition_grouped)
  log_fn(sprintf("Plate %d - Unique 'condition_grouped': %s", i, paste(unique_grouped, collapse = ", ")))

  if (should_remove(removal_row$remove_conditions)) {
    conditions_str <- as.character(removal_row$remove_conditions)
    conditions_to_remove <- trimws(unlist(strsplit(conditions_str, ",")))
    if (length(conditions_to_remove) > 0 && !any(is.na(conditions_to_remove))) {
      if (!"condition_grouped" %in% names(current_data) || all(is.na(current_data$condition_grouped))) {
        log_fn(sprintf("❌ Error: Plate %d - 'condition_grouped' column missing/empty. Cannot remove conditions.", i))
      } else {
        invalid <- setdiff(conditions_to_remove, current_data$condition_grouped)
        if (length(invalid) > 0) {
          log_fn(sprintf(
            "⚠️ Warning: Plate %d - These conditions do not match any 'condition_grouped': %s",
            i, paste(invalid, collapse = ", ")
          ))
        }
        conditions_to_remove <- intersect(conditions_to_remove, current_data$condition_grouped)
        if (length(conditions_to_remove) > 0) {
          log_fn(sprintf("Plate %d - Removing conditions: %s", i, paste(conditions_to_remove, collapse = ", ")))
          current_data <- current_data[!current_data$condition_grouped %in% conditions_to_remove, ]
        }
      }
    } else {
      log_fn(sprintf("⚠️ Warning: Plate %d - Invalid/empty 'remove_conditions': %s", i, conditions_str))
    }
  } else {
    log_fn(sprintf("Plate %d - No conditions to remove.", i))
  }

  log_fn(sprintf("✅ Plate %d - Done.", i))
  current_data
}

# ----------------------------------------------------------------------
# process_zones
# ----------------------------------------------------------------------

#' Split data by zone and compute zone 1 as zone 0 minus zone 2
#'
#' Splits the data frame by the `an` column (zone identifier). If zones
#' 0 and 2 are both present, creates zone 1 by key-joining zones 0 and 2
#' and subtracting the numeric metric columns listed in
#' `cfg$zone_num_cols`. Returns a single data frame with a `zone` column.
#'
#' @param current_data Data frame with an `an` column identifying zones.
#' @param i Integer plate index.
#' @param cfg Processing config list; must contain `zone_num_cols`
#'   (character vector of numeric column names to subtract).
#' @param log_fn Logging function.
#' @return Data frame with all zones row-bound, including a `zone`
#'   column and only the columns listed in the standard output set.
#' @keywords internal
process_zones <- function(current_data, i, cfg,
                          log_fn = function(msg) invisible(NULL)) {
  log_fn("---")
  log_fn(sprintf("🔄 Plate %d - Processing zones…", i))
  log_fn("---")

  zones     <- unique(current_data$an)
  zone_data <- purrr::map(as.character(zones), ~ dplyr::filter(current_data, .data$an == .x))
  names(zone_data) <- as.character(zones)

  if (all(c(0, 2) %in% zones) && length(cfg$zone_num_cols) > 0) {
    z0 <- zone_data[["0"]] |> dplyr::mutate(.row0 = dplyr::row_number())
    z2 <- zone_data[["2"]]

    keys <- intersect(c("plate_id", "animal", "start"), names(z0))
    keys <- intersect(keys, names(z2))

    if (length(keys) < 2) {
      log_fn(sprintf("⚠️ Plate %d - Not enough keys to align zones 0 and 2; skipping zone 1.", i))
    } else {
      z2_small <- z2 |> dplyr::select(dplyr::all_of(c(keys, cfg$zone_num_cols)))

      joined <- dplyr::left_join(z0, z2_small, by = keys, suffix = c("_0", "_2"))

      for (col in cfg$zone_num_cols) {
        c0 <- paste0(col, "_0")
        c2 <- paste0(col, "_2")
        if (c0 %in% names(joined) && c2 %in% names(joined)) {
          joined[[col]] <- joined[[c0]] - joined[[c2]]
        } else {
          log_fn(sprintf("⚠️ Plate %d - zone column '%s' missing after join; skipping.", i, col))
        }
      }

      zone_data[["1"]] <- joined |>
        dplyr::arrange(.row0) |>
        dplyr::select(
          -.row0,
          -dplyr::any_of(paste0(cfg$zone_num_cols, "_0")),
          -dplyr::any_of(paste0(cfg$zone_num_cols, "_2"))
        )

      log_fn(sprintf("✅ Plate %d - Zone 1 calculated by key-join (no recycling).", i))
    }
  }

  processed_zones <- purrr::map(names(zone_data), function(z) {
    cols_to_select <- unique(c(
      "plate_id", "period", "animal",
      "condition", "condition_grouped", "condition_tagged",
      "period_with_numbers", "period_without_numbers",
      "zone", "start", "end",
      cfg$zone_num_cols
    ))

    zd <- zone_data[[z]] |>
      dplyr::mutate(zone = z) |>
      dplyr::select(dplyr::any_of(cols_to_select))

    log_fn(sprintf("✅ Plate %d - Zone %s processed.", i, z))
    zd
  })

  dplyr::bind_rows(processed_zones)
}

# ----------------------------------------------------------------------
# run_processing  (main orchestrator)
# ----------------------------------------------------------------------

#' Run the full ZebraBox processing pipeline on one experimental session
#'
#' Processes a list of raw XLSX plates through the complete pipeline:
#' optional quantization filter, condition assignment, period assignment,
#' removal steps, numeric conversion, and zone computation.
#'
#' @param raw_xlsx_list Named or unnamed list of raw XLSX data frames
#'   (one per plate, sorted in plate order).
#' @param plate_plans List of plate-plan data frames in the same order as
#'   `raw_xlsx_list` (output of [create_plate_plan()] or imported).
#' @param period_df Data frame with `start` and `transition` columns
#'   (output of reading the period-transitions Excel file).
#' @param removal_df Data frame with `plate_id` and optional removal
#'   columns (output of reading the removal-specifications Excel file).
#' @param cfg Processing config list returned by [get_processing_config()].
#' @param log_fn Function accepting a single character string; called for
#'   progress and diagnostic messages. Default: silent.
#' @return A named list:
#'   \describe{
#'     \item{processed_data_list}{List of processed XLSX data frames, one
#'       per plate.}
#'     \item{boundary_associations_list}{List of boundary data frames
#'       (plate_id, time_switch, transition).}
#'   }
#' @seealso [set_mode()], [read_raw_files()], [prepare_dataset()],
#'   [export_results()]
#' @export
run_processing <- function(raw_xlsx_list, plate_plans, period_df, removal_df, cfg,
                           log_fn = function(msg) invisible(NULL),
                           progress_fn = function(i, n, name) invisible(NULL)) {

  n_plates <- length(plate_plans)
  processed_data_list        <- vector("list", n_plates)
  boundary_associations_list <- vector("list", n_plates)

  log_fn("==================================================")
  log_fn("🚀 Run processing started.")
  log_fn(sprintf("Config: %s", cfg$ui_title %||% "<no ui_title>"))
  log_fn(sprintf("Plates detected (.xlsx): %d", n_plates))
  log_fn("==================================================")
  log_fn("🔄 Starting XLSX: extraction → condition mapping → period assignment → removals → zones…")

  for (i in seq_len(n_plates)) {
    log_fn("-----")
    log_fn(sprintf("📄 XLSX | Plate %d/%d", i, n_plates))
    log_fn("-----")

    current_plan <- plate_plans[[i]]
    current_data <- raw_xlsx_list[[i]]

    log_fn(sprintf("Raw XLSX columns: %s", paste(names(current_data), collapse = ", ")))
    log_fn(sprintf("Plate plan columns: %s", paste(names(current_plan), collapse = ", ")))

    if (!is.null(cfg$filter_fn) && is.function(cfg$filter_fn)) {
      log_fn(sprintf("🔎 Plate %d - Quantization filter enabled (cfg$filter_fn). Applying…", i))
      current_data <- cfg$filter_fn(current_data)
      log_fn(sprintf("✅ Plate %d - Quantization filter applied.", i))
    } else {
      log_fn(sprintf("ℹ️ Plate %d - No quantization filter applied (cfg$filter_fn not set for this mode).", i))
    }

    required_columns <- c("animal", "condition", "plate_id")
    missing_cols <- setdiff(required_columns, names(current_plan))
    if (length(missing_cols) > 0) {
      stop(sprintf("Plate %d is missing required columns: %s", i, paste(missing_cols, collapse = ", ")))
    }
    log_fn(sprintf("✅ Plate %d - Plate plan validated.", i))

    log_fn(sprintf("🔄 Plate %d - Assigning conditions (condition, condition_grouped, condition_tagged)…", i))
    cond_res     <- generate_conditions(current_data, current_plan, i, log_fn = log_fn)
    current_data <- cond_res$data
    current_plan <- cond_res$plan
    log_fn(sprintf("✅ Plate %d - Conditions assigned. Columns now: %s", i, paste(names(current_data), collapse = ", ")))

    log_fn(sprintf("🔄 Plate %d - Assigning periods (period_with_numbers, period_without_numbers)…", i))
    per_res      <- assign_periods(current_data, period_df, i, cfg, log_fn = log_fn)
    current_data <- per_res$data
    boundaries   <- per_res$boundaries
    transitions  <- per_res$transitions
    log_fn(sprintf("✅ Plate %d - Periods assigned. Columns now: %s", i, paste(names(current_data), collapse = ", ")))

    plate_id_num <- suppressWarnings(as.numeric(current_plan$plate_id[1]))
    removal_row  <- removal_df[removal_df$plate_id == plate_id_num, , drop = FALSE]

    if (nrow(removal_row) > 0) {
      log_fn(sprintf("🔄 Plate %d - Applying removal specs for plate_id=%s", i, as.character(current_plan$plate_id[1])))
      rr           <- removal_row[1, , drop = FALSE]
      current_data <- remove_time_codes(current_data, rr, i, log_fn = log_fn)
      current_data <- remove_periods(current_data, rr, i, log_fn = log_fn)
      current_data <- remove_wells(current_data, rr, i, log_fn = log_fn)
      current_data <- remove_conditions(current_data, rr, i, log_fn = log_fn)
      log_fn(sprintf("✅ Plate %d - Removal steps completed.", i))
    } else {
      log_fn(sprintf("ℹ️ Plate %d - No removal rules for this plate (continuing).", i))
    }

    log_fn(sprintf("🔄 Plate %d - Converting numeric columns: %s", i, paste(cfg$convert_cols %||% character(), collapse = ", ")))
    current_data <- convert_numeric_cols(current_data, cfg$convert_cols)
    log_fn(sprintf("✅ Plate %d - Numeric conversion done.", i))

    log_fn(sprintf("🔄 Plate %d - Processing zones… (zone derived from raw 'an')", i))
    zone_combined <- process_zones(current_data, i, cfg, log_fn = log_fn)
    processed_data_list[[i]] <- zone_combined
    log_fn(sprintf("✅ Plate %d - Zones processed. Output columns: %s", i, paste(names(zone_combined), collapse = ", ")))

    boundary_associations_list[[i]] <- data.frame(
      plate_id    = as.character(current_plan$plate_id[1]),
      time_switch = boundaries,
      transition  = transitions,
      stringsAsFactors = FALSE
    )

    plate_name <- as.character(attr(raw_xlsx_list[[i]], "file_name") %||% paste("Plate", i))
    progress_fn(i, n_plates, plate_name)
  }

  log_fn("==================================================")
  log_fn("✅ XLSX processing completed for all plates.")
  log_fn(sprintf(
    "Stored: processed_data_list (%d) + boundary_associations_list (%d)",
    length(processed_data_list), length(boundary_associations_list)
  ))
  log_fn("==================================================")

  result <- list(
    processed_data_list        = processed_data_list,
    boundary_associations_list = boundary_associations_list
  )

  log_fn("🏁 Processing finished.")
  result
}

# Internal pipe alias used only within this file
`%||%` <- function(a, b) if (is.null(a)) b else a
