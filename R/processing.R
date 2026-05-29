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

zip_base_name <- function(x) {
  x <- basename(x)
  sub("\\.zip$", "", x, ignore.case = TRUE)
}

xlsx_base_name <- function(x) {
  x <- basename(x)
  sub("\\.(xlsx|xls|csv)$", "", x, ignore.case = TRUE)
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
# match_txt_zip_to_raw_xlsx
# ----------------------------------------------------------------------

#' Match TXT trajectory rows to wells using location prefix
#'
#' Joins the ZIP TXT data frame to the raw XLSX data frame on a 6-character
#' prefix of the `location` / `file_txt_name` columns, attaching the
#' `animal` and `zone` (raw `an`) columns from the XLSX side.
#'
#' @param raw_xlsx Data frame of raw XLSX data. Must contain `location`,
#'   `animal`, and `an` columns.
#' @param zip_txt Data frame of unzipped TXT data. Must contain
#'   `file_txt_name`, `T`, `X`, and `Y` columns.
#' @param n Number of characters used as prefix for matching (default 6).
#' @param log_fn Logging function.
#' @return Data frame with columns `T`, `X`, `Y`, `file_txt_name`,
#'   `animal`, and `zone`.
#' @keywords internal
match_txt_zip_to_raw_xlsx <- function(raw_xlsx, zip_txt, n = 6,
                                      log_fn = function(msg) invisible(NULL)) {
  needed_raw <- c("location", "animal", "an")
  missing_raw <- setdiff(needed_raw, names(raw_xlsx))
  if (length(missing_raw) > 0) {
    stop("Raw .xlsx missing required columns: ", paste(missing_raw, collapse = ", "))
  }

  if (!("file_txt_name" %in% names(zip_txt))) stop("ZIP txt df missing 'file_txt_name' column.")
  needed_txt <- c("T", "X", "Y")
  missing_txt <- setdiff(needed_txt, names(zip_txt))
  if (length(missing_txt) > 0) stop("ZIP txt df missing required columns: ", paste(missing_txt, collapse = ", "))

  raw_map <- raw_xlsx |>
    dplyr::mutate(
      loc6   = substr(as.character(.data$location), 1, n),
      animal = trimws(as.character(.data$animal)),
      zone   = as.character(.data$an)
    ) |>
    dplyr::filter(
      !is.na(.data$loc6), .data$loc6 != "",
      !is.na(.data$animal), .data$animal != ""
    ) |>
    dplyr::distinct(.data$loc6, .data$animal, .data$zone)

  amb <- raw_map |>
    dplyr::count(.data$loc6, name = "n_animals") |>
    dplyr::filter(.data$n_animals > 1)

  if (nrow(amb) > 0) {
    log_fn("⚠️ Warning: some location prefixes map to multiple DIFFERENT animals (keeping first).")
    raw_map <- raw_map |>
      dplyr::group_by(.data$loc6) |>
      dplyr::slice(1) |>
      dplyr::ungroup()
  }

  txt2 <- zip_txt |>
    dplyr::mutate(txt6 = substr(as.character(.data$file_txt_name), 1, n)) |>
    dplyr::left_join(raw_map, by = c("txt6" = "loc6"))

  txt2 |>
    dplyr::select("T", "X", "Y", "file_txt_name", "animal", "zone")
}

# ----------------------------------------------------------------------
# assign_periods_to_txt
# ----------------------------------------------------------------------

#' Assign period labels to TXT trajectory data
#'
#' Uses the same period-transition file as [assign_periods()] but
#' operates on a TXT data frame whose time column is `T`.
#'
#' @param txt_df Data frame with a `T` column (numeric seconds).
#' @param period_df Data frame with `start` and `transition` columns.
#' @param cfg Processing config list; may contain a `period_map` function.
#' @return The data frame with `period_with_numbers` and
#'   `period_without_numbers` columns added.
#' @keywords internal
assign_periods_to_txt <- function(txt_df, period_df, cfg) {
  txt_df$start <- suppressWarnings(as.numeric(txt_df$T))
  txt_df$period_with_numbers <- NA_character_

  split_list <- strsplit(as.character(period_df$transition), "-", fixed = TRUE)
  split_list <- lapply(split_list, function(parts) {
    if (length(parts) == 1) c(parts, parts)
    else if (length(parts) == 2) parts
    else stop("Each transition must contain a single hyphen.")
  })

  for (j in seq_len(nrow(period_df))) {
    start_time <- period_df$start[j]
    parts      <- split_list[[j]]
    before     <- parts[1]
    after      <- parts[2]

    if (j == 1) {
      txt_df$period_with_numbers[txt_df$start < start_time] <- before
    }
    if (j < nrow(period_df)) {
      next_start <- period_df$start[j + 1]
      txt_df$period_with_numbers[txt_df$start >= start_time & txt_df$start < next_start] <- after
    } else {
      txt_df$period_with_numbers[txt_df$start >= start_time] <- after
    }
  }

  if (!is.null(cfg$period_map) && is.function(cfg$period_map)) {
    txt_df$period_without_numbers <- cfg$period_map(txt_df$period_with_numbers)
  } else {
    txt_df$period_without_numbers <- txt_df$period_with_numbers
  }

  txt_df
}

# ----------------------------------------------------------------------
# run_processing  (main orchestrator)
# ----------------------------------------------------------------------

#' Run the full ZebraBox processing pipeline on one experimental session
#'
#' Processes a list of raw XLSX plates through the complete pipeline:
#' optional quantization filter, condition assignment, period assignment,
#' removal steps, numeric conversion, and zone computation. Optionally
#' processes matching ZIP/TXT trajectory files.
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
#' @param raw_zip_list Optional list of unzipped TXT data frames (one per
#'   ZIP archive). When provided, the ZIP/TXT pipeline runs after XLSX
#'   processing.
#' @param log_fn Function accepting a single character string; called for
#'   progress and diagnostic messages. Default: silent.
#' @return A named list:
#'   \describe{
#'     \item{processed_data_list}{List of processed XLSX data frames, one
#'       per plate.}
#'     \item{boundary_associations_list}{List of boundary data frames
#'       (plate_id, time_switch, transition).}
#'     \item{txt_by_plate}{(Only if `raw_zip_list` provided) List of TXT
#'       data frames, indexed by plate.}
#'     \item{txt_all}{(Only if `raw_zip_list` provided) All TXT rows
#'       row-bound.}
#'     \item{zip_xlsx_match}{(Only if `raw_zip_list` provided) ZIP↔XLSX
#'       name-matching table.}
#'   }
#' @seealso [set_mode()], [read_raw_files()], [prepare_dataset()],
#'   [export_results()]
#' @export
run_processing <- function(raw_xlsx_list, plate_plans, period_df, removal_df, cfg,
                           raw_zip_list = NULL,
                           log_fn = function(msg) invisible(NULL),
                           progress_fn = function(i, n, name) invisible(NULL)) {

  n_plates <- length(plate_plans)
  processed_data_list        <- vector("list", n_plates)
  boundary_associations_list <- vector("list", n_plates)

  log_fn("==================================================")
  log_fn("🚀 Run processing started.")
  log_fn(sprintf("Config: %s", cfg$ui_title %||% "<no ui_title>"))
  log_fn(sprintf("Plates detected (.xlsx): %d", n_plates))
  log_fn(sprintf("ZIP archives detected (.zip): %d", if (is.null(raw_zip_list)) 0L else length(raw_zip_list)))
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

  # ---- TXT ZIP post-processing ----
  if (!is.null(raw_zip_list) && length(raw_zip_list) > 0) {

    zip_list  <- raw_zip_list
    xlsx_list <- raw_xlsx_list

    zip_names  <- vapply(zip_list,  function(x) as.character((attr(x, "file_name") %||% "")), FUN.VALUE = character(1))
    xlsx_names <- vapply(xlsx_list, function(x) as.character((attr(x, "file_name") %||% "")), FUN.VALUE = character(1))

    log_fn("-----")
    log_fn("📦 TXT/ZIP post-processing started.")
    log_fn(sprintf("ZIP files: %s",  paste(zip_names,  collapse = " | ")))
    log_fn(sprintf("XLSX files: %s", paste(xlsx_names, collapse = " | ")))

    zip_bases  <- zip_base_name(zip_names)
    xlsx_bases <- xlsx_base_name(xlsx_names)

    idx_xlsx_for_zip <- match(zip_bases, xlsx_bases)
    if (any(is.na(idx_xlsx_for_zip))) {
      missing <- zip_names[is.na(idx_xlsx_for_zip)]
      stop(
        "Processing stopped: no exact match between ZIP and XLSX names for: ",
        paste(missing, collapse = ", "),
        ".\nExpected ZIP base name to exactly match a raw_data .xlsx base name."
      )
    }

    log_fn("✅ ZIP ↔ XLSX base-name matching OK.")
    log_fn(sprintf("ZIP bases:  %s", paste(zip_bases,          collapse = " | ")))
    log_fn(sprintf("XLSX bases: %s", paste(xlsx_bases,         collapse = " | ")))
    log_fn(sprintf("Index map (zip->plate_idx): %s", paste(idx_xlsx_for_zip, collapse = ", ")))

    txt_by_plate <- vector("list", n_plates)
    names(txt_by_plate) <- paste0("Plate_", seq_along(txt_by_plate))

    zip_xlsx_match <- data.frame(
      zip_name  = zip_names,
      xlsx_name = xlsx_names[idx_xlsx_for_zip],
      plate_idx = idx_xlsx_for_zip,
      stringsAsFactors = FALSE
    )

    for (k in seq_along(zip_list)) {
      plate_idx  <- idx_xlsx_for_zip[k]
      raw_xlsx_k <- xlsx_list[[plate_idx]]
      zip_txt_k  <- zip_list[[k]]

      log_fn("-----")
      log_fn(sprintf("🧾 TXT | ZIP %d/%d", k, length(zip_list)))
      log_fn(sprintf("ZIP file:  %s", zip_names[k]))
      log_fn(sprintf("XLSX link: %s (plate_idx=%d)", xlsx_names[plate_idx], plate_idx))
      log_fn(sprintf("Raw XLSX columns (for TXT mapping): %s", paste(names(raw_xlsx_k), collapse = ", ")))
      log_fn(sprintf("ZIP TXT columns: %s", paste(names(zip_txt_k), collapse = ", ")))

      log_fn("🔄 Step 1/3: Match wells by 6-char prefix (location ↔ file_txt_name) + attach animal + zone (raw 'an') …")
      txt_k <- match_txt_zip_to_raw_xlsx(raw_xlsx_k, zip_txt_k, n = 6, log_fn = log_fn)
      log_fn(sprintf("✅ Step 1 complete. TXT columns now: %s", paste(names(txt_k), collapse = ", ")))
      n_total          <- nrow(txt_k)
      n_missing_animal <- sum(is.na(txt_k$animal) | txt_k$animal == "")
      n_missing_zone   <- sum(is.na(txt_k$zone)   | txt_k$zone   == "")
      log_fn(sprintf("Rows: %d | Missing animal: %d | Missing zone: %d", n_total, n_missing_animal, n_missing_zone))

      log_fn("🔄 Step 2/3: Enrich TXT with plate meta (plate_id + conditions) from processed XLSX …")
      proc_k <- result$processed_data_list[[plate_idx]]

      meta_k <- proc_k |>
        dplyr::select(
          "plate_id", "animal",
          "condition", "condition_grouped", "condition_tagged"
        ) |>
        dplyr::mutate(
          animal            = trimws(as.character(.data$animal)),
          plate_id          = as.character(.data$plate_id),
          condition         = as.character(.data$condition),
          condition_grouped = as.character(.data$condition_grouped),
          condition_tagged  = as.character(.data$condition_tagged)
        ) |>
        dplyr::group_by(.data$animal) |>
        dplyr::slice(1) |>
        dplyr::ungroup()

      log_fn(sprintf("Meta columns attached: %s", paste(names(meta_k), collapse = ", ")))

      txt_k <- txt_k |>
        dplyr::mutate(animal = trimws(as.character(.data$animal))) |>
        dplyr::left_join(meta_k, by = "animal", relationship = "many-to-one")

      log_fn(sprintf("✅ Step 2 complete. TXT columns now: %s", paste(names(txt_k), collapse = ", ")))
      n_enriched     <- sum(!is.na(txt_k$plate_id) & txt_k$plate_id != "")
      n_not_enriched <- nrow(txt_k) - n_enriched
      log_fn(sprintf("Enriched rows (plate_id filled): %d | Missing meta: %d", n_enriched, n_not_enriched))

      if (n_not_enriched > 0) {
        bad_animals <- txt_k |>
          dplyr::filter(is.na(.data$plate_id) | .data$plate_id == "") |>
          dplyr::distinct(.data$animal) |>
          dplyr::pull(.data$animal)
        log_fn(sprintf(
          "⚠️ Missing meta for animals (sample): %s",
          paste(utils::head(bad_animals, 10), collapse = ", ")
        ))
      }

      log_fn("🧹 Step 2b/3: Removing TXT rows for wells removed during XLSX processing…")
      valid_animals  <- unique(meta_k$animal)
      n_before_clean <- nrow(txt_k)
      txt_k <- txt_k |>
        dplyr::filter(!is.na(.data$animal), .data$animal != "", .data$animal %in% valid_animals)
      n_after_clean <- nrow(txt_k)
      log_fn(sprintf(
        "✅ TXT cleanup done: removed %d rows (animals not present in processed XLSX). Remaining: %d rows.",
        n_before_clean - n_after_clean, n_after_clean
      ))

      log_fn("🔄 Step 3/3: Assign periods from T using the same transitions file …")
      txt_k <- assign_periods_to_txt(txt_k, period_df, cfg)
      log_fn("✅ Step 3 complete. Period columns attached: period_with_numbers, period_without_numbers")
      log_fn(sprintf("Final TXT columns (before select): %s", paste(names(txt_k), collapse = ", ")))

      txt_k <- txt_k |>
        dplyr::select(
          "T", "X", "Y", "file_txt_name", "animal",
          "plate_id", "condition", "condition_grouped", "condition_tagged",
          "period_with_numbers", "period_without_numbers",
          "zone"
        )

      txt_by_plate[[plate_idx]] <- dplyr::bind_rows(txt_by_plate[[plate_idx]], txt_k)

      log_fn(sprintf(
        "✅ TXT stored for Plate %d: %d rows added | Total plate txt rows: %d",
        plate_idx, nrow(txt_k), nrow(txt_by_plate[[plate_idx]])
      ))
    }

    result$zip_xlsx_match <- zip_xlsx_match
    result$txt_by_plate   <- txt_by_plate
    result$txt_all        <- dplyr::bind_rows(txt_by_plate)

    log_fn("==================================================")
    log_fn("✅ TXT results stored:")
    log_fn(sprintf("- txt_by_plate: %d plates",  length(txt_by_plate)))
    log_fn(sprintf("- txt_all: %d rows",          nrow(result$txt_all)))
    log_fn(sprintf("- zip_xlsx_match: %d rows",   nrow(zip_xlsx_match)))
    log_fn("==================================================")

  } else {
    log_fn("ℹ️ No ZIP provided: skipping TXT post-processing.")
  }

  log_fn("🏁 Processing finished.")
  result
}

# Internal pipe alias used only within this file
`%||%` <- function(a, b) if (is.null(a)) b else a
