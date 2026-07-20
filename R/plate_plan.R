#' Create a randomised plate plan
#'
#' Generates one or more randomised plate plans for ZebraBox experiments,
#' distributing each condition's wells evenly across plates (difference of
#' at most one well per plate) while minimising positional bias.
#'
#' @param plate_type Wells per plate: \code{"12"}, \code{"24"}, \code{"48"},
#'   or \code{"96"}.
#' @param conditions Character vector of condition names
#'   (e.g. \code{c("ctrl", "trt_A", "trt_B")}).
#' @param n_units Integer. Number of wells (units) per condition.
#' @param n_plates Integer. Number of plates to generate. Default \code{1}.
#' @param include_borders Logical. \code{TRUE} (default) uses all wells;
#'   \code{FALSE} excludes the outer ring of border wells from assignment
#'   (they receive condition \code{"X"}).
#' @param seed Numeric. Random seed for reproducibility. Default \code{42}.
#' @param name Character. Base name used to build the \code{file_name}
#'   attribute of each returned data frame
#'   (\code{"<name>_plate_<i>.xlsx"}). Default \code{"plate_plan"}.
#'
#' @return A named list of \code{n_plates} data frames, one per plate.
#'   Each data frame contains:
#'   \describe{
#'     \item{animal}{Well identifier, e.g. \code{"A01_plate_1"}.}
#'     \item{condition}{Assigned condition label (e.g. \code{"ctrl"}) or
#'       \code{"X"} for unassigned / border wells.}
#'     \item{plate_id}{Character plate index (\code{"1"}, \code{"2"}, …).}
#'   }
#'   The \code{file_name} attribute of each data frame is set to
#'   \code{"<name>_plate_<i>.xlsx"}.
#'
#' @seealso [read_plate_plan_files()], [run_processing()]
#' @export
#' @examples
#' plans <- create_plate_plan(
#'   plate_type      = "96",
#'   conditions      = c("ctrl", "trt"),
#'   n_units         = 24,
#'   n_plates        = 2,
#'   include_borders = FALSE,
#'   seed            = 42
#' )
#' length(plans)       # 2
#' names(plans[[1]])   # "animal" "condition" "plate_id"
#' table(plans[[1]]$condition)
create_plate_plan <- function(
    plate_type,
    conditions,
    n_units,
    n_plates        = 1L,
    include_borders = TRUE,
    seed            = 42,
    name            = "plate_plan"
) {
  # ---- Input validation ------------------------------------------------------
  plate_type <- as.character(plate_type)
  if (!plate_type %in% c("12", "24", "48", "96"))
    stop("'plate_type' must be one of: \"12\", \"24\", \"48\", \"96\".")

  conditions <- as.character(conditions)
  if (length(conditions) == 0 || any(!nzchar(trimws(conditions))))
    stop("'conditions' must be a non-empty character vector with no blank entries.")

  n_units  <- as.integer(n_units)
  n_plates <- as.integer(n_plates)

  if (is.na(n_units)  || n_units < 1L)  stop("'n_units' must be a positive integer.")
  if (is.na(n_plates) || n_plates < 1L) stop("'n_plates' must be a positive integer.")

  # ---- Plate geometry --------------------------------------------------------
  cfg             <- plate_well_configs(plate_type)
  wells_template  <- plate_wells_template(plate_type)
  available_wells <- plate_available_wells(plate_type, include_borders)

  available_total <- length(available_wells) * n_plates
  total_units     <- length(conditions) * n_units

  if (total_units > available_total)
    stop(sprintf(
      "Total units (%d) exceed available wells (%d across %d plate(s)).",
      total_units, available_total, n_plates
    ))

  # ---- Distribute conditions evenly across plates ----------------------------
  # Each condition's wells are split across plates as evenly as possible
  # (at most one well of difference between plates).
  cond_by_plate <- lapply(conditions, function(cond) {
    base    <- floor(n_units / n_plates)
    rem     <- n_units %% n_plates
    counts  <- rep(base, n_plates)
    if (rem > 0L) counts[seq_len(rem)] <- counts[seq_len(rem)] + 1L
    counts
  })
  names(cond_by_plate) <- conditions

  # Flat label sequence per condition: the condition name repeated n_units times.
  full_labels <- lapply(conditions, function(cond) rep(cond, n_units))
  names(full_labels) <- conditions

  # Slice each condition's label sequence across plates using cond_by_plate.
  # This guarantees every unit is placed on exactly one plate.
  labels_by_plate <- vector("list", n_plates)
  for (cond in conditions) {
    counts <- cond_by_plate[[cond]]
    ends   <- cumsum(counts)
    starts <- c(1L, ends[-length(ends)] + 1L)
    lbl    <- full_labels[[cond]]
    for (i in seq_len(n_plates)) {
      if (counts[i] > 0L) {
        labels_by_plate[[i]] <- c(labels_by_plate[[i]], lbl[starts[i]:ends[i]])
      }
    }
  }

  # avail_idx is constant across plates — computed once
  avail_idx <- if (!isTRUE(include_borders))
    which(!wells_template %in% cfg$border)
  else
    seq_along(wells_template)

  # ---- Build one data frame per plate ----------------------------------------
  set.seed(seed)

  plate_plan_list <- vector("list", n_plates)

  for (i in seq_len(n_plates)) {

    labels <- labels_by_plate[[i]]

    plate_assign             <- rep("X", length(wells_template))
    choice_idx               <- sample(avail_idx, length(labels))
    plate_assign[choice_idx] <- labels

    df <- data.frame(
      animal    = paste0(wells_template, "_plate_", i),
      condition = plate_assign,
      plate_id  = as.character(i),
      stringsAsFactors = FALSE
    )
    attr(df, "file_name") <- sprintf("%s_plate_%d.xlsx", name, i)

    plate_plan_list[[i]] <- df
  }

  plate_plan_list
}

# ======================================================================
# Plate-plan utility functions
# Used by create_plate_plan() and by inst/app/modules/import_generate_plate_plan.R
# ======================================================================

#' Detect plate format from a plate plan data frame
#'
#' Infers the number of wells (12, 24, 48 or 96) by counting the unique
#' row letters and column indices found in the `animal` column.
#'
#' @param df Data frame with an `animal` column following the
#'   `<row><col>_plate_<i>` convention (e.g. `"A01_plate_1"`).
#' @return Integer scalar: 12L, 24L, 48L or 96L. Returns `NA_integer_`
#'   when the layout cannot be identified.
#' @keywords internal
detect_plate_type <- function(df) {
  wells <- sub("_plate_.*", "", df$animal)
  rows  <- unique(sub("([A-Z]).*",     "\\1", wells))
  cols  <- unique(as.integer(sub("[A-Z](\\d{2})", "\\1", wells)))
  total <- length(rows) * length(cols)
  if (total %in% c(12L, 24L, 48L, 96L)) as.integer(total) else NA_integer_
}

#' Plate geometry configuration (rows, columns, border wells)
#'
#' Single source of truth for plate layout, used by [create_plate_plan()],
#' [get_well_config()], [plate_wells_template()], [plate_available_wells()]
#' and [min_plates_needed()].
#'
#' @param plate_type Character (or integer coercible to character):
#'   `"12"`, `"24"`, `"48"`, or `"96"`.
#' @return Named list with `rows` (character vector of row letters), `cols`
#'   (integer vector of column indices) and `border` (character vector of
#'   border well IDs, e.g. `"A01"`).
#' @keywords internal
plate_well_configs <- function(plate_type) {
  plate_type <- as.character(plate_type)
  configs <- list(
    "12" = list(
      rows   = LETTERS[1:3],
      cols   = 1:4,
      border = c("A01","A02","A03","A04",
                 "B01","B04",
                 "C01","C02","C03","C04")
    ),
    "24" = list(
      rows   = LETTERS[1:4],
      cols   = 1:6,
      border = c("A01","A02","A03","A04","A05","A06",
                 "B01","B06",
                 "C01","C06",
                 "D01","D02","D03","D04","D05","D06")
    ),
    "48" = list(
      rows   = LETTERS[1:6],
      cols   = 1:8,
      border = c("A01","A02","A03","A04","A05","A06","A07","A08",
                 "B01","B08",
                 "C01","C08",
                 "D01","D08",
                 "E01","E08",
                 "F01","F02","F03","F04","F05","F06","F07","F08")
    ),
    "96" = list(
      rows   = LETTERS[1:8],
      cols   = 1:12,
      border = c(
        paste0("A", sprintf("%02d", 1:12)),
        paste0(rep(c("B","C","D","E","F","G"), each = 2), sprintf("%02d", c(1, 12))),
        paste0("H", sprintf("%02d", 1:12))
      )
    )
  )
  cfg <- configs[[plate_type]]
  if (is.null(cfg))
    stop("'plate_type' must be one of: \"12\", \"24\", \"48\", \"96\".")
  cfg
}

#' All well identifiers for a plate format, in row-major order
#'
#' @inheritParams plate_well_configs
#' @return Character vector of well IDs (e.g. `"A01"`, `"A02"`, ...).
#' @keywords internal
plate_wells_template <- function(plate_type) {
  cfg <- plate_well_configs(plate_type)
  with(
    expand.grid(Row = cfg$rows, Column = cfg$cols),
    paste0(Row, sprintf("%02d", Column))
  )
}

#' Wells available for assignment on a plate
#'
#' @inheritParams plate_well_configs
#' @param include_borders Logical. `TRUE` (default) returns all wells;
#'   `FALSE` excludes the outer ring of border wells.
#' @return Character vector of available well IDs.
#' @keywords internal
plate_available_wells <- function(plate_type, include_borders = TRUE) {
  cfg   <- plate_well_configs(plate_type)
  wells <- plate_wells_template(plate_type)
  if (!isTRUE(include_borders)) setdiff(wells, cfg$border) else wells
}

#' Minimum number of plates needed for a plate plan
#'
#' Computes how many plates are required to hold every requested condition
#' unit, given the plate format and whether border wells are excluded. Used
#' to offer plate-count compaction after an over-provisioned generation
#' (e.g. a "Compact Plates" action in the Shiny app) — see
#' [create_plate_plan()].
#'
#' @inheritParams create_plate_plan
#' @return Integer scalar: the minimum feasible `n_plates`.
#' @keywords internal
min_plates_needed <- function(plate_type, conditions, n_units, include_borders = TRUE) {
  available_wells <- plate_available_wells(plate_type, include_borders)
  total_units     <- length(conditions) * as.integer(n_units)
  as.integer(ceiling(total_units / length(available_wells)))
}

#' Find a plate able to absorb every later plate's assigned wells
#'
#' Scans plates in order and returns the first plate index \code{n} such
#' that every assigned well (i.e. not \code{"X"}) on plates
#' \code{n + 1, n + 2, ..., length(plate_list)} fits into plate \code{n}'s
#' currently unassigned, assignable wells. Used to drive the "Compact
#' Plates" action in the Shiny app: rather than re-randomising a whole new
#' layout, the trailing plates' existing wells are physically relocated
#' onto plate \code{n}.
#'
#' @param plate_list List of plate plan data frames (as returned by
#'   [create_plate_plan()]), in plate order. Each must have a \code{condition}
#'   column.
#' @inheritParams create_plate_plan
#' @return Integer plate index \code{n}, or \code{NA_integer_} if no plate
#'   can absorb all the plates after it.
#' @keywords internal
find_compaction_target <- function(plate_list, plate_type, include_borders = TRUE) {
  n_plates <- length(plate_list)
  if (n_plates < 2L) return(NA_integer_)

  wells_template <- plate_wells_template(plate_type)
  avail_idx <- if (!isTRUE(include_borders))
    which(!wells_template %in% plate_well_configs(plate_type)$border)
  else
    seq_along(wells_template)

  assigned_counts <- vapply(plate_list, function(df) sum(df$condition != "X"), integer(1))

  for (n in seq_len(n_plates - 1L)) {
    free_n <- sum(plate_list[[n]]$condition[avail_idx] == "X")
    rest   <- sum(assigned_counts[(n + 1L):n_plates])
    if (rest <= free_n) return(as.integer(n))
  }
  NA_integer_
}

#' Compact a list of plate plans by relocating trailing plates
#'
#' Uses [find_compaction_target()] to find a plate able to absorb every
#' assigned well from the plates after it, then moves those wells onto its
#' free, assignable wells and drops the now-empty trailing plates. Existing
#' assignments — on the target plate and on any plates before it — are left
#' untouched. Returns \code{plate_list} unchanged if no compaction is
#' possible.
#'
#' @inheritParams find_compaction_target
#' @param seed Numeric. Random seed used to place the relocated wells.
#' @return A list of plate plan data frames, the same length as
#'   \code{plate_list} or shorter.
#' @keywords internal
compact_plate_plans <- function(plate_list, plate_type, include_borders = TRUE, seed = 42) {
  target <- find_compaction_target(plate_list, plate_type, include_borders)
  if (is.na(target)) return(plate_list)

  wells_template <- plate_wells_template(plate_type)
  avail_idx <- if (!isTRUE(include_borders))
    which(!wells_template %in% plate_well_configs(plate_type)$border)
  else
    seq_along(wells_template)

  merged_df <- plate_list[[target]]
  free_idx  <- avail_idx[merged_df$condition[avail_idx] == "X"]

  extra_labels <- unlist(lapply(
    plate_list[(target + 1L):length(plate_list)],
    function(df) df$condition[df$condition != "X"]
  ))

  set.seed(seed)
  chosen <- sample(free_idx, length(extra_labels))
  merged_df$condition[chosen] <- extra_labels

  result <- plate_list[seq_len(target)]
  result[[target]] <- merged_df
  result
}

#' Return row/column layout for a given plate format
#'
#' @param plate_type Character (or integer coercible to character):
#'   `"12"`, `"24"`, `"48"`, or `"96"`.
#' @return Named list with:
#'   \describe{
#'     \item{`rows`}{Character vector of row letters.}
#'     \item{`cols`}{Integer vector of column indices.}
#'   }
#' @keywords internal
get_well_config <- function(plate_type) {
  cfg <- plate_well_configs(plate_type)
  list(rows = cfg$rows, cols = cfg$cols)
}

#' Add well coordinate columns to a plate plan data frame
#'
#' Parses the `animal` column to extract the row letter, column index and
#' bare well identifier. Handles plate indices with any number of digits
#' (e.g. `"_plate_10"`).
#'
#' @param df Data frame with an `animal` column following the
#'   `<row><col>_plate_<i>` convention.
#' @return `df` with three additional columns:
#'   \describe{
#'     \item{`Row`}{Single uppercase letter (e.g. `"A"`).}
#'     \item{`Column`}{Integer column index (e.g. `1L`).}
#'     \item{`well_id`}{Well identifier without plate suffix (e.g. `"A01"`).}
#'   }
#' @keywords internal
parse_well_coords <- function(df) {
  df$Row     <- sub("([A-Z])\\d{2}_plate_\\d+", "\\1", df$animal)
  df$Column  <- as.integer(sub("[A-Z](\\d{2})_plate_\\d+", "\\1", df$animal))
  df$well_id <- sub("_plate_.*", "", df$animal)
  df
}

#' Normalise the condition column of a plate plan for display
#'
#' Strips the replicate suffix (e.g. `"ctrl_1"` → `"ctrl"`) and maps
#' empty or `NA` conditions to `"X"`. Handles legacy data frames that carry
#' a `Condition_Base` column instead of `condition`.
#'
#' @param df Data frame with a `condition` column (or `Condition_Base`).
#' @return `df` with a new `Condition` column (character). If
#'   `Condition_Base` was present it is removed.
#' @keywords internal
normalize_plate_conditions <- function(df) {
  if ("Condition_Base" %in% colnames(df)) {
    df$Condition      <- df$Condition_Base
    df$Condition_Base <- NULL
  } else {
    df$Condition <- ifelse(
      is.na(df$condition) | df$condition == "",
      "X",
      sub("_\\d+$", "", df$condition)
    )
  }
  df
}

#' Generate a named color palette for plate conditions
#'
#' Produces one evenly-spaced color per condition via [scales::hue_pal()]
#' plus white (`"#FFFFFF"`) for the sentinel value `"X"` (unassigned wells).
#'
#' @param conditions Character vector of condition names (must not include
#'   `"X"`).
#' @return Named character vector of hex colors; `"X"` maps to `"#FFFFFF"`.
#' @keywords internal
plate_condition_colors <- function(conditions) {
  cols <- scales::hue_pal()(length(conditions))
  stats::setNames(c(cols, "#FFFFFF"), c(conditions, "X"))
}

#' Read and sort plate plan Excel files
#'
#' Reads one or more `.xlsx` plate plan files in a single pass, adds a
#' `plate_id` column when absent, attaches the original filename as the
#' `file_name` attribute, and returns the data frames sorted alphabetically
#' (case-insensitive) by display name — matching the convention used for
#' raw XLSX files throughout the app.
#'
#' @param paths Character vector of file paths.
#' @param names Character vector of display names the same length as
#'   `paths`. Defaults to `basename(paths)`, so it can be omitted in
#'   standalone use. In a Shiny session pass `input$files$name` explicitly.
#' @return A list of data frames, one per file, in alphabetical order of
#'   `names`.
#' @seealso [create_plate_plan()], [run_processing()]
#' @export
read_plate_plan_files <- function(paths, names = basename(paths)) {
  n <- length(paths)
  if (length(names) != n)
    stop("`paths` and `names` must have the same length.")
  if (n == 0L) return(list())

  ord   <- order(tolower(names))
  paths <- paths[ord]
  names <- names[ord]

  lapply(seq_len(n), function(i) {
    df <- as.data.frame(readxl::read_excel(paths[[i]]))
    if (!"plate_id" %in% colnames(df)) df$plate_id <- as.character(i)
    attr(df, "file_name") <- names[[i]]
    df
  })
}
