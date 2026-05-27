#' Create a randomised plate plan
#'
#' Generates one or more randomised plate plans for ZebraBox experiments,
#' distributing conditions and replicates equitably across plates while
#' minimising positional bias.
#'
#' @param plate_type Wells per plate: \code{"12"}, \code{"24"}, \code{"48"},
#'   or \code{"96"}.
#' @param conditions Character vector of condition names
#'   (e.g. \code{c("ctrl", "trt_A", "trt_B")}).
#' @param n_replicates Integer. Number of replicates per condition.
#' @param n_units Integer. Number of wells per replicate.
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
#'     \item{condition}{Assigned condition label (e.g. \code{"ctrl_1"}) or
#'       \code{"X"} for unassigned / border wells.}
#'     \item{plate_id}{Character plate index (\code{"1"}, \code{"2"}, …).}
#'   }
#'   The \code{file_name} attribute of each data frame is set to
#'   \code{"<name>_plate_<i>.xlsx"}.
#'
#' @export
#' @examples
#' plans <- create_plate_plan(
#'   plate_type      = "96",
#'   conditions      = c("ctrl", "trt"),
#'   n_replicates    = 3,
#'   n_units         = 8,
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
    n_replicates,
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

  n_replicates <- as.integer(n_replicates)
  n_units      <- as.integer(n_units)
  n_plates     <- as.integer(n_plates)

  if (is.na(n_replicates) || n_replicates < 1L) stop("'n_replicates' must be a positive integer.")
  if (is.na(n_units)      || n_units < 1L)      stop("'n_units' must be a positive integer.")
  if (is.na(n_plates)     || n_plates < 1L)      stop("'n_plates' must be a positive integer.")

  # ---- Plate geometry --------------------------------------------------------
  well_configs <- list(
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
  cfg <- well_configs[[plate_type]]

  wells_template <- with(
    expand.grid(Row = cfg$rows, Column = cfg$cols),
    paste0(Row, sprintf("%02d", Column))
  )

  available_wells <- if (!isTRUE(include_borders))
    setdiff(wells_template, cfg$border)
  else
    wells_template

  available_total <- length(available_wells) * n_plates
  total_units     <- length(conditions) * n_replicates * n_units

  if (total_units > available_total)
    stop(sprintf(
      "Total units (%d) exceed available wells (%d across %d plate(s)).",
      total_units, available_total, n_plates
    ))

  # ---- Distribute conditions evenly across plates ----------------------------
  cond_by_plate <- lapply(conditions, function(cond) {
    total_c <- n_replicates * n_units
    base    <- floor(total_c / n_plates)
    rem     <- total_c %% n_plates
    counts  <- rep(base, n_plates)
    if (rem > 0L) counts[seq_len(rem)] <- counts[seq_len(rem)] + 1L
    counts
  })
  names(cond_by_plate) <- conditions

  # avail_idx is constant across plates — computed once
  avail_idx <- if (!isTRUE(include_borders))
    which(!wells_template %in% cfg$border)
  else
    seq_along(wells_template)

  # ---- Build one data frame per plate ----------------------------------------
  set.seed(seed)

  plate_plan_list <- vector("list", n_plates)

  for (i in seq_len(n_plates)) {

    labels <- unlist(mapply(
      function(cond, counts) {
        unlist(mapply(
          function(r, cnt) rep(paste0(cond, "_", r), cnt),
          seq_len(n_replicates),
          rep(counts[i] / n_replicates, n_replicates),
          SIMPLIFY = FALSE
        ))
      },
      conditions, cond_by_plate,
      SIMPLIFY = FALSE
    ))

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
#' @export
detect_plate_type <- function(df) {
  wells <- sub("_plate_.*", "", df$animal)
  rows  <- unique(sub("([A-Z]).*",     "\\1", wells))
  cols  <- unique(as.integer(sub("[A-Z](\\d{2})", "\\1", wells)))
  total <- length(rows) * length(cols)
  if (total %in% c(12L, 24L, 48L, 96L)) as.integer(total) else NA_integer_
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
#' @export
get_well_config <- function(plate_type) {
  configs <- list(
    "12" = list(rows = LETTERS[1:3],  cols = 1:4),
    "24" = list(rows = LETTERS[1:4],  cols = 1:6),
    "48" = list(rows = LETTERS[1:6],  cols = 1:8),
    "96" = list(rows = LETTERS[1:8],  cols = 1:12)
  )
  cfg <- configs[[as.character(plate_type)]]
  if (is.null(cfg))
    stop("'plate_type' must be one of: \"12\", \"24\", \"48\", \"96\".")
  cfg
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
#' @export
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
#' @export
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
#' @export
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
#' @param paths Character vector of file paths (e.g. Shiny's
#'   `input$files$datapath`).
#' @param names Character vector of display names the same length as
#'   `paths` (e.g. `input$files$name`).
#' @return A list of data frames, one per file, in alphabetical order of
#'   `names`.
#' @export
read_plate_plan_files <- function(paths, names) {
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
