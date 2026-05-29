# ======================================================================
# R/import.R
# Raw data file reading utilities (no Shiny dependency)
# ======================================================================

#' Read a raw ZebraBox data file
#'
#' Reads a single raw data file and returns a data frame with the original
#' file name stored as an attribute.  This is the canonical reading function
#' used by the Raw Data import module.
#'
#' Supported formats:
#' \describe{
#'   \item{`.xlsx` / `.xls`}{Excel workbook (first sheet). Read with
#'     \pkg{readxl}.}
#'   \item{`.csv`}{Comma- or semicolon-separated values. Read with
#'     \code{\link[data.table]{fread}}, which auto-detects the separator and
#'     handles large files significantly faster than base-R alternatives.}
#'   \item{`.zip`}{Archive containing one `.txt` file per well (Wintrack
#'     trajectory export). Each `.txt` is read with
#'     \code{\link[data.table]{fread}}; a \code{file_txt_name} column is
#'     appended with the bare filename (e.g. \code{"A01"}).}
#' }
#'
#' @param path Character. Absolute path to the file on disk (typically
#'   \code{input$raw_xlsx_files$datapath} in a Shiny session).
#' @param file_name Character. Display name of the file stored as
#'   \code{attr(result, "file_name")}. Defaults to \code{basename(path)},
#'   so it can be omitted in standalone use. In a Shiny session pass
#'   \code{input$raw_xlsx_files$name} explicitly (the upload temp path has
#'   no meaningful basename).
#'
#' @return A data frame. The attribute \code{"file_name"} is set to
#'   \code{file_name}.
#' @seealso [read_raw_files()], [run_processing()]
#' @export
#' @examples
#' \dontrun{
#' df <- read_raw_file("/tmp/plate_1.xlsx", "plate_1.xlsx")
#' attr(df, "file_name")  # "plate_1.xlsx"
#'
#' df <- read_raw_file("/tmp/plate_1.csv", "plate_1.csv")
#' }
read_raw_file <- function(path, file_name = basename(path)) {
  ext <- tolower(tools::file_ext(file_name))

  if (ext %in% c("xlsx", "xls")) {
    df <- as.data.frame(readxl::read_excel(path))
    attr(df, "file_name") <- file_name
    return(df)
  }

  if (ext == "csv") {
    df <- data.table::fread(path, data.table = FALSE, showProgress = FALSE)
    attr(df, "file_name") <- file_name
    return(df)
  }

  if (ext == "zip") {
    tmp <- file.path(
      tempdir(),
      paste0("unz_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
    )
    dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
    utils::unzip(path, exdir = tmp)

    txt_files <- list.files(tmp, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
    if (!length(txt_files)) stop("ZIP contains no .txt files: ", file_name)

    dfs <- lapply(txt_files, function(fp) {
      d <- data.table::fread(fp, data.table = FALSE, showProgress = FALSE)
      d$file_txt_name <- tools::file_path_sans_ext(basename(fp))
      d
    })

    df <- dplyr::bind_rows(dfs)
    attr(df, "file_name") <- file_name
    return(df)
  }

  stop("Unsupported file type (only .xlsx, .xls, .csv, or .zip): ", file_name)
}

#' Read multiple raw ZebraBox data files
#'
#' Convenience wrapper around [read_raw_file()] for batch loading. Reads all
#' files in a single call, sorts them alphabetically by display name
#' (case-insensitive) — matching the convention used for plate plans — and
#' returns a list ready to pass to [run_processing()] as `raw_xlsx_list`.
#'
#' @param paths Character vector of file paths.
#' @param names Character vector of display names the same length as `paths`.
#'   Defaults to `basename(paths)`. Typically omitted in standalone use.
#' @return A named list of data frames in alphabetical order of `names`. Each
#'   data frame carries a `file_name` attribute set to the corresponding
#'   display name.
#' @seealso [read_raw_file()], [run_processing()]
#' @export
#' @examples
#' \dontrun{
#' raw_data <- read_raw_files(c("plate1.xlsx", "plate2.xlsx"))
#' result   <- run_processing(raw_xlsx_list = raw_data, ...)
#' }
read_raw_files <- function(paths, names = basename(paths)) {
  n <- length(paths)
  if (length(names) != n)
    stop("`paths` and `names` must have the same length.")
  if (n == 0L) return(list())

  ord   <- order(tolower(names))
  paths <- paths[ord]
  names <- names[ord]

  lapply(seq_len(n), function(i) read_raw_file(paths[[i]], names[[i]]))
}

#' Create a period-transitions table
#'
#' Builds the `period_df` data frame required by [run_processing()] from
#' plain R vectors, without needing to prepare an Excel file.
#'
#' @param start Numeric vector of transition start times in seconds
#'   (e.g. `c(0, 600, 1200)`). The first value is typically `0`.
#' @param transition Character vector of transition labels the same length as
#'   `start`. Each label must follow the `"periodA-periodB"` convention
#'   (e.g. `"light1-dark1"`). The left part is the period active
#'   *before* the transition, the right part is active *after*.
#' @return A data frame with columns `start` (numeric) and `transition`
#'   (character), ready to be passed to [run_processing()] as `period_df`.
#' @seealso [run_processing()], [create_removal_table()]
#' @export
#' @examples
#' period_df <- create_period_table(
#'   start      = c(0, 600, 1200),
#'   transition = c("light1-dark1", "dark1-light2")
#' )
create_period_table <- function(start, transition) {
  start <- suppressWarnings(as.numeric(start))
  if (any(is.na(start)))
    stop("'start' must be coercible to numeric (seconds). Check for non-numeric values.")

  transition <- as.character(transition)

  if (length(start) == 0L || length(transition) == 0L)
    stop("'start' and 'transition' must each have at least one value.")

  if (length(start) != length(transition))
    stop(sprintf(
      "'start' and 'transition' must have the same length (%d vs %d).",
      length(start), length(transition)
    ))

  bad <- !grepl("-", transition, fixed = TRUE)
  if (any(bad))
    warning(sprintf(
      "Some 'transition' values contain no hyphen and may be malformed (expected 'periodA-periodB'): %s",
      paste(transition[bad], collapse = ", ")
    ))

  data.frame(start = start, transition = transition, stringsAsFactors = FALSE)
}

#' Create a removal-specifications table
#'
#' Builds the `removal_df` data frame required by [run_processing()] from
#' plain R vectors, without needing to prepare an Excel file.
#'
#' Each argument except `plate_id` can be:
#' \itemize{
#'   \item `NA` (default) — skip this removal step for the plate(s).
#'   \item A single string applied to all plates (e.g. `"A01,B02"`).
#'   \item A character vector of length equal to `plate_id`, one spec per
#'     plate.
#' }
#' Multiple items within one spec are comma-separated
#' (e.g. `remove_wells = "A01,B02"`).
#'
#' @param plate_id Integer or character vector of plate identifiers.
#'   Numeric digits are extracted automatically, so `"Plate1"`, `"plate_1"`,
#'   and `1L` all map to plate `1`.
#' @param remove_time_codes Comma-separated numeric time codes (seconds) to
#'   remove. Default `NA` (no removal).
#' @param remove_wells Comma-separated well names to remove
#'   (e.g. `"A01,B02"`). Default `NA`.
#' @param remove_conditions Comma-separated base condition names to remove
#'   (e.g. `"ctrl"`). Default `NA`.
#' @param remove_periods Comma-separated period names (with numbers) to
#'   remove (e.g. `"light1"`). Default `NA`.
#' @return A data frame with columns `plate_id` (numeric), `remove_time_codes`,
#'   `remove_wells`, `remove_conditions`, and `remove_periods` (all character),
#'   ready to be passed to [run_processing()] as `removal_df`.
#' @seealso [run_processing()], [create_period_table()]
#' @export
#' @examples
#' # No removals (all NAs)
#' removal_df <- create_removal_table(plate_id = c(1, 2))
#'
#' # Remove specific wells on plate 1 only
#' removal_df <- create_removal_table(
#'   plate_id     = c(1, 2),
#'   remove_wells = c("A01,B02", NA)
#' )
create_removal_table <- function(
    plate_id,
    remove_time_codes = NA_character_,
    remove_wells      = NA_character_,
    remove_conditions = NA_character_,
    remove_periods    = NA_character_
) {
  if (length(plate_id) == 0L)
    stop("'plate_id' must have at least one entry.")

  plate_id_num <- suppressWarnings(
    as.numeric(gsub("[^0-9]", "", as.character(plate_id)))
  )
  bad_ids <- is.na(plate_id_num)
  if (any(bad_ids))
    stop(sprintf(
      "Could not extract a numeric plate_id from: %s",
      paste(as.character(plate_id)[bad_ids], collapse = ", ")
    ))

  n <- length(plate_id_num)

  recycle_spec <- function(x, arg_name) {
    if (length(x) == 1L) return(rep(x, n))
    if (length(x) == n)  return(x)
    stop(sprintf(
      "'%s' must be length 1 or the same length as plate_id (%d), got %d.",
      arg_name, n, length(x)
    ))
  }

  data.frame(
    plate_id          = plate_id_num,
    remove_time_codes = recycle_spec(remove_time_codes, "remove_time_codes"),
    remove_wells      = recycle_spec(remove_wells,      "remove_wells"),
    remove_conditions = recycle_spec(remove_conditions, "remove_conditions"),
    remove_periods    = recycle_spec(remove_periods,    "remove_periods"),
    stringsAsFactors  = FALSE
  )
}

# ----------------------------------------------------------------------
# export_results
# ----------------------------------------------------------------------

#' Export processed results to disk
#'
#' Writes the `processed_data_list` slot of a [run_processing()] result
#' object to one or more files. In xlsx format all plates are written into
#' a single workbook (`processed_results.xlsx`), one sheet per plate.
#' In csv format each plate is written to a separate file
#' (`plate_<id>.csv`).
#'
#' @param result Named list returned by [run_processing()]. Must contain
#'   a `processed_data_list` slot.
#' @param path Output directory (created automatically if it does not exist).
#'   Default `"."` (current working directory).
#' @param format Output format: `"xlsx"` (default) or `"csv"`.
#' @return Invisibly, the path(s) of the file(s) written.
#' @seealso [run_processing()], [export_figures()]
#' @export
#' @examples
#' \dontrun{
#' result <- run_processing(raw_xlsx_list, plate_plans, period_df,
#'                          removal_df, cfg$processing)
#' export_results(result, path = "output/")
#' export_results(result, path = "output/", format = "csv")
#' }
export_results <- function(result, path = ".", format = "xlsx") {
  if (!is.list(result) || !"processed_data_list" %in% names(result))
    stop("'result' must be the named list returned by run_processing() ",
         "(expected slot: 'processed_data_list').")

  format <- tolower(trimws(as.character(format)))
  if (!format %in% c("xlsx", "csv"))
    stop("'format' must be \"xlsx\" or \"csv\".")

  plates <- result$processed_data_list
  if (length(plates) == 0L) {
    message("No processed data to export.")
    return(invisible(character(0)))
  }

  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  plate_names <- vapply(seq_along(plates), function(i) {
    pid <- unique(as.character(plates[[i]]$plate_id))
    if (length(pid) == 1L && nzchar(pid[1])) paste0("plate_", pid[1]) else paste0("plate_", i)
  }, character(1L))

  if (identical(format, "xlsx")) {
    out_file      <- file.path(path, "processed_results.xlsx")
    sheets        <- lapply(plates, as.data.frame)
    names(sheets) <- substr(plate_names, 1L, 31L)
    openxlsx::write.xlsx(sheets, out_file, overwrite = TRUE)
    return(invisible(out_file))
  }

  out_files <- character(length(plates))
  for (i in seq_along(plates)) {
    fname       <- file.path(path, paste0(plate_names[i], ".csv"))
    utils::write.csv(plates[[i]], fname, row.names = FALSE)
    out_files[i] <- fname
  }
  invisible(out_files)
}
