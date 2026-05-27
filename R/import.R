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
#' @param file_name Character. Original display name of the file (e.g.
#'   \code{"plate_1.xlsx"}). Stored as \code{attr(result, "file_name")}.
#'
#' @return A data frame. The attribute \code{"file_name"} is set to
#'   \code{file_name}.
#' @export
#' @examples
#' \dontrun{
#' df <- read_raw_file("/tmp/plate_1.xlsx", "plate_1.xlsx")
#' attr(df, "file_name")  # "plate_1.xlsx"
#'
#' df <- read_raw_file("/tmp/plate_1.csv", "plate_1.csv")
#' }
read_raw_file <- function(path, file_name) {
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
