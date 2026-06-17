# ======================================================================
# tests/testthat/test-import.R
# Tests for read_raw_file()
# ======================================================================

# ---- Shared fixtures ----

df_ref <- data.frame(
  animal = c("A01_plate_1", "B01_plate_1", "C01_plate_1"),
  value  = c(1.5, 2.5, 3.5),
  stringsAsFactors = FALSE
)

csv_path  <- tempfile(fileext = ".csv")
sc_path   <- tempfile(fileext = ".csv")   # semicolon separator
xlsx_path <- tempfile(fileext = ".xlsx")

utils::write.csv(df_ref, csv_path, row.names = FALSE)
utils::write.table(df_ref, sc_path, sep = ";", row.names = FALSE)
writexl::write_xlsx(df_ref, xlsx_path)

# ZIP fixture — one well file
tmp_dir   <- tempfile("zip_test_")
dir.create(tmp_dir)
well_df   <- data.frame(T = 1:5, X = seq(0.1, 0.5, 0.1), Y = seq(0.5, 0.1, -0.1))
well_path <- file.path(tmp_dir, "A01.txt")
utils::write.table(well_df, well_path, sep = "\t", row.names = FALSE)
zip_path  <- tempfile(fileext = ".zip")
zip::zip(zip_path, files = "A01.txt", root = tmp_dir)

# ======================================================================
# CSV
# ======================================================================

test_that("read_raw_file CSV: returns a data.frame", {
  result <- read_raw_file(csv_path, "test.csv")
  expect_true(is.data.frame(result))
})

test_that("read_raw_file CSV: correct dimensions", {
  result <- read_raw_file(csv_path, "test.csv")
  expect_equal(nrow(result), 3L)
  expect_true(all(c("animal", "value") %in% names(result)))
})

test_that("read_raw_file CSV: file_name attribute matches argument", {
  result <- read_raw_file(csv_path, "my_plate.csv")
  expect_equal(attr(result, "file_name"), "my_plate.csv")
})

test_that("read_raw_file CSV: data values preserved", {
  result <- read_raw_file(csv_path, "test.csv")
  expect_equal(result$animal, df_ref$animal)
  expect_equal(result$value,  df_ref$value)
})

test_that("read_raw_file CSV: semicolon separator auto-detected", {
  result <- read_raw_file(sc_path, "test_sc.csv")
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3L)
  expect_true("animal" %in% names(result))
})

# ======================================================================
# XLSX
# ======================================================================

test_that("read_raw_file XLSX: returns a data.frame", {
  result <- read_raw_file(xlsx_path, "test.xlsx")
  expect_true(is.data.frame(result))
})

test_that("read_raw_file XLSX: correct dimensions", {
  result <- read_raw_file(xlsx_path, "test.xlsx")
  expect_equal(nrow(result), 3L)
  expect_true("animal" %in% names(result))
})

test_that("read_raw_file XLSX: file_name attribute matches argument", {
  result <- read_raw_file(xlsx_path, "my_plate.xlsx")
  expect_equal(attr(result, "file_name"), "my_plate.xlsx")
})

test_that("read_raw_file XLSX: data values preserved", {
  result <- read_raw_file(xlsx_path, "test.xlsx")
  expect_equal(result$animal, df_ref$animal)
})

# ======================================================================
# ZIP
# ======================================================================

test_that("read_raw_file ZIP: returns a data.frame", {
  result <- read_raw_file(zip_path, "plate1.zip")
  expect_true(is.data.frame(result))
})

test_that("read_raw_file ZIP: file_name attribute matches argument", {
  result <- read_raw_file(zip_path, "plate1.zip")
  expect_equal(attr(result, "file_name"), "plate1.zip")
})

test_that("read_raw_file ZIP: file_txt_name column present with bare filename", {
  result <- read_raw_file(zip_path, "plate1.zip")
  expect_true("file_txt_name" %in% names(result))
  expect_equal(unique(result$file_txt_name), "A01")
})

test_that("read_raw_file ZIP: coordinate columns preserved", {
  result <- read_raw_file(zip_path, "plate1.zip")
  expect_equal(nrow(result), 5L)
  expect_true(all(c("T", "X", "Y") %in% names(result)))
  expect_equal(result$X, well_df$X)
})

test_that("read_raw_file ZIP: error when archive contains no .txt files", {
  empty_dir <- tempfile("empty_zip_")
  dir.create(empty_dir)
  csv_dummy <- file.path(empty_dir, "dummy.csv")
  utils::write.csv(data.frame(x = 1), csv_dummy, row.names = FALSE)
  bad_zip <- tempfile(fileext = ".zip")
  zip::zip(bad_zip, files = "dummy.csv", root = empty_dir)
  expect_error(read_raw_file(bad_zip, "empty.zip"), "no .txt files")
})

# ======================================================================
# Unsupported format
# ======================================================================

test_that("read_raw_file: error for unsupported extension", {
  bad_path <- tempfile(fileext = ".txt")
  writeLines("hello", bad_path)
  expect_error(read_raw_file(bad_path, "data.txt"), "Unsupported")
})

# ======================================================================
# read_raw_file — optional file_name (parity tests)
# ======================================================================

test_that("read_raw_file: file_name defaults to basename(path)", {
  result <- read_raw_file(csv_path)
  expect_equal(attr(result, "file_name"), basename(csv_path))
})

test_that("read_raw_file: explicit and default file_name produce identical data", {
  r_explicit <- read_raw_file(xlsx_path, basename(xlsx_path))
  r_default  <- read_raw_file(xlsx_path)
  expect_equal(r_explicit$animal, r_default$animal)
  expect_equal(attr(r_explicit, "file_name"), attr(r_default, "file_name"))
})

# ======================================================================
# read_raw_files (plural) — batch loading
# ======================================================================

test_that("read_raw_files: returns a list", {
  result <- read_raw_files(c(csv_path, xlsx_path))
  expect_true(is.list(result))
})

test_that("read_raw_files: length equals number of paths", {
  result <- read_raw_files(c(csv_path, xlsx_path))
  expect_equal(length(result), 2L)
})

test_that("read_raw_files: each element has a file_name attribute", {
  result <- read_raw_files(c(csv_path, xlsx_path))
  attrs <- vapply(result, function(x) attr(x, "file_name"), character(1))
  expect_true(all(nzchar(attrs)))
})

test_that("read_raw_files: alphabetical sort applied to names", {
  result <- read_raw_files(
    paths = c(xlsx_path, csv_path),
    names = c("z_plate.xlsx", "a_plate.csv")
  )
  attrs <- vapply(result, function(x) attr(x, "file_name"), character(1))
  expect_equal(attrs[1], "a_plate.csv")
  expect_equal(attrs[2], "z_plate.xlsx")
})

test_that("read_raw_files: empty paths returns empty list", {
  result <- read_raw_files(character(0))
  expect_equal(length(result), 0L)
})

test_that("read_raw_files: error when paths and names have different lengths", {
  expect_error(read_raw_files(c(csv_path, xlsx_path), names = "only_one"), "same length")
})

# ======================================================================
# create_period_table
# ======================================================================

test_that("create_period_table: returns a data.frame", {
  result <- create_period_table(c(0, 600), c("light1-dark1", "dark1-light2"))
  expect_true(is.data.frame(result))
})

test_that("create_period_table: has columns start and transition", {
  result <- create_period_table(c(0, 600), c("light1-dark1", "dark1-light2"))
  expect_true(all(c("start", "transition") %in% names(result)))
})

test_that("create_period_table: start column is numeric", {
  result <- create_period_table(c(0, 600), c("light1-dark1", "dark1-light2"))
  expect_true(is.numeric(result$start))
})

test_that("create_period_table: transition column is character", {
  result <- create_period_table(c(0, 600), c("light1-dark1", "dark1-light2"))
  expect_true(is.character(result$transition))
})

test_that("create_period_table: output has same number of rows as inputs", {
  result <- create_period_table(c(0, 300, 600), c("a-b", "b-c", "c-a"))
  expect_equal(nrow(result), 3L)
})

test_that("create_period_table: values are preserved", {
  result <- create_period_table(c(0, 600), c("light1-dark1", "dark1-light2"))
  expect_equal(result$start, c(0, 600))
  expect_equal(result$transition, c("light1-dark1", "dark1-light2"))
})

test_that("create_period_table: error when start contains non-numeric", {
  expect_error(create_period_table(c("abc", 600), c("a-b", "b-c")), "numeric")
})

test_that("create_period_table: error when lengths differ", {
  expect_error(
    create_period_table(c(0, 600), "only_one"),
    "same length"
  )
})

test_that("create_period_table: error when inputs are empty", {
  expect_error(create_period_table(numeric(0), character(0)), "at least one")
})

# ======================================================================
# create_removal_table
# ======================================================================

test_that("create_removal_table: returns a data.frame", {
  result <- create_removal_table(plate_id = 1)
  expect_true(is.data.frame(result))
})

test_that("create_removal_table: has all required columns", {
  result <- create_removal_table(plate_id = 1)
  expected_cols <- c("plate_id", "remove_time_codes", "remove_wells",
                     "remove_conditions", "remove_periods")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("create_removal_table: plate_id is numeric", {
  result <- create_removal_table(plate_id = c(1, 2))
  expect_true(is.numeric(result$plate_id))
})

test_that("create_removal_table: NA defaults preserved as NA", {
  result <- create_removal_table(plate_id = 1)
  expect_true(is.na(result$remove_wells[1]))
  expect_true(is.na(result$remove_conditions[1]))
})

test_that("create_removal_table: scalar spec recycled to n plates", {
  result <- create_removal_table(plate_id = c(1, 2), remove_wells = "A01,B02")
  expect_equal(result$remove_wells, c("A01,B02", "A01,B02"))
})

test_that("create_removal_table: per-plate specs accepted", {
  result <- create_removal_table(
    plate_id     = c(1, 2),
    remove_wells = c("A01", "C03")
  )
  expect_equal(result$remove_wells[1], "A01")
  expect_equal(result$remove_wells[2], "C03")
})

test_that("create_removal_table: plate_id parsed from string formats", {
  result <- create_removal_table(plate_id = "Plate1")
  expect_equal(result$plate_id, 1)
  result2 <- create_removal_table(plate_id = "plate_2")
  expect_equal(result2$plate_id, 2)
})

test_that("create_removal_table: error for empty plate_id", {
  expect_error(create_removal_table(plate_id = integer(0)), "at least one")
})

test_that("create_removal_table: error for spec length mismatch", {
  expect_error(
    create_removal_table(plate_id = c(1, 2), remove_wells = c("A01", "B02", "C03")),
    "length"
  )
})

# =============================================================================
# export_results
# =============================================================================

make_export_result <- function() {
  df <- data.frame(
    plate_id          = "1",
    animal            = c("A01", "A02"),
    condition_grouped = c("ctrl", "trt"),
    inact             = c(50L, 80L),
    stringsAsFactors  = FALSE
  )
  list(processed_data_list = list(df))
}

test_that("export_results: xlsx file is created", {
  td <- withr::local_tempdir()
  export_results(make_export_result(), path = td)
  expect_true(file.exists(file.path(td, "processed_results.xlsx")))
})

test_that("export_results: xlsx has one sheet per plate", {
  td <- withr::local_tempdir()
  export_results(make_export_result(), path = td)
  sheets <- readxl::excel_sheets(file.path(td, "processed_results.xlsx"))
  expect_equal(length(sheets), length(make_export_result()$processed_data_list))
})

test_that("export_results: xlsx sheet name derived from plate_id", {
  td <- withr::local_tempdir()
  export_results(make_export_result(), path = td)
  sheets <- readxl::excel_sheets(file.path(td, "processed_results.xlsx"))
  expect_true(any(grepl("plate_1", sheets)))
})

test_that("export_results: csv format creates one file per plate", {
  td <- withr::local_tempdir()
  export_results(make_export_result(), path = td, format = "csv")
  csv_files <- list.files(td, pattern = "\\.csv$")
  expect_equal(length(csv_files), length(make_export_result()$processed_data_list))
})

test_that("export_results: error if processed_data_list slot missing", {
  expect_error(export_results(list(x = 1)), "processed_data_list")
})
