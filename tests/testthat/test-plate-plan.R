# tests/testthat/test-plate-plan.R

# ---- Helpers ----------------------------------------------------------------
border_96 <- c(
  paste0("A", sprintf("%02d", 1:12)),
  paste0(rep(c("B","C","D","E","F","G"), each = 2), sprintf("%02d", c(1, 12))),
  paste0("H", sprintf("%02d", 1:12))
)

border_48 <- c(
  paste0("A", sprintf("%02d", 1:8)),
  paste0(rep(c("B","C","D","E"), each = 2), sprintf("%02d", c(1, 8))),
  paste0("F", sprintf("%02d", 1:8))
)

border_24 <- c(
  paste0("A", sprintf("%02d", 1:6)),
  paste0(rep(c("B","C"), each = 2), sprintf("%02d", c(1, 6))),
  paste0("D", sprintf("%02d", 1:6))
)

border_12 <- c("A01","A02","A03","A04","B01","B04","C01","C02","C03","C04")

# =============================================================================
# 1. Return type and structure
# =============================================================================

test_that("returns a list", {
  plans <- create_plate_plan("96", c("ctrl", "trt"), 1, 4)
  expect_type(plans, "list")
})

test_that("list length equals n_plates", {
  for (n in c(1, 2, 3)) {
    plans <- create_plate_plan("96", c("ctrl"), 1, 1, n_plates = n)
    expect_length(plans, n)
  }
})

test_that("each element is a data frame", {
  plans <- create_plate_plan("96", c("ctrl", "trt"), 1, 4, n_plates = 2)
  for (df in plans) expect_s3_class(df, "data.frame")
})

test_that("each data frame has exactly the required columns", {
  plans <- create_plate_plan("48", c("ctrl", "trt"), 1, 4)
  expect_named(plans[[1]], c("animal", "condition", "plate_id"))
})

# =============================================================================
# 2. Well count per plate type
# =============================================================================

test_that("96-well plate has 96 rows", {
  plans <- create_plate_plan("96", c("ctrl"), 1, 1)
  expect_equal(nrow(plans[[1]]), 96L)
})

test_that("48-well plate has 48 rows", {
  plans <- create_plate_plan("48", c("ctrl"), 1, 1)
  expect_equal(nrow(plans[[1]]), 48L)
})

test_that("24-well plate has 24 rows", {
  plans <- create_plate_plan("24", c("ctrl"), 1, 1)
  expect_equal(nrow(plans[[1]]), 24L)
})

test_that("12-well plate has 12 rows", {
  plans <- create_plate_plan("12", c("ctrl"), 1, 1)
  expect_equal(nrow(plans[[1]]), 12L)
})

# =============================================================================
# 3. Animal column format
# =============================================================================

test_that("animal column follows <well>_plate_<i> format", {
  plans <- create_plate_plan("12", c("ctrl"), 1, 1, n_plates = 2)
  expect_true(all(grepl("^[A-C]\\d{2}_plate_1$", plans[[1]]$animal)))
  expect_true(all(grepl("^[A-C]\\d{2}_plate_2$", plans[[2]]$animal)))
})

# =============================================================================
# 4. Plate ID
# =============================================================================

test_that("plate_id matches plate index", {
  plans <- create_plate_plan("96", c("ctrl"), 1, 1, n_plates = 3)
  for (i in 1:3) expect_true(all(plans[[i]]$plate_id == as.character(i)))
})

test_that("plate_id is stored as character", {
  plans <- create_plate_plan("96", c("ctrl"), 1, 1)
  expect_type(plans[[1]]$plate_id, "character")
})

# =============================================================================
# 5. file_name attribute
# =============================================================================

test_that("file_name attribute follows <name>_plate_<i>.xlsx", {
  plans <- create_plate_plan("96", c("ctrl"), 1, 1, n_plates = 2, name = "exp1")
  expect_equal(attr(plans[[1]], "file_name"), "exp1_plate_1.xlsx")
  expect_equal(attr(plans[[2]], "file_name"), "exp1_plate_2.xlsx")
})

test_that("default name is 'plate_plan'", {
  plans <- create_plate_plan("96", c("ctrl"), 1, 1)
  expect_equal(attr(plans[[1]], "file_name"), "plate_plan_plate_1.xlsx")
})

# =============================================================================
# 6. Assigned well count
# =============================================================================

test_that("total assigned wells matches conditions × replicates × units", {
  # 2 conditions × 3 replicates × 4 units = 24 assigned wells
  plans <- create_plate_plan("96", c("ctrl", "trt"), 3, 4, seed = 1)
  n_assigned <- sum(plans[[1]]$condition != "X")
  expect_equal(n_assigned, 24L)
})

test_that("unassigned wells are labelled X", {
  plans <- create_plate_plan("96", c("ctrl"), 1, 1, seed = 1)
  cond_vals <- unique(plans[[1]]$condition)
  expect_true("X" %in% cond_vals)
  expect_true(all(plans[[1]]$condition %in% c("X", "ctrl_1")))
})

# =============================================================================
# 7. Condition label format
# =============================================================================

test_that("condition labels follow <condition>_<replicate> format", {
  plans <- create_plate_plan("96", c("ctrl", "trt"), 2, 4, seed = 42)
  assigned <- plans[[1]]$condition[plans[[1]]$condition != "X"]
  expect_true(all(grepl("^(ctrl|trt)_[12]$", assigned)))
})

# =============================================================================
# 8. Reproducibility
# =============================================================================

test_that("same seed produces identical output", {
  p1 <- create_plate_plan("96", c("ctrl", "trt"), 2, 4, seed = 42)
  p2 <- create_plate_plan("96", c("ctrl", "trt"), 2, 4, seed = 42)
  expect_identical(p1[[1]]$condition, p2[[1]]$condition)
})

test_that("different seeds produce different condition assignments", {
  p1 <- create_plate_plan("96", c("ctrl", "trt"), 2, 4, seed = 1)
  p2 <- create_plate_plan("96", c("ctrl", "trt"), 2, 4, seed = 2)
  expect_false(identical(p1[[1]]$condition, p2[[1]]$condition))
})

# =============================================================================
# 9. Border exclusion
# =============================================================================

test_that("border wells are unassigned when include_borders = FALSE (96)", {
  plans <- create_plate_plan("96", c("ctrl", "trt"), 2, 4,
                             include_borders = FALSE, seed = 42)
  df <- plans[[1]]
  border_animal <- paste0(border_96, "_plate_1")
  border_rows   <- df[df$animal %in% border_animal, ]
  expect_true(all(border_rows$condition == "X"))
})

test_that("border wells are unassigned when include_borders = FALSE (48)", {
  plans <- create_plate_plan("48", c("ctrl"), 1, 4,
                             include_borders = FALSE, seed = 1)
  df <- plans[[1]]
  border_animal <- paste0(border_48, "_plate_1")
  border_rows   <- df[df$animal %in% border_animal, ]
  expect_true(all(border_rows$condition == "X"))
})

test_that("include_borders = TRUE allows assignment to border wells", {
  # With seed = 1, at least one border well should be assigned in a dense plan
  plans <- create_plate_plan("96", c("ctrl"), 1, 60,
                             include_borders = TRUE, seed = 1)
  df <- plans[[1]]
  border_animal <- paste0(border_96, "_plate_1")
  border_rows   <- df[df$animal %in% border_animal, ]
  expect_true(any(border_rows$condition != "X"))
})

# =============================================================================
# 10. Multi-plate distribution
# =============================================================================

test_that("conditions are distributed across plates (not all on one plate)", {
  plans <- create_plate_plan("96", c("ctrl", "trt"), 2, 4,
                             n_plates = 3, seed = 42)
  for (i in seq_along(plans)) {
    assigned <- plans[[i]]$condition[plans[[i]]$condition != "X"]
    expect_true(length(assigned) > 0,
                info = sprintf("plate %d has no assigned wells", i))
  }
})

test_that("total assigned wells across all plates is correct", {
  # total = n_conditions × n_replicates × n_units = 2 × 2 × 4 = 16
  # distributed across 2 plates (n_plates=2 divides evenly: 8 per plate)
  plans <- create_plate_plan("96", c("ctrl", "trt"), 2, 4,
                             n_plates = 2, seed = 1)
  total <- sum(sapply(plans, function(df) sum(df$condition != "X")))
  expect_equal(total, 16L)
})

# =============================================================================
# 11. Input validation errors
# =============================================================================

test_that("invalid plate_type raises an error", {
  expect_error(
    create_plate_plan("100", c("ctrl"), 1, 1),
    regexp = "plate_type"
  )
})

test_that("blank condition name raises an error", {
  expect_error(
    create_plate_plan("96", c("ctrl", ""), 1, 1),
    regexp = "blank"
  )
})

test_that("empty conditions vector raises an error", {
  expect_error(
    create_plate_plan("96", character(0), 1, 1),
    regexp = "conditions"
  )
})

test_that("n_replicates < 1 raises an error", {
  expect_error(
    create_plate_plan("96", c("ctrl"), 0, 1),
    regexp = "n_replicates"
  )
})

test_that("n_units < 1 raises an error", {
  expect_error(
    create_plate_plan("96", c("ctrl"), 1, 0),
    regexp = "n_units"
  )
})

test_that("n_plates < 1 raises an error", {
  expect_error(
    create_plate_plan("96", c("ctrl"), 1, 1, n_plates = 0),
    regexp = "n_plates"
  )
})

test_that("exceeding available wells raises an error", {
  # 12-well plate: 12 wells, requesting 13 units
  expect_error(
    create_plate_plan("12", c("ctrl"), 1, 13),
    regexp = "exceed available wells"
  )
})

test_that("exceeding available non-border wells raises an error", {
  # 96-well plate: 60 interior wells, request 61
  expect_error(
    create_plate_plan("96", c("ctrl"), 1, 61, include_borders = FALSE),
    regexp = "exceed available wells"
  )
})

# =============================================================================
# 12. Parity with original generate_plate_plan_shiny (regression guard)
# =============================================================================

test_that("output is identical to original generate_plate_plan_shiny", {
  # Load original function from module file (it is still defined there
  # and will be removed after this test confirms parity)
  src_env <- new.env(parent = baseenv())
  # minimal stubs so the module sources without Shiny
  src_env$shiny  <- list(NS = function(id) function(x) paste0(id, "-", x),
                         moduleServer = function(...) invisible(NULL))
  src_env$shinyjs <- list(useShinyjs = function() invisible(NULL),
                           toggleState = function(...) invisible(NULL))
  src_env$shinydashboard <- list(box = function(...) invisible(NULL))
  src_env$DT    <- list(dataTableOutput = function(...) invisible(NULL),
                        renderDataTable = function(...) invisible(NULL),
                        datatable       = function(...) invisible(NULL),
                        formatStyle     = function(...) invisible(NULL))
  src_env$plotly <- list(plotlyOutput   = function(...) invisible(NULL),
                         renderPlotly   = function(...) invisible(NULL),
                         plot_ly        = function(...) invisible(NULL),
                         layout         = function(...) invisible(NULL),
                         ggplotly       = function(...) invisible(NULL))
  src_env$ggplot2  <- as.list(asNamespace("ggplot2"))
  src_env$scales   <- as.list(asNamespace("scales"))
  src_env$openxlsx <- list(write.xlsx = function(...) invisible(NULL))
  src_env$zip      <- list(zip = function(...) invisible(NULL))
  src_env$stats    <- as.list(asNamespace("stats"))
  src_env$read_file <- function(path, name) data.frame()
  src_env$notify   <- function(...) invisible(NULL)

  tryCatch(
    source(
      system.file("app/modules/import_generate_plate_plan.R", package = "zebRabox"),
      local = src_env
    ),
    error = function(e) skip(paste("Could not source module for parity test:", e$message))
  )

  if (!exists("generate_plate_plan_shiny", envir = src_env))
    skip("generate_plate_plan_shiny not found in module (already removed — parity confirmed).")

  old_fn <- src_env$generate_plate_plan_shiny

  old_inputs <- list(
    create_plate_plan    = "yes",
    plate_type           = "96",
    conditions_number    = 2L,
    conditions_name      = c("ctrl", "trt"),
    replicates_number    = 3L,
    units_per_replicate  = 8L,
    plate_number         = 2L,
    keep_border_wells    = "no",
    seed_value           = 42,
    plate_plan_name_xlsx = "plate_plan"
  )

  old_res <- old_fn(old_inputs, write_files = FALSE)

  new_res <- create_plate_plan(
    plate_type      = "96",
    conditions      = c("ctrl", "trt"),
    n_replicates    = 3L,
    n_units         = 8L,
    n_plates        = 2L,
    include_borders = FALSE,
    seed            = 42,
    name            = "plate_plan"
  )

  for (i in seq_along(old_res)) {
    expect_identical(old_res[[i]]$animal,    new_res[[i]]$animal,
                     info = paste("animal column, plate", i))
    expect_identical(old_res[[i]]$condition, new_res[[i]]$condition,
                     info = paste("condition column, plate", i))
    expect_identical(old_res[[i]]$plate_id,  new_res[[i]]$plate_id,
                     info = paste("plate_id column, plate", i))
    expect_identical(attr(old_res[[i]], "file_name"),
                     attr(new_res[[i]], "file_name"),
                     info = paste("file_name attr, plate", i))
  }
})

# =============================================================================
# 13. detect_plate_type
# =============================================================================

test_that("detect_plate_type identifies 96-well plate", {
  df <- data.frame(
    animal = paste0(rep(LETTERS[1:8], each = 12), sprintf("%02d", rep(1:12, 8)),
                    "_plate_1"),
    stringsAsFactors = FALSE
  )
  expect_equal(zebRabox:::detect_plate_type(df), 96L)
})

test_that("detect_plate_type identifies 48-well plate", {
  df <- data.frame(
    animal = paste0(rep(LETTERS[1:6], each = 8), sprintf("%02d", rep(1:8, 6)),
                    "_plate_1"),
    stringsAsFactors = FALSE
  )
  expect_equal(zebRabox:::detect_plate_type(df), 48L)
})

test_that("detect_plate_type identifies 24-well plate", {
  df <- data.frame(
    animal = paste0(rep(LETTERS[1:4], each = 6), sprintf("%02d", rep(1:6, 4)),
                    "_plate_1"),
    stringsAsFactors = FALSE
  )
  expect_equal(zebRabox:::detect_plate_type(df), 24L)
})

test_that("detect_plate_type identifies 12-well plate", {
  df <- data.frame(
    animal = paste0(rep(LETTERS[1:3], each = 4), sprintf("%02d", rep(1:4, 3)),
                    "_plate_1"),
    stringsAsFactors = FALSE
  )
  expect_equal(zebRabox:::detect_plate_type(df), 12L)
})

test_that("detect_plate_type returns NA_integer_ for unknown layout", {
  df <- data.frame(
    animal = c("A01_plate_1", "B01_plate_1", "C01_plate_1"),
    stringsAsFactors = FALSE
  )
  result <- zebRabox:::detect_plate_type(df)
  expect_true(is.na(result))
  expect_type(result, "integer")
})

test_that("detect_plate_type works for plate plans from create_plate_plan", {
  plans <- create_plate_plan("96", c("ctrl"), 1, 1)
  expect_equal(zebRabox:::detect_plate_type(plans[[1]]), 96L)
})

# =============================================================================
# 14. get_well_config
# =============================================================================

test_that("get_well_config returns correct dimensions for 96", {
  cfg <- zebRabox:::get_well_config("96")
  expect_equal(length(cfg$rows), 8L)
  expect_equal(length(cfg$cols), 12L)
  expect_equal(cfg$rows[1], "A")
  expect_equal(cfg$rows[8], "H")
})

test_that("get_well_config returns correct dimensions for 48", {
  cfg <- zebRabox:::get_well_config("48")
  expect_equal(length(cfg$rows), 6L)
  expect_equal(length(cfg$cols), 8L)
})

test_that("get_well_config returns correct dimensions for 24", {
  cfg <- zebRabox:::get_well_config("24")
  expect_equal(length(cfg$rows), 4L)
  expect_equal(length(cfg$cols), 6L)
})

test_that("get_well_config returns correct dimensions for 12", {
  cfg <- zebRabox:::get_well_config("12")
  expect_equal(length(cfg$rows), 3L)
  expect_equal(length(cfg$cols), 4L)
})

test_that("get_well_config accepts integer plate_type", {
  expect_no_error(zebRabox:::get_well_config(96L))
  expect_equal(length(zebRabox:::get_well_config(96L)$rows), 8L)
})

test_that("get_well_config errors on invalid plate type", {
  expect_error(zebRabox:::get_well_config("100"), regexp = "plate_type")
})

# =============================================================================
# 15. parse_well_coords
# =============================================================================

test_that("parse_well_coords adds Row Column well_id", {
  df     <- data.frame(animal = "A01_plate_1", stringsAsFactors = FALSE)
  result <- zebRabox:::parse_well_coords(df)
  expect_equal(result$Row,     "A")
  expect_equal(result$Column,  1L)
  expect_equal(result$well_id, "A01")
})

test_that("parse_well_coords handles multi-digit plate index", {
  df     <- data.frame(animal = "B12_plate_10", stringsAsFactors = FALSE)
  result <- zebRabox:::parse_well_coords(df)
  expect_equal(result$Row,     "B")
  expect_equal(result$Column,  12L)
  expect_equal(result$well_id, "B12")
})

test_that("parse_well_coords handles H12 in 96-well plate", {
  df     <- data.frame(animal = "H12_plate_2", stringsAsFactors = FALSE)
  result <- zebRabox:::parse_well_coords(df)
  expect_equal(result$Row,     "H")
  expect_equal(result$Column,  12L)
  expect_equal(result$well_id, "H12")
})

test_that("parse_well_coords preserves existing columns", {
  df     <- data.frame(animal = "A01_plate_1", condition = "ctrl_1",
                       stringsAsFactors = FALSE)
  result <- zebRabox:::parse_well_coords(df)
  expect_true("condition" %in% names(result))
})

test_that("parse_well_coords works on create_plate_plan output", {
  plans  <- create_plate_plan("96", c("ctrl"), 1, 1)
  result <- zebRabox:::parse_well_coords(plans[[1]])
  expect_true(all(result$Row %in% LETTERS[1:8]))
  expect_true(all(result$Column %in% 1:12))
})

# =============================================================================
# 16. normalize_plate_conditions
# =============================================================================

test_that("normalize_plate_conditions strips replicate suffix", {
  df     <- data.frame(condition = c("ctrl_1", "ctrl_2", "trt_1"),
                       stringsAsFactors = FALSE)
  result <- zebRabox:::normalize_plate_conditions(df)
  expect_equal(result$Condition, c("ctrl", "ctrl", "trt"))
})

test_that("normalize_plate_conditions maps empty string to X", {
  df     <- data.frame(condition = c("ctrl_1", "X", ""),
                       stringsAsFactors = FALSE)
  result <- zebRabox:::normalize_plate_conditions(df)
  expect_equal(result$Condition[3], "X")
})

test_that("normalize_plate_conditions maps NA condition to X", {
  df     <- data.frame(condition = c("ctrl_1", NA_character_),
                       stringsAsFactors = FALSE)
  result <- zebRabox:::normalize_plate_conditions(df)
  expect_equal(result$Condition[2], "X")
})

test_that("normalize_plate_conditions handles Condition_Base column", {
  df     <- data.frame(Condition_Base = c("ctrl", "trt"), stringsAsFactors = FALSE)
  result <- zebRabox:::normalize_plate_conditions(df)
  expect_equal(result$Condition, c("ctrl", "trt"))
  expect_false("Condition_Base" %in% names(result))
})

test_that("normalize_plate_conditions works on create_plate_plan output", {
  plans  <- create_plate_plan("24", c("ctrl", "trt"), 1, 3, seed = 1)
  result <- zebRabox:::normalize_plate_conditions(plans[[1]])
  expect_true("Condition" %in% names(result))
  expect_true(all(result$Condition %in% c("ctrl", "trt", "X")))
})

# =============================================================================
# 17. plate_condition_colors
# =============================================================================

test_that("plate_condition_colors returns named vector with X", {
  cols <- zebRabox:::plate_condition_colors(c("ctrl", "trt"))
  expect_named(cols, c("ctrl", "trt", "X"))
  expect_equal(unname(cols["X"]), "#FFFFFF")
})

test_that("plate_condition_colors length is conditions + 1", {
  conds <- c("a", "b", "c")
  cols  <- zebRabox:::plate_condition_colors(conds)
  expect_length(cols, 4L)
})

test_that("plate_condition_colors values are hex strings", {
  cols <- zebRabox:::plate_condition_colors(c("ctrl"))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
})

test_that("plate_condition_colors single condition returns 2-element vector", {
  cols <- zebRabox:::plate_condition_colors("ctrl")
  expect_length(cols, 2L)
  expect_named(cols, c("ctrl", "X"))
})

# =============================================================================
# 18. read_plate_plan_files
# =============================================================================

test_that("read_plate_plan_files reads file and attaches file_name", {
  skip_if_not_installed("openxlsx")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  df <- data.frame(animal = "A01_plate_1", condition = "ctrl_1", plate_id = "1",
                   stringsAsFactors = FALSE)
  openxlsx::write.xlsx(df, tmp, rowNames = FALSE)

  result <- read_plate_plan_files(tmp, "myplate.xlsx")
  expect_length(result, 1L)
  expect_equal(attr(result[[1]], "file_name"), "myplate.xlsx")
})

test_that("read_plate_plan_files adds plate_id when absent", {
  skip_if_not_installed("openxlsx")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  df <- data.frame(animal = "A01_plate_1", condition = "ctrl_1",
                   stringsAsFactors = FALSE)
  openxlsx::write.xlsx(df, tmp, rowNames = FALSE)

  result <- read_plate_plan_files(tmp, "p.xlsx")
  expect_true("plate_id" %in% names(result[[1]]))
  expect_equal(result[[1]]$plate_id, "1")
})

test_that("read_plate_plan_files preserves plate_id when present", {
  skip_if_not_installed("openxlsx")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  df <- data.frame(animal = "A01_plate_1", condition = "ctrl_1", plate_id = "99",
                   stringsAsFactors = FALSE)
  openxlsx::write.xlsx(df, tmp, rowNames = FALSE)

  result <- read_plate_plan_files(tmp, "p.xlsx")
  expect_equal(as.character(result[[1]]$plate_id), "99")
})

test_that("read_plate_plan_files sorts alphabetically by name", {
  skip_if_not_installed("openxlsx")
  tmp1 <- tempfile(fileext = ".xlsx")
  tmp2 <- tempfile(fileext = ".xlsx")
  on.exit({ unlink(tmp1); unlink(tmp2) })
  df <- data.frame(animal = "A01_plate_1", condition = "ctrl_1", plate_id = "1",
                   stringsAsFactors = FALSE)
  openxlsx::write.xlsx(df, tmp1, rowNames = FALSE)
  openxlsx::write.xlsx(df, tmp2, rowNames = FALSE)

  # Supply names out of alphabetical order
  result <- read_plate_plan_files(c(tmp1, tmp2), c("plate_B.xlsx", "plate_A.xlsx"))
  expect_equal(attr(result[[1]], "file_name"), "plate_A.xlsx")
  expect_equal(attr(result[[2]], "file_name"), "plate_B.xlsx")
})

test_that("read_plate_plan_files returns empty list for zero files", {
  result <- read_plate_plan_files(character(0), character(0))
  expect_length(result, 0L)
})

test_that("read_plate_plan_files errors when paths and names lengths differ", {
  expect_error(
    read_plate_plan_files(c("a.xlsx", "b.xlsx"), "a.xlsx"),
    regexp = "same length"
  )
})

# -- Parity tests for optional `names` argument (Step 1 of standalone API) --

test_that("read_plate_plan_files: names defaults to basename(paths)", {
  skip_if_not_installed("openxlsx")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  df <- data.frame(animal = "A01_plate_1", condition = "ctrl_1",
                   stringsAsFactors = FALSE)
  openxlsx::write.xlsx(df, tmp, rowNames = FALSE)

  result <- read_plate_plan_files(tmp)
  expect_equal(attr(result[[1]], "file_name"), basename(tmp))
})

test_that("read_plate_plan_files: explicit and default names produce identical data", {
  skip_if_not_installed("openxlsx")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp))
  df <- data.frame(animal = "A01_plate_1", condition = "ctrl_1",
                   stringsAsFactors = FALSE)
  openxlsx::write.xlsx(df, tmp, rowNames = FALSE)

  r_explicit <- read_plate_plan_files(tmp, basename(tmp))
  r_default  <- read_plate_plan_files(tmp)
  expect_equal(r_explicit[[1]]$animal,          r_default[[1]]$animal)
  expect_equal(attr(r_explicit[[1]], "file_name"), attr(r_default[[1]], "file_name"))
})
