# tests/testthat/test-processing.R

# =============================================================================
# Shared test fixtures
# =============================================================================

# Minimal plate plan: 4 animals, 2 conditions, 1 replicate each
make_plan <- function(plate_id = "1") {
  data.frame(
    animal    = c("A01", "A02", "B01", "B02"),
    condition = c("ctrl_1", "ctrl_1", "trt_1", "X"),
    plate_id  = plate_id,
    stringsAsFactors = FALSE
  )
}

# Raw data where animal matches plan without suffix
make_raw <- function(n_rows = 8, plate_id = NULL) {
  df <- data.frame(
    animal = rep(c("A01", "A02", "B01", "B02"), length.out = n_rows),
    an     = rep(c(0, 2), length.out = n_rows),
    start  = seq(0, by = 10, length.out = n_rows),
    inact  = runif(n_rows, 0, 100),
    inadur = runif(n_rows, 0, 100),
    inadist = runif(n_rows, 0, 100),
    smlct  = runif(n_rows, 0, 100),
    smldist = runif(n_rows, 0, 100),
    smldur = runif(n_rows, 0, 100),
    larct  = runif(n_rows, 0, 100),
    lardur = runif(n_rows, 0, 100),
    lardist = runif(n_rows, 0, 100),
    emptyct = runif(n_rows, 0, 100),
    emptydur = runif(n_rows, 0, 100),
    stringsAsFactors = FALSE
  )
  if (!is.null(plate_id)) df$plate_id <- plate_id
  df
}

# Period transitions: one transition at t=40 (light1 -> dark1)
make_period_df <- function() {
  data.frame(
    start      = c(0, 40),
    transition = c("light1-light1", "light1-dark1"),
    stringsAsFactors = FALSE
  )
}

# Removal df: no removals for plate 1
make_removal_df <- function() {
  data.frame(
    plate_id           = 1,
    remove_time_codes  = NA_character_,
    remove_wells       = NA_character_,
    remove_conditions  = NA_character_,
    remove_periods     = NA_character_,
    stringsAsFactors   = FALSE
  )
}

# Minimal TM LDM config
tm_ldm_cfg <- list(
  ui_title     = "Tracking Mode, Light-Dark Mode",
  period_map   = function(x) {
    dplyr::case_when(
      grepl("^light", x) ~ "light",
      grepl("^dark",  x) ~ "dark",
      TRUE ~ x
    )
  },
  convert_cols  = c("inact", "inadur", "inadist", "smlct", "smldist", "smldur",
                    "larct", "lardur", "lardist", "emptyct", "emptydur"),
  zone_num_cols = c("inact", "inadur", "inadist", "smlct", "smldist", "smldur",
                    "larct", "lardur", "lardist", "emptyct", "emptydur"),
  filter_fn     = NULL
)

# =============================================================================
# 1. convert_numeric_cols
# =============================================================================

test_that("convert_numeric_cols converts comma-decimal strings to numeric", {
  df <- data.frame(x = c("1,5", "2,0", "3"), stringsAsFactors = FALSE)
  out <- zebRabox:::convert_numeric_cols(df, "x")
  expect_equal(out$x, c(1.5, 2.0, 3.0))
})

test_that("convert_numeric_cols ignores columns not in df", {
  df <- data.frame(a = c("1,2"), stringsAsFactors = FALSE)
  expect_silent(zebRabox:::convert_numeric_cols(df, c("a", "missing_col")))
})

test_that("convert_numeric_cols returns unchanged df when cols is empty", {
  df <- data.frame(a = c("1,2"), stringsAsFactors = FALSE)
  out <- zebRabox:::convert_numeric_cols(df, character(0))
  expect_identical(out, df)
})

# =============================================================================
# 2. generate_conditions
# =============================================================================

test_that("generate_conditions adds condition column", {
  plan <- make_plan()
  raw  <- make_raw()
  res  <- zebRabox:::generate_conditions(raw, plan, 1L)
  expect_true("condition" %in% names(res$data))
})

test_that("generate_conditions assigns correct condition labels", {
  plan <- make_plan()
  raw  <- data.frame(animal = c("A01", "B01"), an = c(0, 0),
                     start = c(0, 10), stringsAsFactors = FALSE)
  res  <- zebRabox:::generate_conditions(raw, plan, 1L)
  expect_equal(res$data$condition, c("ctrl_1", "trt_1"))
})

test_that("generate_conditions adds plate_id from plan", {
  plan <- make_plan("2")
  raw  <- make_raw()
  res  <- zebRabox:::generate_conditions(raw, plan, 1L)
  expect_true(all(res$data$plate_id == "2"))
})

test_that("generate_conditions adds condition_grouped column", {
  plan <- make_plan()
  raw  <- make_raw()
  res  <- zebRabox:::generate_conditions(raw, plan, 1L)
  expect_true("condition_grouped" %in% names(res$data))
})

test_that("condition_grouped strips replicate suffix", {
  plan <- make_plan()
  raw  <- data.frame(animal = "A01", an = 0L, start = 0,
                     stringsAsFactors = FALSE)
  res  <- zebRabox:::generate_conditions(raw, plan, 1L)
  expect_equal(res$data$condition_grouped, "ctrl")
})

test_that("generate_conditions adds condition_tagged column", {
  plan <- make_plan()
  raw  <- make_raw()
  res  <- zebRabox:::generate_conditions(raw, plan, 1L)
  expect_true("condition_tagged" %in% names(res$data))
})

test_that("generate_conditions returns list with data and plan", {
  plan <- make_plan()
  raw  <- make_raw()
  res  <- zebRabox:::generate_conditions(raw, plan, 1L)
  expect_named(res, c("data", "plan"))
})

test_that("generate_conditions errors when no animals match", {
  plan <- make_plan()
  raw  <- data.frame(animal = c("Z99", "Z98"), an = c(0, 0),
                     start = c(0, 10), stringsAsFactors = FALSE)
  expect_error(zebRabox:::generate_conditions(raw, plan, 1L), regexp = "failed")
})

test_that("log_fn receives messages", {
  plan <- make_plan()
  raw  <- make_raw()
  msgs <- character()
  zebRabox:::generate_conditions(raw, plan, 1L, log_fn = function(m) msgs <<- c(msgs, m))
  expect_true(length(msgs) == 0)  # no messages in normal run
})

# =============================================================================
# 3. assign_periods
# =============================================================================

test_that("assign_periods adds period_with_numbers column", {
  raw     <- make_raw()
  per_df  <- make_period_df()
  res     <- zebRabox:::assign_periods(raw, per_df, 1L, tm_ldm_cfg)
  expect_true("period_with_numbers" %in% names(res$data))
})

test_that("assign_periods adds period_without_numbers column", {
  raw     <- make_raw()
  per_df  <- make_period_df()
  res     <- zebRabox:::assign_periods(raw, per_df, 1L, tm_ldm_cfg)
  expect_true("period_without_numbers" %in% names(res$data))
})

test_that("rows before first boundary get 'before' period label", {
  raw <- data.frame(animal = "A01", an = 0L, start = 5,
                    stringsAsFactors = FALSE)
  per_df <- data.frame(start = 10, transition = "light1-dark1",
                       stringsAsFactors = FALSE)
  res <- zebRabox:::assign_periods(raw, per_df, 1L, list(period_map = NULL))
  expect_equal(res$data$period_with_numbers, "light1")
})

test_that("rows after last boundary get 'after' period label", {
  raw <- data.frame(animal = "A01", an = 0L, start = 50,
                    stringsAsFactors = FALSE)
  per_df <- data.frame(start = 10, transition = "light1-dark1",
                       stringsAsFactors = FALSE)
  res <- zebRabox:::assign_periods(raw, per_df, 1L, list(period_map = NULL))
  expect_equal(res$data$period_with_numbers, "dark1")
})

test_that("period_without_numbers strips numbers via period_map", {
  raw <- data.frame(animal = "A01", an = 0L, start = 50,
                    stringsAsFactors = FALSE)
  per_df <- data.frame(start = 10, transition = "light1-dark1",
                       stringsAsFactors = FALSE)
  res <- zebRabox:::assign_periods(raw, per_df, 1L, tm_ldm_cfg)
  expect_equal(res$data$period_without_numbers, "dark")
})

test_that("assign_periods returns boundaries and transitions", {
  raw    <- make_raw()
  per_df <- make_period_df()
  res    <- zebRabox:::assign_periods(raw, per_df, 1L, tm_ldm_cfg)
  expect_named(res, c("data", "boundaries", "transitions"))
  expect_equal(res$boundaries, per_df$start)
})

# =============================================================================
# 4. remove_time_codes
# =============================================================================

test_that("remove_time_codes removes matching rows", {
  raw <- data.frame(animal = c("A01", "A02"), start = c(10, 20),
                    stringsAsFactors = FALSE)
  rr  <- data.frame(remove_time_codes = "10", stringsAsFactors = FALSE)
  out <- zebRabox:::remove_time_codes(raw, rr, 1L)
  expect_equal(nrow(out), 1L)
  expect_equal(out$start, 20)
})

test_that("remove_time_codes skips when NA", {
  raw <- data.frame(animal = c("A01"), start = 10, stringsAsFactors = FALSE)
  rr  <- data.frame(remove_time_codes = NA_character_, stringsAsFactors = FALSE)
  out <- zebRabox:::remove_time_codes(raw, rr, 1L)
  expect_equal(nrow(out), 1L)
})

test_that("remove_time_codes skips when 'no'", {
  raw <- data.frame(animal = c("A01"), start = 10, stringsAsFactors = FALSE)
  rr  <- data.frame(remove_time_codes = "no", stringsAsFactors = FALSE)
  out <- zebRabox:::remove_time_codes(raw, rr, 1L)
  expect_equal(nrow(out), 1L)
})

# =============================================================================
# 5. remove_periods
# =============================================================================

test_that("remove_periods removes rows with matching period", {
  raw <- data.frame(animal = c("A01", "A02"),
                    period_with_numbers = c("light1", "dark1"),
                    stringsAsFactors = FALSE)
  rr  <- data.frame(remove_periods = "dark1", stringsAsFactors = FALSE)
  out <- zebRabox:::remove_periods(raw, rr, 1L)
  expect_equal(nrow(out), 1L)
  expect_equal(out$period_with_numbers, "light1")
})

test_that("remove_periods skips when NA", {
  raw <- data.frame(animal = "A01", period_with_numbers = "light1",
                    stringsAsFactors = FALSE)
  rr  <- data.frame(remove_periods = NA_character_, stringsAsFactors = FALSE)
  out <- zebRabox:::remove_periods(raw, rr, 1L)
  expect_equal(nrow(out), 1L)
})

# =============================================================================
# 6. remove_wells
# =============================================================================

test_that("remove_wells removes named wells", {
  raw <- data.frame(animal = c("A01", "B01"), start = c(0, 10),
                    stringsAsFactors = FALSE)
  rr  <- data.frame(remove_wells = "A01", stringsAsFactors = FALSE)
  out <- zebRabox:::remove_wells(raw, rr, 1L)
  expect_equal(nrow(out), 1L)
  expect_equal(out$animal, "B01")
})

test_that("remove_wells skips when NA", {
  raw <- data.frame(animal = "A01", stringsAsFactors = FALSE)
  rr  <- data.frame(remove_wells = NA_character_, stringsAsFactors = FALSE)
  out <- zebRabox:::remove_wells(raw, rr, 1L)
  expect_equal(nrow(out), 1L)
})

test_that("remove_wells handles comma-separated list", {
  raw <- data.frame(animal = c("A01", "A02", "B01"), start = 1:3,
                    stringsAsFactors = FALSE)
  rr  <- data.frame(remove_wells = "A01, B01", stringsAsFactors = FALSE)
  out <- zebRabox:::remove_wells(raw, rr, 1L)
  expect_equal(out$animal, "A02")
})

# =============================================================================
# 7. remove_conditions
# =============================================================================

test_that("remove_conditions removes all replicates of a condition_grouped", {
  raw <- data.frame(
    animal            = c("A01", "A02", "B01"),
    condition         = c("ctrl_1", "ctrl_2", "trt_1"),
    condition_grouped = c("ctrl",   "ctrl",   "trt"),
    stringsAsFactors  = FALSE
  )
  rr  <- data.frame(remove_conditions = "ctrl", stringsAsFactors = FALSE)
  out <- zebRabox:::remove_conditions(raw, rr, 1L)
  expect_equal(nrow(out), 1L)
  expect_equal(out$condition, "trt_1")
})

test_that("remove_conditions skips when NA", {
  raw <- data.frame(
    animal            = "A01",
    condition         = "ctrl_1",
    condition_grouped = "ctrl",
    stringsAsFactors  = FALSE
  )
  rr  <- data.frame(remove_conditions = NA_character_, stringsAsFactors = FALSE)
  out <- zebRabox:::remove_conditions(raw, rr, 1L)
  expect_equal(nrow(out), 1L)
})

# =============================================================================
# 8. process_zones
# =============================================================================

make_zoned_raw <- function() {
  # 2 animals × 2 zones (0 and 2), each animal at 2 distinct start times
  # zone 0: rows 1-4, zone 2: rows 5-8  (animal+start unique within each zone)
  data.frame(
    animal   = c("A01", "A02", "A01", "A02",
                 "A01", "A02", "A01", "A02"),
    an       = c(0, 0, 0, 0, 2, 2, 2, 2),
    start    = c(0, 10, 20, 30, 0, 10, 20, 30),
    plate_id = "1",
    condition         = "ctrl_1",
    condition_grouped = "ctrl",
    condition_tagged  = "ctrl_1",
    period_with_numbers    = "light1",
    period_without_numbers = "light",
    inact    = c(50, 80, 60, 90, 20, 30, 25, 40),
    inadur   = c(40, 70, 50, 80, 15, 25, 20, 35),
    inadist  = c(30, 60, 40, 70, 10, 20, 15, 30),
    smlct    = c(20, 50, 30, 60,  5, 15, 10, 25),
    smldist  = c(15, 45, 25, 55,  4, 14,  9, 24),
    smldur   = c(10, 40, 20, 50,  3, 13,  8, 23),
    larct    = c( 5, 35, 15, 45,  2, 12,  7, 22),
    lardur   = c( 4, 30, 14, 40,  1, 11,  6, 21),
    lardist  = c( 3, 25, 13, 35,  1, 10,  5, 20),
    emptyct  = c( 2, 20, 12, 30,  1,  9,  4, 19),
    emptydur = c( 1, 15, 11, 25,  1,  8,  3, 18),
    stringsAsFactors = FALSE
  )
}

test_that("process_zones returns a data frame", {
  raw <- make_zoned_raw()
  out <- zebRabox:::process_zones(raw, 1L, tm_ldm_cfg)
  expect_s3_class(out, "data.frame")
})

test_that("process_zones creates a zone column", {
  raw <- make_zoned_raw()
  out <- zebRabox:::process_zones(raw, 1L, tm_ldm_cfg)
  expect_true("zone" %in% names(out))
})

test_that("process_zones computes zone 1 when zones 0 and 2 are present", {
  raw <- make_zoned_raw()
  out <- zebRabox:::process_zones(raw, 1L, tm_ldm_cfg)
  expect_true("1" %in% out$zone)
})

test_that("zone 1 inact = zone 0 inact - zone 2 inact (for matching rows)", {
  raw <- make_zoned_raw()
  out <- zebRabox:::process_zones(raw, 1L, tm_ldm_cfg)

  z0 <- raw[raw$an == 0, ]
  z2 <- raw[raw$an == 2, ]
  z1 <- out[out$zone == "1", ]

  # Sort all three by animal then start for aligned comparison
  ord0 <- order(z0$animal, z0$start)
  ord2 <- order(z2$animal, z2$start)
  ord1 <- order(z1$animal, z1$start)

  expect_equal(z1$inact[ord1], z0$inact[ord0] - z2$inact[ord2], tolerance = 1e-9)
})

test_that("process_zones works with a single zone", {
  raw <- make_zoned_raw()
  raw <- raw[raw$an == 0, ]  # only zone 0
  out <- zebRabox:::process_zones(raw, 1L, tm_ldm_cfg)
  expect_true("0" %in% out$zone)
  expect_false("1" %in% out$zone)
})

# =============================================================================
# 9. assign_periods_to_txt
# =============================================================================

test_that("assign_periods_to_txt adds period_with_numbers from T column", {
  txt <- data.frame(T = c(5, 15), X = 0, Y = 0,
                    file_txt_name = "A01", stringsAsFactors = FALSE)
  per_df <- data.frame(start = 10, transition = "light1-dark1",
                       stringsAsFactors = FALSE)
  out <- zebRabox:::assign_periods_to_txt(txt, per_df, list(period_map = NULL))
  expect_equal(out$period_with_numbers, c("light1", "dark1"))
})

test_that("assign_periods_to_txt applies period_map", {
  txt <- data.frame(T = 50, X = 0, Y = 0,
                    file_txt_name = "A01", stringsAsFactors = FALSE)
  per_df <- data.frame(start = 10, transition = "light1-dark1",
                       stringsAsFactors = FALSE)
  out <- zebRabox:::assign_periods_to_txt(txt, per_df, tm_ldm_cfg)
  expect_equal(out$period_without_numbers, "dark")
})

# =============================================================================
# 10. run_processing — integration test (XLSX only, no ZIP)
# =============================================================================

make_full_raw <- function(n_rows = 8) {
  df <- make_raw(n_rows)
  df$start <- as.numeric(seq(0, by = 10, length.out = n_rows))
  df
}

test_that("run_processing returns a list with required slots", {
  set.seed(1)
  result <- run_processing(
    raw_xlsx_list = list(make_full_raw()),
    plate_plans   = list(make_plan()),
    period_df     = make_period_df(),
    removal_df    = make_removal_df(),
    cfg           = tm_ldm_cfg
  )
  expect_named(result, c("processed_data_list", "boundary_associations_list"),
               ignore.order = TRUE)
})

test_that("run_processing returns one processed df per plate", {
  set.seed(1)
  result <- run_processing(
    raw_xlsx_list = list(make_full_raw(), make_full_raw()),
    plate_plans   = list(make_plan("1"), make_plan("2")),
    period_df     = make_period_df(),
    removal_df    = rbind(make_removal_df(), data.frame(
      plate_id           = 2,
      remove_time_codes  = NA_character_,
      remove_wells       = NA_character_,
      remove_conditions  = NA_character_,
      remove_periods     = NA_character_,
      stringsAsFactors   = FALSE
    )),
    cfg = tm_ldm_cfg
  )
  expect_length(result$processed_data_list, 2L)
})

test_that("processed data has period_with_numbers column", {
  set.seed(1)
  result <- run_processing(
    raw_xlsx_list = list(make_full_raw()),
    plate_plans   = list(make_plan()),
    period_df     = make_period_df(),
    removal_df    = make_removal_df(),
    cfg           = tm_ldm_cfg
  )
  expect_true("period_with_numbers" %in% names(result$processed_data_list[[1]]))
})

test_that("processed data has condition column", {
  set.seed(1)
  result <- run_processing(
    raw_xlsx_list = list(make_full_raw()),
    plate_plans   = list(make_plan()),
    period_df     = make_period_df(),
    removal_df    = make_removal_df(),
    cfg           = tm_ldm_cfg
  )
  expect_true("condition" %in% names(result$processed_data_list[[1]]))
})

test_that("run_processing boundary_associations has correct columns", {
  set.seed(1)
  result <- run_processing(
    raw_xlsx_list = list(make_full_raw()),
    plate_plans   = list(make_plan()),
    period_df     = make_period_df(),
    removal_df    = make_removal_df(),
    cfg           = tm_ldm_cfg
  )
  ba <- result$boundary_associations_list[[1]]
  expect_true(all(c("plate_id", "time_switch", "transition") %in% names(ba)))
})

test_that("run_processing log_fn receives messages", {
  set.seed(1)
  msgs <- character()
  run_processing(
    raw_xlsx_list = list(make_full_raw()),
    plate_plans   = list(make_plan()),
    period_df     = make_period_df(),
    removal_df    = make_removal_df(),
    cfg           = tm_ldm_cfg,
    log_fn        = function(m) msgs <<- c(msgs, m)
  )
  expect_true(length(msgs) > 0)
  expect_true(any(grepl("🚀", msgs)))
})

test_that("run_processing applies removal rules", {
  set.seed(1)
  # remove animal A01 from plate 1
  rem_df <- data.frame(
    plate_id           = 1,
    remove_time_codes  = NA_character_,
    remove_wells       = "A01",
    remove_conditions  = NA_character_,
    remove_periods     = NA_character_,
    stringsAsFactors   = FALSE
  )
  result <- run_processing(
    raw_xlsx_list = list(make_full_raw()),
    plate_plans   = list(make_plan()),
    period_df     = make_period_df(),
    removal_df    = rem_df,
    cfg           = tm_ldm_cfg
  )
  out_animals <- result$processed_data_list[[1]]$animal
  expect_false("A01" %in% out_animals)
})

test_that("run_processing without ZIP has no txt slots", {
  set.seed(1)
  result <- run_processing(
    raw_xlsx_list = list(make_full_raw()),
    plate_plans   = list(make_plan()),
    period_df     = make_period_df(),
    removal_df    = make_removal_df(),
    cfg           = tm_ldm_cfg
  )
  expect_null(result$txt_by_plate)
  expect_null(result$txt_all)
  expect_null(result$zip_xlsx_match)
})

# =============================================================================
# 11. match_txt_zip_to_raw_xlsx
# =============================================================================

test_that("match_txt_zip_to_raw_xlsx returns required columns", {
  raw <- data.frame(
    location = c("ABC123_row1", "DEF456_row1"),
    animal   = c("A01", "A02"),
    an       = c(0, 0),
    stringsAsFactors = FALSE
  )
  txt <- data.frame(
    file_txt_name = c("ABC123", "DEF456"),
    T = c(0, 10), X = 0, Y = 0,
    stringsAsFactors = FALSE
  )
  out <- zebRabox:::match_txt_zip_to_raw_xlsx(raw, txt)
  expect_true(all(c("T", "X", "Y", "file_txt_name", "animal", "zone") %in% names(out)))
})

test_that("match_txt_zip_to_raw_xlsx matches on 6-char prefix", {
  raw <- data.frame(
    location = "ABC123_extra",
    animal   = "A01",
    an       = 0L,
    stringsAsFactors = FALSE
  )
  txt <- data.frame(
    file_txt_name = "ABC123",
    T = 5, X = 1, Y = 2,
    stringsAsFactors = FALSE
  )
  out <- zebRabox:::match_txt_zip_to_raw_xlsx(raw, txt)
  expect_equal(out$animal, "A01")
})

test_that("match_txt_zip_to_raw_xlsx errors on missing raw column", {
  raw <- data.frame(animal = "A01", an = 0L, stringsAsFactors = FALSE)  # no location
  txt <- data.frame(file_txt_name = "ABC123", T = 0, X = 0, Y = 0,
                    stringsAsFactors = FALSE)
  expect_error(zebRabox:::match_txt_zip_to_raw_xlsx(raw, txt), regexp = "location")
})
