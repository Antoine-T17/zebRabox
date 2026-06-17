# tests/testthat/test-config.R

# =============================================================================
# get_processing_config
# =============================================================================

test_that("get_processing_config returns a list", {
  expect_type(get_processing_config("tm_ldm"), "list")
})

test_that("get_processing_config has required fields", {
  cfg <- get_processing_config("tm_ldm")
  expect_true(all(c("ui_title","period_map","period_keys",
                    "convert_cols","zone_num_cols","filter_fn") %in% names(cfg)))
})

test_that("get_processing_config tm_ldm has light/dark period_keys", {
  cfg <- get_processing_config("tm_ldm")
  expect_equal(cfg$period_keys, c("light", "dark"))
})

test_that("get_processing_config tm_vm has vibration/rest period_keys", {
  cfg <- get_processing_config("tm_vm")
  expect_equal(cfg$period_keys, c("vibration", "rest"))
})

test_that("get_processing_config qm_ldm has light/dark period_keys", {
  cfg <- get_processing_config("qm_ldm")
  expect_equal(cfg$period_keys, c("light", "dark"))
})

test_that("get_processing_config qm_vm has vibration/rest period_keys", {
  cfg <- get_processing_config("qm_vm")
  expect_equal(cfg$period_keys, c("vibration", "rest"))
})

test_that("get_processing_config tm_ldm has NULL filter_fn", {
  expect_null(get_processing_config("tm_ldm")$filter_fn)
})

test_that("get_processing_config tm_vm has NULL filter_fn", {
  expect_null(get_processing_config("tm_vm")$filter_fn)
})

test_that("get_processing_config qm_ldm has a non-NULL filter_fn", {
  expect_true(is.function(get_processing_config("qm_ldm")$filter_fn))
})

test_that("get_processing_config qm_vm has a non-NULL filter_fn", {
  expect_true(is.function(get_processing_config("qm_vm")$filter_fn))
})

test_that("get_processing_config period_map maps light correctly for ldm modes", {
  for (m in c("tm_ldm", "qm_ldm")) {
    pm <- get_processing_config(m)$period_map
    expect_equal(pm("light1"), "light")
    expect_equal(pm("dark2"),  "dark")
    expect_equal(pm("other"),  "other")
  }
})

test_that("get_processing_config period_map maps vibration/rest correctly for vm modes", {
  for (m in c("tm_vm", "qm_vm")) {
    pm <- get_processing_config(m)$period_map
    expect_equal(pm("vibration1"), "vibration")
    expect_equal(pm("rest2"),      "rest")
    expect_equal(pm("other"),      "other")
  }
})

test_that("get_processing_config qm filter_fn keeps quantauc rows", {
  fn  <- get_processing_config("qm_ldm")$filter_fn
  df  <- data.frame(datatype = c("quantauc", "other", "quantauc"),
                    value = 1:3, stringsAsFactors = FALSE)
  out <- fn(df)
  expect_equal(nrow(out), 2)
  expect_true(all(out$datatype == "quantauc"))
})

test_that("get_processing_config qm filter_fn returns df unchanged when no datatype col", {
  fn  <- get_processing_config("qm_ldm")$filter_fn
  df  <- data.frame(value = 1:3)
  out <- fn(df)
  expect_equal(nrow(out), 3)
})

test_that("get_processing_config unknown mode falls back to tm_ldm", {
  cfg <- get_processing_config("nonexistent_mode")
  expect_equal(cfg$period_keys, c("light", "dark"))
  expect_null(cfg$filter_fn)
})

test_that("get_processing_config tm convert_cols contains inact", {
  expect_true("inact" %in% get_processing_config("tm_ldm")$convert_cols)
})

test_that("get_processing_config qm convert_cols contains frect", {
  expect_true("frect" %in% get_processing_config("qm_ldm")$convert_cols)
})

# =============================================================================
# get_visualization_config
# =============================================================================

test_that("get_visualization_config returns a list", {
  expect_type(get_visualization_config("tm_ldm"), "list")
})

test_that("get_visualization_config has required fields", {
  cfg <- get_visualization_config("tm_ldm")
  expect_true(all(c("ui_title","expected_vars","period_keys",
                    "period_labels","period_ui_name",
                    "period_default_colors") %in% names(cfg)))
})

test_that("get_visualization_config tm_ldm has light/dark period_keys", {
  cfg <- get_visualization_config("tm_ldm")
  expect_equal(cfg$period_keys, c("light", "dark"))
})

test_that("get_visualization_config tm_vm has vibration/rest period_keys", {
  cfg <- get_visualization_config("tm_vm")
  expect_equal(cfg$period_keys, c("vibration", "rest"))
})

test_that("get_visualization_config qm modes have correct expected_vars", {
  cfg <- get_visualization_config("qm_ldm")
  expect_true("frect" %in% cfg$expected_vars)
  expect_false("totaldist" %in% cfg$expected_vars)
})

test_that("get_visualization_config tm modes have correct expected_vars", {
  cfg <- get_visualization_config("tm_ldm")
  expect_true("totaldist" %in% cfg$expected_vars)
  expect_false("frect" %in% cfg$expected_vars)
})

test_that("get_visualization_config period_keys and period_labels same length", {
  for (m in c("tm_ldm","tm_vm","qm_ldm","qm_vm")) {
    cfg <- get_visualization_config(m)
    expect_equal(length(cfg$period_keys), length(cfg$period_labels))
  }
})

test_that("get_visualization_config unknown mode falls back to tm_ldm", {
  cfg <- get_visualization_config("nonexistent_mode")
  expect_equal(cfg$period_keys, c("light", "dark"))
  expect_true("totaldist" %in% cfg$expected_vars)
})

test_that("get_visualization_config period_default_colors is a non-empty string", {
  for (m in c("tm_ldm","tm_vm","qm_ldm","qm_vm")) {
    cols <- get_visualization_config(m)$period_default_colors
    expect_type(cols, "character")
    expect_true(nzchar(cols))
  }
})

# =============================================================================
# set_mode
# =============================================================================

# -- Return structure --

test_that("set_mode: returns a list", {
  expect_type(set_mode("tracking", "light_dark"), "list")
})

test_that("set_mode: return list has processing, visualization, mode_key slots", {
  result <- set_mode("tracking", "light_dark")
  expect_true(all(c("processing", "visualization", "mode_key") %in% names(result)))
})

test_that("set_mode: $processing is a list", {
  expect_type(set_mode("tracking", "light_dark")$processing, "list")
})

test_that("set_mode: $visualization is a list", {
  expect_type(set_mode("tracking", "light_dark")$visualization, "list")
})

test_that("set_mode: $mode_key is a character scalar", {
  result <- set_mode("tracking", "light_dark")
  expect_type(result$mode_key, "character")
  expect_length(result$mode_key, 1L)
})

# -- Correct mode_key for all 4 combinations --

test_that("set_mode: tracking + light_dark → mode_key tm_ldm", {
  expect_equal(set_mode("tracking", "light_dark")$mode_key, "tm_ldm")
})

test_that("set_mode: tracking + vibration → mode_key tm_vm", {
  expect_equal(set_mode("tracking", "vibration")$mode_key, "tm_vm")
})

test_that("set_mode: quantization + light_dark → mode_key qm_ldm", {
  expect_equal(set_mode("quantization", "light_dark")$mode_key, "qm_ldm")
})

test_that("set_mode: quantization + vibration → mode_key qm_vm", {
  expect_equal(set_mode("quantization", "vibration")$mode_key, "qm_vm")
})

# -- Parity with get_processing_config / get_visualization_config --

test_that("set_mode: $processing$period_keys matches get_processing_config", {
  combos <- list(
    list(p = "tracking",      s = "light_dark", key = "tm_ldm"),
    list(p = "tracking",      s = "vibration",  key = "tm_vm"),
    list(p = "quantization",  s = "light_dark", key = "qm_ldm"),
    list(p = "quantization",  s = "vibration",  key = "qm_vm")
  )
  for (m in combos) {
    direct  <- get_processing_config(m$key)
    wrapper <- set_mode(m$p, m$s)$processing
    expect_equal(wrapper$period_keys,   direct$period_keys,   info = m$key)
    expect_equal(wrapper$ui_title,      direct$ui_title,      info = m$key)
    expect_equal(wrapper$convert_cols,  direct$convert_cols,  info = m$key)
    expect_equal(wrapper$zone_num_cols, direct$zone_num_cols, info = m$key)
    expect_equal(is.null(wrapper$filter_fn), is.null(direct$filter_fn), info = m$key)
  }
})

test_that("set_mode: $visualization$period_keys matches get_visualization_config", {
  combos <- list(
    list(p = "tracking",      s = "light_dark", key = "tm_ldm"),
    list(p = "tracking",      s = "vibration",  key = "tm_vm"),
    list(p = "quantization",  s = "light_dark", key = "qm_ldm"),
    list(p = "quantization",  s = "vibration",  key = "qm_vm")
  )
  for (m in combos) {
    direct  <- get_visualization_config(m$key)
    wrapper <- set_mode(m$p, m$s)$visualization
    expect_equal(wrapper$period_keys,   direct$period_keys,   info = m$key)
    expect_equal(wrapper$period_labels, direct$period_labels, info = m$key)
    expect_equal(wrapper$expected_vars, direct$expected_vars, info = m$key)
    expect_equal(wrapper$ui_title,      direct$ui_title,      info = m$key)
  }
})

# -- Input validation --

test_that("set_mode: error for invalid primary", {
  expect_error(set_mode("tracking_wrong", "light_dark"), "'primary' must be one of")
})

test_that("set_mode: error for invalid secondary", {
  expect_error(set_mode("tracking", "wrong_stimulus"), "'secondary' must be one of")
})

test_that("set_mode: error message names the invalid value", {
  expect_error(set_mode("bad_value", "light_dark"), "bad_value")
})

# -- Case and whitespace tolerance --

test_that("set_mode: case-insensitive primary", {
  expect_equal(set_mode("Tracking", "light_dark")$mode_key, "tm_ldm")
  expect_equal(set_mode("QUANTIZATION", "light_dark")$mode_key, "qm_ldm")
})

test_that("set_mode: case-insensitive secondary", {
  expect_equal(set_mode("tracking", "Light_Dark")$mode_key, "tm_ldm")
  expect_equal(set_mode("tracking", "VIBRATION")$mode_key,  "tm_vm")
})

test_that("set_mode: leading/trailing whitespace tolerated", {
  expect_equal(set_mode("  tracking  ", "  light_dark  ")$mode_key, "tm_ldm")
})
