# tests/testthat/test-example.R
# Tests for zebrabox_example() and the shipped demo dataset

test_that("zebrabox_example returns existing paths for both modes", {
  for (m in c("light_dark", "vibration")) {
    demo <- zebrabox_example(m)
    expect_named(demo, c("dir", "raw", "plate_plans", "period_transition", "removal"))
    expect_true(dir.exists(demo$dir))
    expect_length(demo$raw, 2)
    expect_length(demo$plate_plans, 2)
    expect_true(all(file.exists(unlist(demo[-1]))))
  }
})

test_that("zebrabox_example defaults to light_dark and rejects unknown modes", {
  expect_equal(basename(zebrabox_example()$dir), "light_dark")
  expect_error(zebrabox_example("unknown"))
})

test_that("demo dataset runs through the processing pipeline", {
  skip_on_cran()
  demo <- zebrabox_example("light_dark")
  cfg  <- set_mode("tracking", "light_dark")

  result <- run_processing(
    raw_xlsx_list = read_raw_files(demo$raw),
    plate_plans   = read_plate_plan_files(demo$plate_plans),
    period_df     = as.data.frame(readxl::read_excel(demo$period_transition)),
    removal_df    = as.data.frame(readxl::read_excel(demo$removal)),
    cfg           = cfg$processing
  )

  expect_length(result$processed_data_list, 2)
  expect_true(all(vapply(result$processed_data_list, nrow, integer(1)) > 0))
})
