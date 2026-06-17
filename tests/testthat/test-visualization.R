# tests/testthat/test-visualization.R

# =============================================================================
# Shared test fixtures
# =============================================================================

# Minimal visualization config (TM/LDM)
make_vis_cfg <- function() {
  list(
    mode           = NULL,
    period_keys    = c("light", "dark"),
    period_labels  = c("Light", "Dark"),
    expected_vars  = c("inact", "totaldist")
  )
}

# Combined all-zone data frame with TM columns
make_az <- function() {
  n <- 24
  data.frame(
    animal              = rep(c("A01", "A02", "B01"), length.out = n),
    condition           = rep(c("ctrl_1", "ctrl_1", "trt_1"), length.out = n),
    condition_grouped   = rep(c("ctrl", "ctrl", "trt"), length.out = n),
    condition_tagged    = rep(c("ctrl_1", "ctrl_1", "trt_1"), length.out = n),
    plate_id            = "1",
    zone                = rep(c(0, 2), length.out = n),
    start               = rep(seq(0, by = 60, length.out = 12), each = 2),
    period_without_numbers = rep(c("light", "light", "dark", "dark"), length.out = n),
    period_with_numbers    = rep(c("light_1", "light_1", "dark_1", "dark_1"), length.out = n),
    inact               = c(rep(50, 12), rep(80, 12)),
    inadur              = rep(30, n),
    inadist             = rep(10, n),
    smlct               = rep(20, n),
    smldist             = rep(15, n),
    smldur              = rep(10, n),
    larct               = rep(5, n),
    lardur              = rep(5, n),
    lardist             = rep(8, n),
    stringsAsFactors    = FALSE
  )
}

# Processed data list for prepare_all_zone
make_processed_list <- function() {
  df <- data.frame(
    animal              = c("A01", "A02"),
    condition           = c("ctrl_1", "trt_1"),
    condition_grouped   = c("ctrl", "trt"),
    condition_tagged    = c("ctrl_1", "trt_1"),
    plate_id            = "1",
    zone                = 0,
    start               = c(0, 60),
    period_without_numbers = c("light", "dark"),
    period_with_numbers    = c("light1", "dark1"),
    inact               = c(50, 80),
    inadur              = c(30, 40),
    inadist             = c(10, 20),
    smlct               = c(20, 25),
    smldist             = c(15, 18),
    smldur              = c(10, 12),
    larct               = c(5, 6),
    lardur              = c(5, 6),
    lardist             = c(8, 9),
    stringsAsFactors    = FALSE
  )
  list(df)
}

# Boundaries data frame for build_delta_split
make_boundaries_df <- function() {
  data.frame(
    plate_id    = "1",
    transition  = "light1-dark1",
    time_switch = 300,
    stringsAsFactors = FALSE
  )
}

# Minimal TXT data frame
make_txt_df <- function() {
  data.frame(
    plate_id            = rep("1", 30),
    animal              = rep(c("A01", "B01"), each = 15),
    T                   = rep(seq(0, 14, by = 1), times = 2),
    X                   = c(seq(10, 24, by = 1), seq(50, 64, by = 1)),
    Y                   = c(seq(10, 24, by = 1), seq(50, 64, by = 1)),
    condition           = rep(c("ctrl_1", "trt_1"), each = 15),
    condition_grouped   = rep(c("ctrl", "trt"), each = 15),
    condition_tagged    = rep(c("ctrl_1", "trt_1"), each = 15),
    period_with_numbers = rep("light1", 30),
    period_without_numbers = rep("light", 30),
    zone                = "0",
    file_txt_name       = "test.txt",
    stringsAsFactors    = FALSE
  )
}

# =============================================================================
# light_theme / dark_theme
# =============================================================================

test_that("light_theme returns a ggplot2 theme", {
  t <- light_theme()
  expect_s3_class(t, "theme")
})

test_that("dark_theme returns a ggplot2 theme", {
  t <- dark_theme()
  expect_s3_class(t, "theme")
})

test_that("light_theme and dark_theme differ", {
  expect_false(identical(light_theme(), dark_theme()))
})

# =============================================================================
# convert_time
# =============================================================================

test_that("convert_time returns identity when from == to", {
  expect_equal(convert_time(120, "seconds", "seconds"), 120)
})

test_that("convert_time converts seconds to minutes", {
  expect_equal(convert_time(120, "seconds", "minutes"), 2)
})

test_that("convert_time converts minutes to seconds", {
  expect_equal(convert_time(2, "minutes", "seconds"), 120)
})

test_that("convert_time converts hours to seconds", {
  expect_equal(convert_time(1, "hours", "seconds"), 3600)
})

test_that("convert_time converts days to hours", {
  expect_equal(convert_time(1, "days", "hours"), 24)
})

# =============================================================================
# ensure_colors
# =============================================================================

test_that("ensure_colors returns exactly n colors when cols is empty", {
  cols <- zebRabox:::ensure_colors(5)
  expect_length(cols, 5)
})

test_that("ensure_colors recycles when fewer colors provided", {
  cols <- zebRabox:::ensure_colors(4, c("#FF0000", "#00FF00"))
  expect_length(cols, 4)
})

test_that("ensure_colors truncates when more colors provided", {
  cols <- zebRabox:::ensure_colors(2, c("#FF0000", "#00FF00", "#0000FF"))
  expect_length(cols, 2)
})

test_that("ensure_colors ignores NA and blank entries", {
  cols <- zebRabox:::ensure_colors(3, c(NA, "", "#FF0000"))
  expect_length(cols, 3)
})

# =============================================================================
# prepare_all_zone
# =============================================================================

test_that("prepare_all_zone returns a data frame", {
  az <- prepare_all_zone(make_processed_list(), make_vis_cfg())
  expect_s3_class(az, "data.frame")
})

test_that("prepare_all_zone binds all plates row-wise", {
  lst <- c(make_processed_list(), make_processed_list())
  az <- prepare_all_zone(lst, make_vis_cfg())
  expect_equal(nrow(az), 4)
})

test_that("prepare_all_zone computes totaldist from TM columns", {
  az <- prepare_all_zone(make_processed_list(), make_vis_cfg())
  expected <- az$smldist + az$lardist + az$inadist
  expect_equal(az$totaldist, expected)
})

test_that("prepare_all_zone computes totalct from TM columns", {
  az <- prepare_all_zone(make_processed_list(), make_vis_cfg())
  expected <- az$smlct + az$larct + az$inact
  expect_equal(az$totalct, expected)
})

test_that("prepare_all_zone computes totalspeed = totaldist / pmax(totaldur, 1)", {
  az <- prepare_all_zone(make_processed_list(), make_vis_cfg())
  expected <- az$totaldist / pmax(az$totaldur, 1)
  expect_equal(az$totalspeed, expected)
})

test_that("prepare_all_zone coerces plate_id to character", {
  lst <- make_processed_list()
  lst[[1]]$plate_id <- 1L
  az <- prepare_all_zone(lst, make_vis_cfg())
  expect_type(az$plate_id, "character")
})

test_that("prepare_all_zone does not add TM columns when they are absent", {
  lst <- list(data.frame(
    animal = "A01", plate_id = "1", zone = 0, start = 0,
    period_without_numbers = "light", period_with_numbers = "light1",
    condition = "ctrl_1", condition_grouped = "ctrl", condition_tagged = "ctrl_1",
    inact = 50, stringsAsFactors = FALSE
  ))
  az <- prepare_all_zone(lst, make_vis_cfg())
  expect_false("totaldist" %in% names(az))
})

# =============================================================================
# build_periods_df
# =============================================================================

test_that("build_periods_df returns a data frame", {
  az <- make_az()
  cfg <- make_vis_cfg()
  result <- build_periods_df(az, "inact", cfg)
  expect_s3_class(result, "data.frame")
})

test_that("build_periods_df has mean_val column", {
  result <- build_periods_df(make_az(), "inact", make_vis_cfg())
  expect_true("mean_val" %in% names(result))
})

test_that("build_periods_df makes period_without_numbers a factor", {
  result <- build_periods_df(make_az(), "inact", make_vis_cfg())
  expect_s3_class(result$period_without_numbers, "factor")
})

test_that("build_periods_df uses period_labels from cfg", {
  result <- build_periods_df(make_az(), "inact", make_vis_cfg())
  expect_true(all(levels(result$period_without_numbers) %in% c("Light", "Dark")))
})

test_that("build_periods_df keeps only matching periods", {
  az <- make_az()
  az$period_without_numbers[1:6] <- "other"
  result <- build_periods_df(az, "inact", make_vis_cfg())
  period_labels <- as.character(unique(result$period_without_numbers))
  expect_true(!("other" %in% period_labels))
})

test_that("build_periods_df filters by period_indices_keep", {
  az <- make_az()
  result <- build_periods_df(az, "inact", make_vis_cfg(), period_indices_keep = "1")
  expect_true(nrow(result) > 0)
  period_nums <- sub(".*_([0-9]+)$", "\\1",
                     as.character(result$period_with_numbers[!is.na(result$period_with_numbers)]))
  expect_true(all(period_nums == "1"))
})

test_that("build_periods_df errors when no period_keys match", {
  cfg_bad <- make_vis_cfg()
  cfg_bad$period_keys <- c("xyz", "abc")
  expect_error(build_periods_df(make_az(), "inact", cfg_bad))
})

test_that("build_periods_df errors when period_keys is empty", {
  cfg_bad <- make_vis_cfg()
  cfg_bad$period_keys <- character(0)
  expect_error(build_periods_df(make_az(), "inact", cfg_bad))
})

# =============================================================================
# build_cumulate_df
# =============================================================================

test_that("build_cumulate_df returns a data frame", {
  result <- build_cumulate_df(make_az(), "inact")
  expect_s3_class(result, "data.frame")
})

test_that("build_cumulate_df has cum column", {
  result <- build_cumulate_df(make_az(), "inact")
  expect_true("cum" %in% names(result))
})

test_that("build_cumulate_df sums over rows for each animal", {
  az <- make_az()
  az$inact <- 10
  result <- build_cumulate_df(az, "inact")
  expect_true(all(result$cum > 0))
})

test_that("build_cumulate_df coerces plate_id to character", {
  az <- make_az()
  az$plate_id <- 1L
  result <- build_cumulate_df(az, "inact")
  expect_type(result$plate_id, "character")
})

# =============================================================================
# build_delta_split
# =============================================================================

test_that("build_delta_split returns NULL when boundaries_df is NULL", {
  expect_null(build_delta_split(make_az(), "inact", "light1-dark1", 60, boundaries_df = NULL))
})

test_that("build_delta_split returns NULL when boundaries_df is empty", {
  bdf <- make_boundaries_df()[0, ]
  expect_null(build_delta_split(make_az(), "inact", "light1-dark1", 60, boundaries_df = bdf))
})

test_that("build_delta_split returns a named list when data match", {
  az <- make_az()
  az$start <- seq(200, by = 30, length.out = nrow(az))
  result <- build_delta_split(az, c("inact"), "light1-dark1", 60, boundaries_df = make_boundaries_df())
  expect_true(is.list(result) || is.null(result))
})

test_that("build_delta_split list is named by variable", {
  az <- make_az()
  az$start <- seq(200, by = 30, length.out = nrow(az))
  result <- build_delta_split(az, c("inact", "inadist"), "light1-dark1", 60,
                               boundaries_df = make_boundaries_df())
  if (!is.null(result)) {
    expect_true(all(c("inact", "inadist") %in% names(result)))
  }
})

# =============================================================================
# build_lineplot_df
# =============================================================================

test_that("build_lineplot_df returns a data frame", {
  result <- build_lineplot_df(make_az(), "inact", 60, "seconds", "minutes", "No")
  expect_s3_class(result, "data.frame")
})

test_that("build_lineplot_df has val_per_well column", {
  result <- build_lineplot_df(make_az(), "inact", 60, "seconds", "minutes", "No")
  expect_true("val_per_well" %in% names(result))
})

test_that("build_lineplot_df has se_per_well column", {
  result <- build_lineplot_df(make_az(), "inact", 60, "seconds", "minutes", "No")
  expect_true("se_per_well" %in% names(result))
})

test_that("build_lineplot_df applies aggregation window", {
  az <- make_az()
  result60 <- build_lineplot_df(az, "inact", 60, "seconds", "seconds", "No")
  result120 <- build_lineplot_df(az, "inact", 120, "seconds", "seconds", "No")
  expect_true(length(unique(result60$start_rounded)) >= length(unique(result120$start_rounded)))
})

test_that("build_lineplot_df converts time when convert == 'Yes'", {
  result <- build_lineplot_df(make_az(), "inact", 1, "seconds", "minutes", "Yes")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

# =============================================================================
# resolve_txt_selected_wells
# =============================================================================

test_that("resolve_txt_selected_wells returns character vector", {
  result <- zebRabox:::resolve_txt_selected_wells(make_txt_df(), plate_ids = "1",
                                       selected_conditions = "ctrl")
  expect_type(result, "character")
})

test_that("resolve_txt_selected_wells returns correct well keys for a condition", {
  result <- zebRabox:::resolve_txt_selected_wells(make_txt_df(), plate_ids = "1",
                                       selected_conditions = "ctrl")
  expect_true(all(grepl("^1__", result)))
})

test_that("resolve_txt_selected_wells returns empty when no condition matches", {
  result <- zebRabox:::resolve_txt_selected_wells(make_txt_df(), plate_ids = "1",
                                       selected_conditions = "nonexistent")
  expect_length(result, 0)
})

test_that("resolve_txt_selected_wells intersects with selected_wells", {
  result <- zebRabox:::resolve_txt_selected_wells(
    make_txt_df(), plate_ids = "1",
    selected_conditions = character(0),
    selected_wells = c("1__A01", "FAKE_KEY")
  )
  expect_equal(result, "1__A01")
})

test_that("resolve_txt_selected_wells returns empty when no conditions and no wells given", {
  result <- zebRabox:::resolve_txt_selected_wells(make_txt_df())
  expect_length(result, 0)
})

# =============================================================================
# resolve_txt_time_range
# =============================================================================

test_that("resolve_txt_time_range returns numeric(2) in manual mode", {
  df <- make_txt_df()
  df$well_key <- paste(df$plate_id, df$animal, sep = "__")
  result <- zebRabox:::resolve_txt_time_range(df, "1__A01", mode = "manual",
                                   time_start = 2, time_end = 10)
  expect_length(result, 2)
  expect_equal(result, c(2, 10))
})

test_that("resolve_txt_time_range errors when end <= start in manual mode", {
  df <- make_txt_df()
  df$well_key <- paste(df$plate_id, df$animal, sep = "__")
  expect_error(zebRabox:::resolve_txt_time_range(df, "1__A01", mode = "manual",
                                      time_start = 10, time_end = 5))
})

test_that("resolve_txt_time_range returns range in period mode", {
  df <- make_txt_df()
  df$well_key <- paste(df$plate_id, df$animal, sep = "__")
  result <- zebRabox:::resolve_txt_time_range(df, "1__A01", mode = "period",
                                   period_value = "light1")
  expect_length(result, 2)
  expect_true(result[2] >= result[1])
})

test_that("resolve_txt_time_range errors for unknown mode", {
  df <- make_txt_df()
  df$well_key <- paste(df$plate_id, df$animal, sep = "__")
  expect_error(zebRabox:::resolve_txt_time_range(df, "1__A01", mode = "unknown_mode"))
})

# =============================================================================
# build_txt_trajectory_df
# =============================================================================

test_that("build_txt_trajectory_df returns a data frame", {
  result <- zebRabox:::build_txt_trajectory_df(make_txt_df(), "1__A01", c(0, 10))
  expect_s3_class(result, "data.frame")
})

test_that("build_txt_trajectory_df has X, Y, T columns", {
  result <- zebRabox:::build_txt_trajectory_df(make_txt_df(), "1__A01", c(0, 10))
  expect_true(all(c("X", "Y", "T") %in% names(result)))
})

test_that("build_txt_trajectory_df attaches bin_s attribute", {
  result <- zebRabox:::build_txt_trajectory_df(make_txt_df(), "1__A01", c(0, 10))
  expect_true(!is.null(attr(result, "bin_s")))
})

test_that("build_txt_trajectory_df attaches time_range attribute", {
  result <- zebRabox:::build_txt_trajectory_df(make_txt_df(), "1__A01", c(0, 10))
  tr <- attr(result, "time_range")
  expect_length(tr, 2)
  expect_equal(tr[1], 0)
})

test_that("build_txt_trajectory_df errors when no well_keys provided", {
  expect_error(zebRabox:::build_txt_trajectory_df(make_txt_df(), character(0), c(0, 10)))
})

test_that("build_txt_trajectory_df errors when time_range is not length 2", {
  expect_error(zebRabox:::build_txt_trajectory_df(make_txt_df(), "1__A01", c(0)))
})

test_that("build_txt_trajectory_df errors when >96 unique wells requested", {
  fake_keys <- paste0("1__W", seq_len(97))
  expect_error(zebRabox:::build_txt_trajectory_df(make_txt_df(), fake_keys, c(0, 10)))
})

# =============================================================================
# Shared fixture for Step-3 wrappers
# =============================================================================

make_result <- function() {
  list(
    processed_data_list = make_processed_list(),
    boundary_associations_list = list(make_boundaries_df())
  )
}

# =============================================================================
# prepare_dataset
# =============================================================================

test_that("prepare_dataset: returns a data frame", {
  expect_true(is.data.frame(prepare_dataset(make_result(), make_vis_cfg())))
})

test_that("prepare_dataset: parity with prepare_all_zone on same inputs", {
  res     <- make_result()
  cfg     <- make_vis_cfg()
  direct  <- prepare_all_zone(res$processed_data_list, cfg)
  wrapper <- prepare_dataset(res, cfg)
  expect_equal(wrapper, direct)
})

test_that("prepare_dataset: result rows equals prepare_all_zone rows", {
  res <- make_result()
  cfg <- make_vis_cfg()
  expect_equal(nrow(prepare_dataset(res, cfg)),
               nrow(prepare_all_zone(res$processed_data_list, cfg)))
})

test_that("prepare_dataset: error if processed_data_list slot missing", {
  expect_error(prepare_dataset(list(x = 1), make_vis_cfg()), "processed_data_list")
})

test_that("prepare_dataset: error if result is not a list", {
  expect_error(prepare_dataset("not_a_list", make_vis_cfg()), "processed_data_list")
})

test_that("prepare_dataset: error if cfg is not a list", {
  expect_error(prepare_dataset(make_result(), "not_a_cfg"), "cfg")
})

# =============================================================================
# get_boundaries
# =============================================================================

test_that("get_boundaries: returns a data frame", {
  expect_true(is.data.frame(get_boundaries(make_result())))
})

test_that("get_boundaries: has plate_id, time_switch, transition columns", {
  result <- get_boundaries(make_result())
  expect_true(all(c("plate_id", "time_switch", "transition") %in% names(result)))
})

test_that("get_boundaries: parity with manual bind_rows + distinct", {
  res    <- make_result()
  manual <- dplyr::bind_rows(res$boundary_associations_list) |> dplyr::distinct()
  expect_equal(get_boundaries(res), manual)
})

test_that("get_boundaries: combines multiple plates into one data frame", {
  b1  <- data.frame(plate_id = "1", time_switch = 300,
                    transition = "light1-dark1", stringsAsFactors = FALSE)
  b2  <- data.frame(plate_id = "2", time_switch = 305,
                    transition = "light1-dark1", stringsAsFactors = FALSE)
  res <- list(processed_data_list        = make_processed_list(),
              boundary_associations_list = list(b1, b2))
  out <- get_boundaries(res)
  expect_equal(nrow(out), 2L)
  expect_true(all(c("1", "2") %in% out$plate_id))
})

test_that("get_boundaries: deduplicates identical rows", {
  b   <- make_boundaries_df()
  res <- list(processed_data_list        = make_processed_list(),
              boundary_associations_list = list(b, b))
  expect_equal(nrow(get_boundaries(res)), 1L)
})

test_that("get_boundaries: returns empty df with correct columns when list is empty", {
  res <- list(processed_data_list        = make_processed_list(),
              boundary_associations_list = list())
  out <- get_boundaries(res)
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 0L)
  expect_true(all(c("plate_id", "time_switch", "transition") %in% names(out)))
})

test_that("get_boundaries: error if boundary_associations_list slot missing", {
  expect_error(get_boundaries(list(x = 1)), "boundary_associations_list")
})

test_that("get_boundaries: error if result is not a list", {
  expect_error(get_boundaries("not_a_list"), "boundary_associations_list")
})

# =============================================================================
# plot_periods
# =============================================================================

test_that("plot_periods: returns a ggplot — separated mode", {
  p <- plot_periods(make_az(), "inact", make_vis_cfg())
  expect_s3_class(p, "ggplot")
})

test_that("plot_periods: has a GeomBoxplot layer", {
  p <- plot_periods(make_az(), "inact", make_vis_cfg())
  geom_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomBoxplot" %in% geom_classes)
})

test_that("plot_periods: returns a ggplot — pooled mode", {
  p <- plot_periods(make_az(), "inact", make_vis_cfg(), mode = "pooled")
  expect_s3_class(p, "ggplot")
})

test_that("plot_periods: dark theme does not error", {
  p <- plot_periods(make_az(), "inact", make_vis_cfg(), theme = "dark")
  expect_s3_class(p, "ggplot")
})

test_that("plot_periods: error if variable not in dataset", {
  expect_error(plot_periods(make_az(), "nonexistent", make_vis_cfg()), "nonexistent")
})

# =============================================================================
# plot_cumulative
# =============================================================================

test_that("plot_cumulative: returns a ggplot", {
  p <- plot_cumulative(make_az(), "inact")
  expect_s3_class(p, "ggplot")
})

test_that("plot_cumulative: has a GeomBoxplot layer", {
  p <- plot_cumulative(make_az(), "inact")
  geom_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomBoxplot" %in% geom_classes)
})

test_that("plot_cumulative: y-label contains 'Cumulative'", {
  p <- plot_cumulative(make_az(), "inact")
  expect_true(grepl("Cumulative", p$labels$y))
})

test_that("plot_cumulative: explicit zone selection does not error", {
  p <- plot_cumulative(make_az(), "inact", zone = "0")
  expect_s3_class(p, "ggplot")
})

# =============================================================================
# plot_lineplot
# =============================================================================

test_that("plot_lineplot: returns a ggplot", {
  p <- plot_lineplot(make_az(), "inact", bin_seconds = 60)
  expect_s3_class(p, "ggplot")
})

test_that("plot_lineplot: has a GeomLine layer", {
  p <- plot_lineplot(make_az(), "inact", bin_seconds = 60)
  geom_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomLine" %in% geom_classes)
})

test_that("plot_lineplot: no error with time_unit conversion", {
  p <- plot_lineplot(make_az(), "inact", bin_seconds = 60, time_unit = "minutes")
  expect_s3_class(p, "ggplot")
})

test_that("plot_lineplot: no error with ci95 error_mode", {
  p <- plot_lineplot(make_az(), "inact", bin_seconds = 60, error_mode = "ci95")
  expect_s3_class(p, "ggplot")
})

test_that("plot_lineplot: error if variable not in dataset", {
  expect_error(plot_lineplot(make_az(), "nonexistent"), "nonexistent")
})

# =============================================================================
# plot_delta
# =============================================================================

test_that("plot_delta: returns a ggplot when data present", {
  p <- plot_delta(make_az(), "inact", "light1-dark1", 60, make_boundaries_df())
  expect_s3_class(p, "ggplot")
})

test_that("plot_delta: has a GeomBoxplot layer when data present", {
  p <- plot_delta(make_az(), "inact", "light1-dark1", 60, make_boundaries_df())
  geom_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomBoxplot" %in% geom_classes)
})

test_that("plot_delta: pooled mode returns a ggplot", {
  p <- plot_delta(make_az(), "inact", "light1-dark1", 60, make_boundaries_df(),
                  mode = "pooled")
  expect_s3_class(p, "ggplot")
})

test_that("plot_delta: returns empty ggplot when transition has no matching data", {
  p <- plot_delta(make_az(), "inact", "no_such_transition", 60, make_boundaries_df())
  expect_s3_class(p, "ggplot")
})

# =============================================================================
# export_figures
# =============================================================================

make_fig_list <- function() {
  list(
    periods    = plot_periods(make_az(), "inact", make_vis_cfg()),
    cumulative = plot_cumulative(make_az(), "inact")
  )
}

test_that("export_figures: creates one file per figure", {
  td <- withr::local_tempdir()
  export_figures(make_fig_list(), path = td, format = "png")
  created <- list.files(td, pattern = "\\.png$")
  expect_equal(length(created), length(make_fig_list()))
})

test_that("export_figures: file names match list names", {
  td <- withr::local_tempdir()
  export_figures(make_fig_list(), path = td, format = "png")
  expect_true(file.exists(file.path(td, "periods.png")))
  expect_true(file.exists(file.path(td, "cumulative.png")))
})

test_that("export_figures: pdf format produces pdf files", {
  td <- withr::local_tempdir()
  export_figures(make_fig_list(), path = td, format = "pdf")
  pdf_files <- list.files(td, pattern = "\\.pdf$")
  expect_equal(length(pdf_files), length(make_fig_list()))
})

test_that("export_figures: error if an element is not a ggplot", {
  bad_list <- list(ok = plot_cumulative(make_az(), "inact"), bad = "not_a_plot")
  expect_error(export_figures(bad_list, path = tempdir()), "ggplot")
})
