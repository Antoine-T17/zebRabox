# ======================================================================
# zebRabox — Standalone API Template
# ======================================================================
# All 27 public functions are shown with their key parameters.
# Adapt paths and parameters to your experiment before running.
# ======================================================================

library(zebRabox)

setwd("C:/Users/Antoine Local/Desktop/PhD 2024-2027/Cahier de laboratoire/others/projects/R packages/zebRabox_claude/inst/examples/all")


# ── MODE ──────────────────────────────────────────────────────────────
# set_mode(primary, secondary)
#   primary   : "tracking"     | "quantization"
#   secondary : "light_dark"   | "vibration"
#
cfg <- set_mode("tracking", "light_dark")
# cfg$processing    → pass to run_processing()
# cfg$visualization → pass to prepare_dataset() and plot_*()
# cfg$mode_key      → "tm_ldm" | "tm_vm" | "qm_ldm" | "qm_vm"

# Advanced: access configs directly (same result as set_mode)
# get_processing_config("tm_ldm")
# get_visualization_config("tm_ldm")


# ── PLATE PLAN ────────────────────────────────────────────────────────
# create_plate_plan(plate_type, conditions, n_replicates, n_units,
#                   n_plates, include_borders, seed, name)
plates <- create_plate_plan(
  plate_type   = "96",     # "12" | "24" | "48" | "96"
  conditions   = c("ctrl", "trt"),
  n_replicates = 4,        # replicates per condition
  n_units      = 6,        # animals per replicate
  n_plates     = 2,
  seed         = 42
)

# read_plate_plan_files(paths, names = basename(paths))
plates <- read_plate_plan_files(
  paths = c("plate_plan_plate_4.xlsx", "plate_plan_plate_5.xlsx", "plate_plan_plate_6.xlsx", "plate_plan_plate_7.xlsx", "plate_plan_plate_8.xlsx", "plate_plan_plate_9.xlsx")
)


# ── DATA IMPORT ───────────────────────────────────────────────────────
# read_raw_files(paths, names = basename(paths))
raw_data <- read_raw_files(
  paths = c("plaque_4_light_dark_24_04_2026.xlsx", "plaque_5_light_dark_24_04_2026.xlsx", "plaque_6_light_dark_24_04_2026.xlsx",
            "plaque_7_light_dark_30_04_2026.xlsx", "plaque_8_light_dark_30_04_2026.xlsx", "plaque_9_light_dark_30_04_2026.xlsx")
)

# read_raw_file(path, file_name = basename(path))  — single file
# df <- read_raw_file("plate1.xlsx")

# create_period_table(start, transition)
period_df <- create_period_table(
  start      = c(1800, 2400, 2700, 3300, 3600, 4200),
  transition = c("acclimatation-light_1", "light_1-dark_1", "dark_1-light_2", "light_2-dark_2", "dark_2-light_3", "light_3-dark_3")
)

# create_removal_table(plate_id, remove_time_codes, remove_wells,
#                       remove_conditions, remove_periods)
removal_df <- create_removal_table(
  plate_id          = c(4, 6, 8),
  remove_wells      = c("C03", "C01", "C03"), # plate 1: remove A01 & B01; plate 2: none
  remove_conditions = "X",               # NA = skip for all plates
  remove_periods    = NA,
  remove_time_codes = NA
)


# ── PROCESSING ────────────────────────────────────────────────────────
# run_processing(raw_xlsx_list, plate_plans, period_df, removal_df, cfg,
#                raw_zip_list, log_fn, progress_fn)
result <- run_processing(
  raw_xlsx_list = raw_data,
  plate_plans   = plates,
  period_df     = period_df,
  removal_df    = removal_df,
  cfg           = cfg$processing
)

# With optional ZIP trajectory archives
# result <- run_processing(
#   raw_xlsx_list = raw_data,
#   raw_zip_list  = zip_data,           # list from read_raw_files(.zip)
#   plate_plans   = plates,
#   period_df     = period_df,
#   removal_df    = removal_df,
#   cfg           = cfg$processing
# )


# ── VISUALIZATION DATASETS ────────────────────────────────────────────
# prepare_dataset(result, cfg)  — wrapper for prepare_all_zone()
dataset <- prepare_dataset(result, cfg$visualization)

# get_boundaries(result)  — extracts transition time points
bounds  <- get_boundaries(result)

# prepare_all_zone(processed_data_list, cfg)  — lower-level alternative
# dataset <- prepare_all_zone(result$processed_data_list, cfg$visualization)

# build_periods_df(az, v, cfg, period_indices_keep)
periods_df <- build_periods_df(dataset, "totaldist", cfg$visualization)
periods_df <- build_periods_df(dataset, "totaldist", cfg$visualization,
                                period_indices_keep = NULL)
# build_cumulate_df(az, v)
cumul_df <- build_cumulate_df(dataset, "totaldist")

# build_lineplot_df(az, v, agg_period, unit_from, unit_to, convert)
line_df  <- build_lineplot_df(dataset, "totaldist",
                               agg_period = 60, unit_from = "seconds",
                               unit_to = "minutes", convert = "Yes")

# build_delta_split(az, vars, transition, delta_sec, boundaries_df)
delta_list <- build_delta_split(dataset, "totaldist",
                                 "dark_1-light_2", delta_sec = 300,
                                 boundaries_df = bounds)


# ── FIGURES ───────────────────────────────────────────────────────────
# All plot_*() functions return ggplot2 objects.
# theme = "light" (default) | "dark"
# mode  = "separated" (default) | "pooled"

# plot_periods(dataset, variable, cfg, zone, mode, period_colors,
#              period_indices_keep, condition_order, condition_colors, theme)
p_periods <- plot_periods(
  dataset  = dataset,
  variable = "totaldist",
  cfg      = cfg$visualization,
  mode     = "separated"
)

# plot_cumulative(dataset, variable, zone, condition_order,
#                 condition_colors, theme)
p_cumul <- plot_cumulative(dataset, variable = "totaldist")

# plot_lineplot(dataset, variable, bin_seconds, time_unit, error_mode,
#               zone, condition_order, condition_colors, theme)
p_line <- plot_lineplot(
  dataset     = dataset,
  variable    = "totaldist",
  bin_seconds = 60,
  time_unit   = "minutes",   # x-axis unit: "seconds"|"minutes"|"hours"|"days"
  error_mode  = "error_bar"  # "error_bar" | "ci95"
)

# plot_delta(dataset, variable, transition, delta_sec, boundaries_df,
#            zone, mode, phase_colors, condition_order, condition_colors, theme)
p_delta <- plot_delta(
  dataset       = dataset,
  variable      = "totaldist",
  transition    = "dark_1-light_2",
  delta_sec     = 300,
  boundaries_df = bounds
)

# Themes usable with any ggplot2 figure (+ syntax)
p_periods + light_theme()
p_periods + dark_theme()

# convert_time(x, from, to)  — unit conversion helper
convert_time(600, from = "seconds", to = "minutes")  # → 10


# ── EXPORT ────────────────────────────────────────────────────────────
# export_results(result, path, format)
#   format: "xlsx" (default, one sheet per plate) | "csv" (one file per plate)
export_results(result, path = "output/")
export_results(result, path = "output/csv/", format = "csv")

# export_figures(figure_list, path, format, width, height, dpi, ...)
export_figures(
  figure_list = list(
    periods    = p_periods,
    cumulative = p_cumul,
    lineplot   = p_line,
    delta      = p_delta
  ),
  path   = "output/figures/",
  format = "png",     # "png" | "pdf" | "svg" | "jpg" | "tiff"
  width  = 10,
  height = 7,
  dpi    = 300
)


# ── LAUNCH SHINY APP ──────────────────────────────────────────────────
# zebRabox::run_app()
