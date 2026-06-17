# ======================================================================
# zebRabox — Standalone Workflow Reference
# ======================================================================
#
# Complete analysis workflow without the Shiny application.
# Sections marked with "── REQUIRES FILES ──" need real data files;
# replace the placeholder paths before running those sections.
#
# Steps covered:
#   1.  Load package
#   2.  Set analysis mode
#   3.  Create or import plate plan
#   4.  Import raw data
#   5.  Define period transitions
#   6.  Define removal specifications
#   7.  Run processing pipeline
#   8.  Prepare visualization dataset
#   9.  Generate figures
#  10.  Export results and figures
# ======================================================================

# ── 1 · Package ────────────────────────────────────────────────────────

library(zebRabox)


# ── 2 · Mode ───────────────────────────────────────────────────────────
# Choose primary mode ("tracking" or "quantization") and
# secondary protocol ("light_dark" or "vibration").

cfg <- set_mode("tracking", "light_dark")

# cfg$processing    → pass to run_processing()
# cfg$visualization → pass to prepare_dataset() and plot_*()
# cfg$mode_key      → internal key ("tm_ldm"), useful for debugging


# ── 3 · Plate plan ─────────────────────────────────────────────────────

# Option A — generate a randomised plan from scratch (no files needed).
# n_replicates × n_units × length(conditions) must equal plate capacity.
plates <- create_plate_plan(
  plate_type   = "96",
  conditions   = c("ctrl", "trt"),
  n_replicates = 4,
  n_units      = 6,
  n_plates     = 2
)

# Option B — import from Excel files (one file per plate).
# Required columns: animal (e.g. "A01_plate_1"), condition, plate_id.
# ── REQUIRES FILES ──────────────────────────────────────────────────────
if (FALSE) {
  plates <- read_plate_plan_files(
    paths = c("plan_plate1.xlsx", "plan_plate2.xlsx")
  )
}
# ────────────────────────────────────────────────────────────────────────


# ── 4 · Raw data ────────────────────────────────────────────────────────
# Files are sorted alphabetically (case-insensitive) before processing,
# matching the order of plate_plans.
# ── REQUIRES FILES ──────────────────────────────────────────────────────
if (FALSE) {
  raw_data <- read_raw_files(
    paths = c("plate1.xlsx", "plate2.xlsx")
  )

  # With optional ZIP trajectory archives (one per plate):
  raw_data <- read_raw_files(
    paths = c("plate1.xlsx", "plate2.xlsx")
  )
  zip_data <- read_raw_files(
    paths = c("plate1.zip", "plate2.zip")
  )
}
# ────────────────────────────────────────────────────────────────────────


# ── 5 · Period transitions ─────────────────────────────────────────────
# start      : time in seconds when each period begins.
# transition : "periodA-periodB" label (left = period ending,
#              right = period starting). Length must equal length(start).

period_df <- create_period_table(
  start      = c(0, 600, 1200),
  transition = c("light1-dark1", "dark1-light2", "light2-dark2")
)


# ── 6 · Removal specifications ─────────────────────────────────────────
# All removal arguments default to NA (= skip that step for that plate).
# Scalar specs are recycled across all plates.

removal_df <- create_removal_table(
  plate_id          = c(1, 2),
  remove_wells      = c("A01,B01", NA),  # remove two wells on plate 1 only
  remove_conditions = NA,                # keep all conditions
  remove_periods    = NA,                # keep all periods
  remove_time_codes = NA                 # keep all time points
)


# ── 7 · Processing ─────────────────────────────────────────────────────
# ── REQUIRES FILES ──────────────────────────────────────────────────────
if (FALSE) {
  result <- run_processing(
    raw_xlsx_list = raw_data,
    plate_plans   = plates,
    period_df     = period_df,
    removal_df    = removal_df,
    cfg           = cfg$processing
  )

  # With ZIP trajectories:
  result <- run_processing(
    raw_xlsx_list = raw_data,
    raw_zip_list  = zip_data,
    plate_plans   = plates,
    period_df     = period_df,
    removal_df    = removal_df,
    cfg           = cfg$processing
  )
}
# ────────────────────────────────────────────────────────────────────────


# ── 8 · Visualization dataset ──────────────────────────────────────────
# ── REQUIRES FILES ──────────────────────────────────────────────────────
if (FALSE) {
  # Combined all-plate data frame with TM-derived metrics (totaldist, etc.)
  dataset <- prepare_dataset(result, cfg$visualization)

  # Boundary data frame required for delta plots
  bounds <- get_boundaries(result)
}
# ────────────────────────────────────────────────────────────────────────


# ── 9 · Figures ────────────────────────────────────────────────────────
# All plot_*() functions return standard ggplot2 objects.
# Use + ggplot2::theme(...) to customise further.
# ── REQUIRES FILES ──────────────────────────────────────────────────────
if (FALSE) {
  # Boxplots per period — one facet per period (default), or pooled
  p_periods <- plot_periods(
    dataset  = dataset,
    variable = "inact",
    cfg      = cfg$visualization,
    mode     = "separated",   # or "pooled"
    theme    = "light"        # or "dark"
  )

  # Cumulative boxplots — total activity per animal over the whole experiment
  p_cumulative <- plot_cumulative(
    dataset  = dataset,
    variable = "inact"
  )

  # Lineplot — mean per-condition activity over time
  p_lineplot <- plot_lineplot(
    dataset     = dataset,
    variable    = "inact",
    bin_seconds = 60,
    time_unit   = "minutes",   # x-axis unit; "seconds", "minutes", "hours", "days"
    error_mode  = "error_bar"  # or "ci95"
  )

  # Delta — Before / Switch / After window around a transition
  p_delta <- plot_delta(
    dataset       = dataset,
    variable      = "inact",
    transition    = "light1-dark1",
    delta_sec     = 120,
    boundaries_df = bounds,
    mode          = "separated"  # or "pooled"
  )

  # Inspect any figure interactively
  print(p_periods)
  print(p_cumulative)
  print(p_lineplot)
  print(p_delta)
}
# ────────────────────────────────────────────────────────────────────────


# ── 10 · Export ────────────────────────────────────────────────────────
# ── REQUIRES FILES ──────────────────────────────────────────────────────
if (FALSE) {
  # Processed data — one Excel sheet per plate, or one CSV per plate
  export_results(result, path = "output/")
  export_results(result, path = "output/csv/", format = "csv")

  # Figures — one file per plot, named by list key
  export_figures(
    figure_list = list(
      periods    = p_periods,
      cumulative = p_cumulative,
      lineplot   = p_lineplot,
      delta      = p_delta
    ),
    path   = "output/figures/",
    format = "png",
    width  = 10,
    height = 7,
    dpi    = 300
  )
}
# ────────────────────────────────────────────────────────────────────────


# ── Optional · Launch the Shiny app ────────────────────────────────────

# zebRabox::run_app()
