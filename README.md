<img src="inst/app/www/my_sticker_without_border.png" align="right" width="100" />

# ZebRabox : **From raw data to interactive visualization.**

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)
[![R >= 4.1](https://img.shields.io/badge/R-%E2%89%A5%204.1-276DC3.svg)](https://www.r-project.org/)
<!-- badges: end -->

zebRabox is an R package and Shiny application that turns the raw exports of the ViewPoint **ZebraBox** into clean, merged, analysis-ready datasets and publication-quality figures. It can be used **without writing code** (Shiny app) or **from R scripts** (standalone functions) for fully reproducible pipelines.

## Introduction

The ZebraBox chamber is a complete system for high-throughput behavioral tests. Applications include pharmacology, toxicology, ecotoxicology, and behavioral genetics. The device holds 6-, 12-, 24-, 48- or 96-well plates and has a high-resolution camera which films in visible or infrared light. Tests run from minutes to days and each data point links to a fixed time window (integration period). Output files are `.raw` and `.xls`.

## Why This App?

Data in many files and formats leads to errors, duplicates, and manual fixes. This app centralizes everything in one place. You get accurate, up-to-date, and auditable results without code.

**Simple : drag, drop, clean, merge, and analyze in a few clicks.**

Main features:

- randomized **plate plan** generation (with optional exclusion of border wells) or import of existing plans;
- import of raw ZebraBox data for **Tracking** or **Quantization** mode, in **Light/Dark** or **Vibration/Rest** protocols;
- **processing**: condition mapping, period assignment from a transition file, removal of time codes, wells, periods or conditions, and zone computation;
- **visualization**: period, cumulative and delta boxplots and line plots, interactive (hover/zoom) and exportable as PNG;
- **percentage tables** and **statistics** (normality and variance checks, then Student, Welch or paired t-tests / ANOVA, or Wilcoxon / Kruskal-Wallis, with Tukey, Dunnett or Dunn post-hoc tests).

## Installation

```r
install.packages("remotes")
remotes::install_github("Antoine-T17/zebRabox", dependencies = TRUE, upgrade = "never")
```

## Quick start

### Shiny app

```r
zebRabox::run_app()
```

### Try it with the demo dataset

A small real dataset (two 24-well plates of *Daphnia*, Tracking mode) is shipped with the package, with one **Light/Dark** session and one **Vibration/Rest** session. Each session contains the raw data, the plate plans, a period transition file and a removal specification file.

- **In the app:** click *Download demo dataset (.zip)* on the welcome page, then load the plate plans, the raw data, select *Tracking Mode* and the matching secondary mode, and upload the transition and removal files in *Processing*.
- **In R:** `zebrabox_example("light_dark")` or `zebrabox_example("vibration")` returns the paths of the files.

### Standalone R workflow

Every step of the app is available as an exported function:

```r
library(zebRabox)

demo <- zebrabox_example("light_dark")
cfg  <- set_mode("tracking", "light_dark")

plates     <- read_plate_plan_files(demo$plate_plans)
raw_data   <- read_raw_files(demo$raw)
period_df  <- as.data.frame(readxl::read_excel(demo$period_transition))
removal_df <- as.data.frame(readxl::read_excel(demo$removal))

result  <- run_processing(raw_data, plates, period_df, removal_df, cfg$processing)
dataset <- prepare_dataset(result, cfg$visualization)

plot_periods(dataset, variable = "totaldist", cfg = cfg$visualization)
export_results(result, path = "output/")
```

A complete, commented script covering all public functions is provided in [`inst/examples/template.R`](inst/examples/template.R).

## Using the app

### Load or Create a Plate Plan

ZebraBox uses standard multi-well plates. Edge wells can show border effects. The app can generate a random plan. Download it as `.xlsx` or view it as a table and figure. If you already have a plan, load it and wait to see *Upload complete* to click on the button. Column names must follow the required format. *What is the required format?* Download a [96-well sample plate plan](inst/app/inputs/sample_plate_plan_plate_1.xlsx).

### Load Raw Data

Raw files can be `.xlsx`, `.xls` or `.csv` (one file per plate). When loading, wait to see *Upload complete* to click on the button.

The number of raw files must match your plate plans.

### Choose Primary Mode

-   **Tracking mode**\
    Follows each subject’s path.\
    Yields distance moved, active time, and movement counts.\
    Refer to the guidelines provided by the supplier for details.

-   **Quantization mode**\
    Counts pixel color changes.\
    Yields high, medium, or low activity levels.\
    Refer to the guidelines provided by the supplier for details.

### Choose Secondary Mode

-   **Light/Dark (LDM)**\
    For experiments with light shifts (0–100% or 100–0%).\
    Works even if no change occurred.

-   **Vibration/Rest (VM)**\
    For experiments with vibration frequencies (Hz).

### Processing Data

#### Match Raw Data to Plate Plans

Confirm that each raw data file is correctly paired with its corresponding plate plan. Use the dropdown menu to select the appropriate plate plan.

#### Additional Parameters

Before running full processing, upload your period transition file and removal specification file. Both must be `.xlsx` files with column names in the required format. *What is the required format?* Download a [sample period transition file](inst/app/inputs/sample_period_transition.xlsx) and a [sample removal specification file](inst/app/inputs/sample_removal_specification.xlsx).

#### Run Full Processing

After you start processing, follow each step in the *Console Output*. When it finishes, explore results in the *Processed Data* tab or download them for external analysis.

### Visualize Data

Choose a plot type and generate the dataset. Check the *Console Output* to confirm the generation completed successfully. The *Visualization* tab is designed for quick data exploration. For deeper customization, download the processed data or the generated plot datasets.

You can configure and export figures with:

-   **Theme:** light or dark\
-   **Zone selection:** restrict to specific zones (if applicable)\
-   **X-axis label order:** define a custom order\
-   **Label color:** set category/condition colors\
-   **Save options:** export each figure as PNG

#### Boxplots

Visualize distributions across transition periods (e.g., Light/Dark or Vibration/Rest).

#### Cumulative Boxplots

View overall distributions independent of transition periods.

#### Delta Boxplots

Compare values **before**, **during**, and **after** a selected transition period to pinpoint responses (e.g., escape/avoidance) to stimuli.

#### Line Plots

Track activity or movement patterns over time to reveal trends and temporal dynamics.

#### Percentage Tables and Statistics

Compute percentage changes between two periods, and run the statistical tests suited to your design. The app checks normality and homogeneity of variances and selects parametric or non-parametric tests and the matching post-hoc comparisons, with diagnostic plots.

## Testing

```r
devtools::test()
```

## Citation

If you use zebRabox in your work, please cite it. Citation metadata is available in [`CITATION.cff`](CITATION.cff) (use *Cite this repository* on GitHub). A DOI is provided through Zenodo for each release.

## License

MIT © Antoine Tourret. See [LICENSE.md](LICENSE.md).
