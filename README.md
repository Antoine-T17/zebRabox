<img src="inst/app/www/my_sticker_without_border.png" align="right" width="100" />

# ZebRabox : **From raw data to interactive visualization.**

## Introduction

The ZebraBox chamber is a complete system for high-throughput behavioral tests. Applications include pharmacology, toxicology, ecotoxicology, and behavioral genetics. The device holds 6-, 12-, 24-, 48- or 96-well plates and has a high-resolution camera which films in visible or infrared light. Tests run from minutes to days and each data point links to a fixed time window (integration period). Output files are `.raw` and `.xls`.

## Why This App?

Data in many files and formats leads to errors, duplicates, and manual fixes. This app centralizes everything in one place. You get accurate, up-to-date, and auditable results without code. <br>

**Simple : drag, drop, clean, merge, and analyze in a few clicks.**

## How to run the app (local)

Copy/paste the following lines into the R console in RStudio :

```r
install.packages("remotes")
remotes::install_github("Antoine-T17/zebRabox")

zebRabox::run_app()
```

### Load or Create a Plate Plan

ZebraBox uses standard multi-well plates. Edge wells can show border effects. The app can generate a random plan. Download it as `.xlsx` or view it as a table and figure. If you already have a plan, load it and wait to see *Upload complete* to click on the button. Column names must follow the required format. *What is the required format?* Download a [96-well sample plate plan](inputs/sample_plate_plan_plate_1.xlsx).

### Load Raw Data

Raw files must be in `.xlsx`. Convert your original `.xls` files before loading. When loading, wait to see *Upload complete* to click on the button.

The number of raw files must match your plate plans.

### Choose Primary Mode

-   **Tracking mode**\
    Follows each subject’s path.\
    Yields distance moved, active time, and movement counts.\
    See details in the [ZebraBox guide](inst/app/inputs/zebrabox_guidelines.pdf).

-   **Quantization mode**\
    Counts pixel color changes.\
    Yields high, medium, or low activity levels.\
    See details in the [ZebraBox guide](inst/app/inputs/zebrabox_guidelines.pdf).

### Choose Secondary Mode

-   **Light/Dark (LDM)**\
    For experiments with light shifts (0–100% or 100–0%).\
    Works even if no change occurred.

-   **Vibration/Rest (VM)**\
    For experiments with vibration frequencies (Hz).

## Processing Data

### Match Raw Data to Plate Plans

Confirm that each raw data file is correctly paired with its corresponding plate plan. Use the dropdown menu to select the appropriate plate plan.

### Additional Parameters

Before running full processing, upload your period transition file and removal specification file. Both must be `.xlsx` files with column names in the required format. *What is the required format?* Download a [sample period transition file](inputs/sample_period_transition.xlsx) and a [sample removal specification file](inputs/sample_removal_specification.xlsx).

### Run Full Processing

After you start processing, follow each step in the *Console Output*. When it finishes, explore results in the *Processed Data* tab or download them for external analysis.

## Visualize Data

Choose a plot type and generate the dataset. Check the *Console Output* to confirm the generation completed successfully. The *Visualization* tab is designed for quick data exploration. For deeper customization, download the processed data or the generated plot datasets.

You can configure and export figures with:

-   **Theme:** light or dark\
-   **Zone selection:** restrict to specific zones (if applicable)\
-   **X-axis label order:** define a custom order\
-   **Label color:** set category/condition colors\
-   **Save options:** export each figure as PNG

### Boxplots

Visualize distributions across transition periods (e.g., Light/Dark or Vibration/Rest).

### Cumulative Boxplots

View overall distributions independent of transition periods.

### Delta Boxplots

Compare values **before**, **during**, and **after** a selected transition period to pinpoint responses (e.g., escape/avoidance) to stimuli.

### Line Plots

Track activity or movement patterns over time to reveal trends and temporal dynamics.
