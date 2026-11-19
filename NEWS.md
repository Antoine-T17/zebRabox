# zebRabox 0.2.0

* First archived release (Zenodo).
* Raw data are imported as `.xlsx`, `.xls` or `.csv` files (one file per plate).
  The experimental ZIP / Wintrack `.txt` trajectory pipeline has been removed.
* New demo dataset (two 24-well plates, Light/Dark and Vibration/Rest sessions)
  available with `zebrabox_example()` and downloadable from the app welcome page.
* `inst/examples/template.R` now runs as-is on the demo dataset.
* All processing and visualization steps are available as standalone exported
  functions (`set_mode()`, `create_plate_plan()`, `read_raw_files()`,
  `run_processing()`, `prepare_dataset()`, `plot_*()`, `export_*()`).

# zebRabox 0.1.1

* Initial development version of the Shiny application.
