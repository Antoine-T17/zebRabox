# ======================================================================
# global.R
# Global setup and utilities (runs once per R session)
# ======================================================================

# ------------------------------------------------------------------------------
# Required packages (checked early with clear error messages)
# ------------------------------------------------------------------------------

pkgs <- c(
  "shiny","shinyjs","shinydashboard","shinyWidgets","bslib","sass",
  "readxl","openxlsx","dplyr","ggplot2","ggforce","plotly","htmlwidgets",
  "DT","zip","scales","rhandsontable","shinyjqui","stringr","RColorBrewer",
  "writexl","purrr","tidyr","fmsb","ggiraph"
)

# Check which packages are missing (do not attach packages yet)
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

# If we are in a renv project and packages are missing, try restoring once.
# This helps on fresh machines where users run the app via runGitHub().
if (length(missing) > 0 && file.exists("renv.lock") && requireNamespace("renv", quietly = TRUE)) {
  message(
    "Missing packages detected: ", paste(missing, collapse = ", "),
    "\nAttempting to install required dependencies via renv::restore()..."
  )
  try(renv::restore(project = ".", prompt = FALSE), silent = TRUE)
  
  # Re-check after restore attempt
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
}

# If still missing, stop with a helpful message
if (length(missing) > 0) {
  stop(
    "Missing packages: ", paste(missing, collapse = ", "),
    "\nIf you are on Windows, make sure Rtools is installed.",
    "\nThen re-run: renv::restore(prompt = FALSE)"
  )
}

# Attach packages now that we know they're installed
invisible(lapply(pkgs, library, character.only = TRUE))


# ----------------------------------------------------------------------
# Global Utility Functions
# ----------------------------------------------------------------------

# Convert selected columns to numeric (with locale-safe decimal replacement)
convert_numeric_cols <- function(df, cols) {
  for (col in intersect(names(df), cols)) {
    df[[col]] <- as.numeric(gsub(",", ".", as.character(df[[col]])))
  }
  df
}

# UI placeholder shown until modes are defined
waiting_message_ui <- function() {
  shiny::fluidRow(
    shiny::column(
      width = 12, align = "center",
      shiny::h3(
        "Still waiting for primary and secondary modes...",
        style = "color: #2196F3; font-style: italic;"
      ),
      shiny::icon("coffee", class = "fa-3x", style = "margin-top: 1em;"),
      shiny::div(style = "margin-bottom: 20px;"),  # spacing
      shiny::p("Go fill them in the Raw Data Tab", style = "font-size: 1.2em;")
    )
  )
}

# File Reading Utility
read_file <- function(path, file_name) {
  df <- if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    utils::read.csv2(path, sep = ";", dec = ".")
  } else {
    readxl::read_excel(path)
  }
  attr(df, "file_name") <- file_name
  df
}

# Notification helper  
notify <- function(msg, type = c("message","warning","error"), duration = 6) {
  type <- match.arg(type)
  shiny::showNotification(msg, type = type, duration = duration)
}


# ======================================================================
# Processing mode configuration (used by the generic processing module)
# ======================================================================

processing_config <- list(
  tm_ldm = list(
    ui_title = "Tracking Mode, Light-Dark Mode",
    # maps "lightXX" -> "light", "darkYY" -> "dark", otherwise unchanged
    period_map = function(x) {
      dplyr::case_when(
        stringr::str_detect(x, "^light") ~ "light",
        stringr::str_detect(x, "^dark")  ~ "dark",
        TRUE ~ x
      )
    }
  ),
  tm_vm = list(
    ui_title = "Tracking Mode, Vibration-Rest Mode",
    # maps "vibrationXX" -> "vibration", "restYY" -> "rest", otherwise unchanged
    period_map = function(x) {
      dplyr::case_when(
        stringr::str_detect(x, "^vibration") ~ "vibration",
        stringr::str_detect(x, "^rest")      ~ "rest",
        TRUE ~ x
      )
    }
  )
)

# Small helper to fetch config safely
# global.R (append)

# Return a configuration list used by the generic processing module
get_processing_config <- function(mode) {
  ldm_period_map <- function(x) {
    dplyr::case_when(
      stringr::str_detect(x, "^light") ~ "light",
      stringr::str_detect(x, "^dark")  ~ "dark",
      TRUE ~ x
    )
  }
  
  vm_period_map <- function(x) {
    dplyr::case_when(
      stringr::str_detect(x, "^vibration") ~ "vibration",
      stringr::str_detect(x, "^rest")      ~ "rest",
      TRUE ~ x
    )
  }
  
  qm_filter_fn <- function(df) {
    if ("datatype" %in% colnames(df) && any(stringr::str_detect(df$datatype, "quantauc"))) {
      dplyr::filter(df, stringr::str_detect(datatype, "quantauc"))
    } else {
      df
    }
  }
  
  switch(mode,
         "tm_ldm" = list(
           ui_title        = "Tracking Mode, Light-Dark Mode",
           period_map      = ldm_period_map,
           period_keys     = c("light", "dark"),       # déjà là
           convert_cols    = c("inact","inadur","inadist","smlct","smldist","smldur","larct","lardur","lardist","emptyct","emptydur","period"),
           zone_num_cols   = c("inact","inadur","inadist","smlct","smldist","smldur","larct","lardur","lardist","emptyct","emptydur"),
           filter_fn       = NULL
         ),
         "tm_vm" = list(
           ui_title        = "Tracking Mode, Vibration-Rest Mode",
           period_map      = vm_period_map,
           period_keys     = c("vibration", "rest"),   # déjà là
           convert_cols    = c("inact","inadur","inadist","smlct","smldist","smldur","larct","lardur","lardist","emptyct","emptydur","period"),
           zone_num_cols   = c("inact","inadur","inadist","smlct","smldist","smldur","larct","lardur","lardist","emptyct","emptydur"),
           filter_fn       = NULL
         ),
         "qm_ldm" = list(
           ui_title        = "Quantization Mode, Light-Dark Mode",
           period_map      = ldm_period_map,
           period_keys     = c("light", "dark"),       # AJOUTÉ
           convert_cols    = c("frect","fredur","midct","middur","burct","burdur","zerct","zerdur","actinteg","period"),
           zone_num_cols   = c("frect","fredur","midct","middur","burct","burdur","zerct","zerdur","actinteg"),
           filter_fn       = qm_filter_fn
         ),
         "qm_vm" = list(
           ui_title        = "Quantization Mode, Vibration-Rest Mode",
           period_map      = vm_period_map,
           period_keys     = c("vibration", "rest"),   # AJOUTÉ
           convert_cols    = c("frect","fredur","midct","middur","burct","burdur","zerct","zerdur","actinteg","period"),
           zone_num_cols   = c("frect","fredur","midct","middur","burct","burdur","zerct","zerdur","actinteg"),
           filter_fn       = qm_filter_fn
         ),
         list(
           ui_title        = "Tracking Mode, Light-Dark Mode",
           period_map      = ldm_period_map,
           period_keys     = c("light", "dark"),
           convert_cols    = c("inact","inadur","inadist","smlct","smldist","smldur","larct","lardur","lardist","emptyct","emptydur","period"),
           zone_num_cols   = c("inact","inadur","inadist","smlct","smldist","smldur","larct","lardur","lardist","emptyct","emptydur"),
           filter_fn       = NULL
         )
  )
}

# ======================================================================
# Visualization config (which response vars & period labels per mode)
# ======================================================================

get_visualization_config <- function(mode) {
  # Expected metric sets
  tm_expected <- c(
    "totaldist","totaldur","totalct","totalspeed",
    "lardist","lardur","larct","larspeed",
    "smldist","smldur","smlct","smlspeed",
    "inadist","inadur","inact","emptydur","emptyct"
  )
  qm_expected <- c("frect","fredur","midct","middur","burct","burdur","zerct","zerdur","actinteg")
  
  # Label helpers
  ldm_period_keys   <- c("light","dark")
  ldm_period_labels <- c("Light period","Dark period")
  vm_period_keys    <- c("vibration","rest")
  vm_period_labels  <- c("Vibration period","Rest period")
  
  switch(mode,
         "tm_ldm" = list(
           ui_title             = "Tracking Mode, Light-Dark Mode",
           expected_vars        = tm_expected,
           period_keys          = ldm_period_keys,
           period_labels        = ldm_period_labels,
           period_ui_name       = "Light/Dark",
           period_default_colors = "#FFC300,#B3AAAA"
         ),
         "tm_vm" = list(
           ui_title             = "Tracking Mode, Vibration-Rest Mode",
           expected_vars        = tm_expected,
           period_keys          = vm_period_keys,
           period_labels        = vm_period_labels,
           period_ui_name       = "Vibration/Rest",
           period_default_colors = "#5E81AC,#A3BE8C"
         ),
         "qm_ldm" = list(
           ui_title             = "Quantization Mode, Light-Dark Mode",
           expected_vars        = qm_expected,
           period_keys          = ldm_period_keys,
           period_labels        = ldm_period_labels,
           period_ui_name       = "Light/Dark",
           period_default_colors = "#FFC300,#B3AAAA"
         ),
         "qm_vm" = list(
           ui_title             = "Quantization Mode, Vibration-Rest Mode",
           expected_vars        = qm_expected,
           period_keys          = vm_period_keys,
           period_labels        = vm_period_labels,
           period_ui_name       = "Vibration/Rest",
           period_default_colors = "#5E81AC,#A3BE8C"
         ),
         # fallback
         list(
           ui_title             = "Tracking Mode, Light-Dark Mode",
           expected_vars        = tm_expected,
           period_keys          = ldm_period_keys,
           period_labels        = ldm_period_labels,
           period_ui_name       = "Light/Dark",
           period_default_colors = "#FFC300,#B3AAAA"
         )
  )
}

# ----------------------------------------------------------------------
# Time conversion utility (used by visualization module)
# ----------------------------------------------------------------------
convert_time <- function(x, from, to) {
  if (from == to) return(x)
  f <- c(seconds = 1, minutes = 60, hours = 3600, days = 86400)
  x * f[[from]] / f[[to]]
}

# ----------------------------------------------------------------------
# Color utility: ensure exactly n colors (repeat/truncate/generate)
# ----------------------------------------------------------------------
ensure_colors <- function(n, cols = character(0)) {
  cols <- cols[!is.na(cols) & nzchar(trimws(cols))]
  if (length(cols) == 0) {
    base <- tryCatch(RColorBrewer::brewer.pal(min(8, max(3, n)), "Set1"),
                     error = function(e) grDevices::rainbow(min(8, max(3, n))))
    return(grDevices::colorRampPalette(base)(n))
  }
  cols <- trimws(cols)
  if (length(cols) < n) {
    cols <- rep(cols, length.out = n)
  } else if (length(cols) > n) {
    cols <- cols[seq_len(n)]
  }
  cols
}


# ----------------------------------------------------------------------
# Options
# ----------------------------------------------------------------------
# Allow large Excel uploads (500 MB)
options(shiny.maxRequestSize = 500 * 1024^2)

# ----------------------------------------------------------------------
# Auto-load All Modules
# ----------------------------------------------------------------------
module_files <- sort(list.files("modules", pattern = "\\.R$", full.names = TRUE, recursive = TRUE))
module_env <- environment()  # env de global.R (persistant)
invisible(lapply(module_files, function(f) source(f, local = module_env)))
