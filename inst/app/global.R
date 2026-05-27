# ======================================================================
# global.R
# Global setup and utilities (runs once per R session)
# ======================================================================

# ----------------------------------------------------------------------
# Global Utility Functions
# ----------------------------------------------------------------------

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


# Notification helper
notify <- function(msg, type = c("message","warning","error"), duration = 6) {
  type <- match.arg(type)
  shiny::showNotification(msg, type = type, duration = duration)
}

# ======================================================================
# Mode configuration factories — get_processing_config() and
# get_visualization_config() are exported from the zebRabox package
# (R/config.R) and available here because the package is loaded.
# ======================================================================

# ----------------------------------------------------------------------
# Options
# ----------------------------------------------------------------------
# Allow large Excel uploads (500 MB)
options(shiny.maxRequestSize = 500 * 1024^2)

# ----------------------------------------------------------------------
# Remove stale theme-helper copies from .GlobalEnv.
# global.R is sourced by Shiny into .GlobalEnv, so any leftover light_theme /
# dark_theme / void_theme from a previous source() call or saved .RData would
# shadow the package versions. Removing them here lets normal search-path
# lookup find the correct zebRabox:: versions without polluting .GlobalEnv.
# ----------------------------------------------------------------------
invisible(suppressWarnings(rm(list = c("light_theme", "dark_theme", "void_theme"))))

# ----------------------------------------------------------------------
# Auto-load All Modules
# ----------------------------------------------------------------------
module_files <- sort(list.files("modules", pattern = "\\.R$", full.names = TRUE, recursive = TRUE))
module_env <- environment()  # env de global.R (persistant)
invisible(lapply(module_files, function(f) source(f, local = module_env)))
