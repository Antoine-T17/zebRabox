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

# Null-coalesce operator — shared across all modules (removes need for local redefinitions)
`%||%` <- function(a, b) if (is.null(a)) b else a

# Theme helper — centralises the light/dark switch logic used in visualization_module
# Returns: obj (ggplot2 theme result), fn (theme function ref), edge_col, is_light
get_theme <- function(theme_choice) {
  is_light <- tolower(as.character(theme_choice)) == "light"
  list(
    obj      = if (is_light) light_theme() else dark_theme(),
    fn       = if (is_light) light_theme   else dark_theme,
    edge_col = if (is_light) "black"       else "white",
    is_light = is_light
  )
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
