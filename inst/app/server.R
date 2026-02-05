# ======================================================================
# server.R
# Server logic (runs once per user session)
# ======================================================================

server <- function(input, output, session) {

  # ------------------------------------------------------------------
  # Reactive Values (per-session global store)
  # ------------------------------------------------------------------
  rv <- shiny::reactiveValues(
    # Plate plans
    plate_plan_df_list  = NULL,
    plate_plan_type     = NULL,

    # Raw data (canonical + sources)
    raw_data_list       = NULL,  # <- ALWAYS: one dataframe per plate (xlsx + zip)
    raw_xlsx_list       = NULL,  # <- xlsx plates
    raw_zip_list        = NULL,  # <- zip plates (each zip -> many txt -> one plate df)

    # Mapping / processing
    mapping             = NULL,
    ordered_plate_plans = NULL,
    processing_results  = NULL,

    # Visualisation objects
    plot                                  = NULL,
    all_zone_combined_light_dark_boxplots = NULL,
    all_zone_combined_cum_boxplots        = NULL,
    all_zone_combined_delta_boxplots      = NULL,
    all_zone_combined_lineplots           = NULL,

    # Modes
    primary_mode        = NULL,
    secondary_mode      = NULL,

    # Optional: keep track of temp dirs created by unzip to allow cleanup on reset
    tmp_unzip_dirs      = character()
  )

  # ------------------------------------------------------------------
  # Prevent multiple module instantiations
  # ------------------------------------------------------------------
  processing_started    <- shiny::reactiveVal(FALSE)
  visualization_started <- shiny::reactiveVal(FALSE)

  # ------------------------------------------------------------------
  # Always-Loaded Modules (created once per session)
  # ------------------------------------------------------------------
  plate_plan_server("plate_plan", rv)
  raw_data_server("raw_data", rv)

  # ------------------------------------------------------------------
  # Helper Reactive: Build Mode Identifier
  # ------------------------------------------------------------------
  get_mode <- shiny::reactive({
    is_missing <- function(x) is.null(x) || (is.character(x) && !nzchar(trimws(x)))
    if (is_missing(rv$primary_mode) || is_missing(rv$secondary_mode)) return(NULL)

    primary   <- ifelse(rv$primary_mode   == "Tracking Mode", "tm", "qm")
    secondary <- ifelse(rv$secondary_mode == "Light Dark Mode", "ldm", "vm")
    paste(primary, secondary, sep = "_")
  })

  # ------------------------------------------------------------------
  # Dynamic UI: Processing
  # ------------------------------------------------------------------
  output$processing_ui <- shiny::renderUI({
    tryCatch({
      if (is.null(rv$primary_mode) || is.null(rv$secondary_mode)) {
        return(waiting_message_ui())
      }
      mode <- get_mode(); shiny::req(mode)
      cfg  <- get_processing_config(mode)
      processing_module_ui("processing", cfg)
    }, error = function(e) {
      shiny::wellPanel(shiny::h4("Processing UI error"), shiny::pre(e$message))
    })
  })

  # ------------------------------------------------------------------
  # Dynamic UI: Visualization
  # ------------------------------------------------------------------
  output$visualization_ui <- shiny::renderUI({
    tryCatch({
      if (is.null(rv$primary_mode) || is.null(rv$secondary_mode)) {
        return(waiting_message_ui())
      }
      mode <- get_mode(); shiny::req(mode)
      cfg  <- get_visualization_config(mode)
      visualization_module_ui("visualization", cfg)
    }, error = function(e) {
      shiny::wellPanel(shiny::h4("Visualization UI error"), shiny::pre(e$message))
    })
  })

  # ------------------------------------------------------------------
  # Lazy-create module servers AFTER a valid mode exists (once)
  # This prevents reading reactives outside a reactive context.
  # ------------------------------------------------------------------
  shiny::observeEvent(get_mode(), {
    mode <- get_mode(); shiny::req(mode)

    if (!isTRUE(processing_started())) {
      processing_module_server(
        "processing",
        rv,
        config = function() {
          m <- get_mode()
          if (is.null(m)) return(NULL)
          get_processing_config(m)
        }
      )
      processing_started(TRUE)
    }

    if (!isTRUE(visualization_started())) {
      visualization_module_server(
        "visualization",
        rv,
        config = function() {
          m <- get_mode()
          if (is.null(m)) return(NULL)
          get_visualization_config(m)
        }
      )
      visualization_started(TRUE)
    }
  }, ignoreInit = TRUE)

  # ------------------------------------------------------------------
  # Exit Button
  # ------------------------------------------------------------------
  shiny::observeEvent(input$exit_app, {
    shiny::stopApp()
  })

  # ------------------------------------------------------------------
  # Reset Button (Full Reset of data/state). We do NOT recreate modules.
  # ------------------------------------------------------------------
  shiny::observeEvent(input$reset_app, {

    # ---- Optional: cleanup temp unzip dirs (if you tracked them) ----
    if (length(rv$tmp_unzip_dirs %||% character()) > 0) {
      for (p in rv$tmp_unzip_dirs) {
        if (is.character(p) && nzchar(p) && dir.exists(p)) {
          try(unlink(p, recursive = TRUE, force = TRUE), silent = TRUE)
        }
      }
    }

    # ---- Clear RV (data/state) ----
    rv$plate_plan_df_list                    <- NULL
    rv$plate_plan_type                       <- NULL

    rv$raw_data_list                         <- NULL
    rv$raw_xlsx_list                         <- NULL
    rv$raw_zip_list                          <- NULL
    rv$tmp_unzip_dirs                        <- character()

    rv$mapping                               <- NULL
    rv$ordered_plate_plans                   <- NULL
    rv$processing_results                    <- NULL

    rv$plot                                  <- NULL
    rv$all_zone_combined_light_dark_boxplots <- NULL
    rv$all_zone_combined_cum_boxplots        <- NULL
    rv$all_zone_combined_delta_boxplots      <- NULL
    rv$all_zone_combined_lineplots           <- NULL

    rv$primary_mode                          <- NULL
    rv$secondary_mode                        <- NULL

    # ---- Reset fileInputs (Plate plan + Raw data + Processing uploads) ----
    # Plate plan module fileInput
    shinyjs::reset("plate_plan-plate_plan_files")

    # Raw data module fileInputs (xlsx + zip)
    shinyjs::reset("raw_data-raw_xlsx_files")
    shinyjs::reset("raw_data-raw_zip_files")

    # Processing module fileInputs (if module has been created, reset is still safe)
    shinyjs::reset("processing-period_file")
    shinyjs::reset("processing-removal_file")

    # ---- Reset selects / numeric / text in Plate plan module ----
    shiny::updateSelectInput(session, "plate_plan-create_plate_plan", selected = "")
    shiny::updateSelectInput(session, "plate_plan-plate_type",        selected = "")
    shiny::updateSelectInput(session, "plate_plan-keep_border_wells", selected = "")

    shiny::updateNumericInput(session, "plate_plan-plate_number",        value = 1)
    shiny::updateNumericInput(session, "plate_plan-conditions_number",   value = 1)
    shiny::updateNumericInput(session, "plate_plan-replicates_number",   value = 1)
    shiny::updateNumericInput(session, "plate_plan-units_per_replicate", value = 1)
    shiny::updateNumericInput(session, "plate_plan-seed_value",          value = 42)

    shiny::updateTextInput(session, "plate_plan-conditions_name",      value = "")
    shiny::updateTextInput(session, "plate_plan-plate_plan_name_xlsx", value = "plate_plan")

    # ---- Reset modes (selectInputs live in raw_data module UI) ----
    shiny::updateSelectInput(session, "raw_data-primary_mode",   selected = "")
    shiny::updateSelectInput(session, "raw_data-secondary_mode", selected = "")

    notify("Application fully reset — please reload your plate plans, raw data and modes.", "message")
  })
}

# ======================================================================
# End of server.R
# ======================================================================
