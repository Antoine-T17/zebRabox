# ======================================================================
# server.R
# Server logic (runs once per user session)
# ======================================================================

server <- function(input, output, session) {
  
  # ------------------------------------------------------------------
  # Reactive Values (per-session global store)
  # ------------------------------------------------------------------
  rv <- shiny::reactiveValues(
    plate_plan_df_list                    = NULL,
    plate_plan_type                       = NULL, 
    raw_data_list                         = NULL,
    mapping                               = NULL,
    ordered_plate_plans                   = NULL,
    processing_results                    = NULL,
    # Visualisation
    plot                                  = NULL,
    all_zone_combined_light_dark_boxplots = NULL,
    all_zone_combined_cum_boxplots        = NULL,
    all_zone_combined_delta_boxplots      = NULL,
    all_zone_combined_lineplots           = NULL,
    # Modes
    primary_mode                          = NULL,
    secondary_mode                        = NULL
  )
  
  # Prevent multiple module instantiations if mode changes or inputs refresh
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
    rv$plate_plan_df_list                    <- NULL
    rv$plate_plan_type                       <- NULL
    rv$raw_data_list                         <- NULL
    rv$processing_results                    <- NULL
    rv$ordered_plate_plans                   <- NULL
    rv$mapping                               <- NULL
    rv$plot                                  <- NULL
    rv$primary_mode                          <- NULL
    rv$secondary_mode                        <- NULL
    rv$all_zone_combined_light_dark_boxplots <- NULL
    rv$all_zone_combined_cum_boxplots        <- NULL
    rv$all_zone_combined_delta_boxplots      <- NULL
    rv$all_zone_combined_lineplots           <- NULL
    
    shinyjs::reset("plate_plan-plate_plan_files")
    shinyjs::reset("raw_data-raw_data_files")
    shiny::updateSelectInput(session, "raw_data-primary_mode",   selected = "")
    shiny::updateSelectInput(session, "raw_data-secondary_mode", selected = "")
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
    
    notify("Application fully reset — please reload your plate plans, raw data and modes.", "message")
  })
}
