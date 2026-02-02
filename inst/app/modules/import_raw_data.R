# ======================================================================
# Raw Data Module
# Allows users to upload raw Zebrabox data, select modes, and preview.
# ======================================================================

# ----------------------------------------------------------------------
# UI
# ----------------------------------------------------------------------
raw_data_ui <- function(
    id,
    primary_choices   = c("Tracking Mode", "Quantization Mode"),
    secondary_choices = c("Light Dark Mode", "Vibration Mode")
) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shinyjs::useShinyjs(),
    
    # ---------------- Inputs ----------------
    shiny::fluidRow(
      shinydashboard::box(
        title = "Raw Data Inputs",
        width = 12,
        shiny::wellPanel(
          shiny::tagAppendAttributes(
            shiny::fileInput(
              ns("raw_data_files"), "Upload Raw Data Files (Excel)",
              multiple = TRUE, accept = c(".csv", ".xlsx")
            ),
            `aria-label` = "Upload CSV or Excel files"
          ),
          shiny::actionButton(ns("load_raw_data"), "Load Raw Data"),
          shiny::selectInput(
            ns("primary_mode"), "Primary Mode",
            choices = c("Select a mode" = "", primary_choices)
          ),
          shiny::selectInput(
            ns("secondary_mode"), "Secondary Mode",
            choices = c("Select a mode" = "", secondary_choices)
          )
        )
      )
    ),
    
    # ---------------- Preview ----------------
    shiny::fluidRow(
      shinydashboard::box(
        title = "Raw Data Preview",
        width = 12,
        shiny::uiOutput(ns("raw_data_tabs"))
      )
    )
  )
}

# ----------------------------------------------------------------------
# Server
# ----------------------------------------------------------------------
raw_data_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # ---------------- Enable/Disable Load Button ----------------
    shiny::observe({
      shinyjs::toggleState("load_raw_data", !is.null(input$raw_data_files))
    })
    
    # ---------------- Load Raw Data ----------------
    shiny::observeEvent(input$load_raw_data, {
      tryCatch({
        shiny::req(input$raw_data_files)
        rv$raw_data_list <- Map(read_file, input$raw_data_files$datapath, input$raw_data_files$name)
        notify("Raw data loaded successfully.", type = "message")
      }, error = function(e) {
        notify(conditionMessage(e), type = "error", duration = NULL)
      })
    })
    
    # ---------------- Robust modes updater (module-level) ----------------
    # local safe %||% in case not defined globally in this module
    `%||%` <- function(a, b) if (is.null(a)) b else a
    
    observe({
      # read raw inputs (namespaced already inside module server)
      p_in <- input$primary_mode
      s_in <- input$secondary_mode
      
      # normalize: treat "" and whitespace as missing
      normalize_sel <- function(x) {
        if (is.null(x)) return(NULL)
        x_chr <- as.character(x)
        if (!nzchar(trimws(x_chr))) return(NULL)
        x_chr
      }
      p <- normalize_sel(p_in)
      s <- normalize_sel(s_in)
      
      # current stored values (NULL if unset)
      prev_p <- rv$primary_mode %||% NULL
      prev_s <- rv$secondary_mode %||% NULL
      
      # only update rv when something actually changed
      if (!identical(p, prev_p) || !identical(s, prev_s)) {
        rv$primary_mode   <- p
        rv$secondary_mode <- s
        
        # notify only when both selections are valid (non-NULL).
        # prevents notification when reset clears fields.
        if (!is.null(p) && !is.null(s)) {
          notify("Modes updated successfully.", type = "message")
        }
      }
    })
    
    
    # ---------------- Common DataTable Options ----------------
    dt_options <- list(
      pageLength   = 25,
      autoWidth    = TRUE,
      orderClasses = TRUE,
      scrollX      = TRUE
    )
    
    # ---------------- Render Preview Tabs ----------------
    output$raw_data_tabs <- shiny::renderUI({
      shiny::req(rv$raw_data_list)
      if (length(rv$raw_data_list) == 0) {
        notify("No raw data loaded yet. Please upload and load raw data files.", type = "warning")
        return(NULL)
      }
      
      tabs <- lapply(seq_along(rv$raw_data_list), function(i) {
        file_name <- attr(rv$raw_data_list[[i]], "file_name")
        shiny::tabPanel(
          title = file_name,
          DT::dataTableOutput(session$ns(paste0("raw_data_table_", i)))
        )
      })
      do.call(shiny::tabsetPanel, c(tabs, list(id = session$ns("raw_data_tabset"))))
    })
    
    # ---------------- Render DataTables ----------------
    shiny::observe({
      shiny::req(!is.null(rv$raw_data_list))
      lapply(seq_along(rv$raw_data_list), function(i) {
        output[[paste0("raw_data_table_", i)]] <- DT::renderDataTable({
          # ---- Guard against reset/empty state ----
          if (is.null(rv$raw_data_list) || length(rv$raw_data_list) < i) return(NULL)
          DT::datatable(rv$raw_data_list[[i]], filter = "top", options = dt_options)
        })
      })
    })
  })
}
