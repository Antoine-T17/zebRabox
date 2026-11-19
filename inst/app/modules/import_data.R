# ======================================================================
# modules/import_data.R
# Data Importation Module
# Upload of raw data files (.xlsx/.xls/.csv, 1 file = 1 plate) and selection
# of the primary / secondary modes.
# Sets rv$raw_xlsx_list, rv$raw_data_list, rv$primary_mode, rv$secondary_mode.
# ======================================================================

# ----------------------------------------------------------------------
# UI
# ----------------------------------------------------------------------
data_importation_ui <- function(
    id,
    primary_choices   = c("Tracking Mode", "Quantization Mode"),
    secondary_choices = c("Light Dark Mode", "Vibration Mode")
) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),

    shiny::tags$style(shiny::HTML(paste0("
      /* Data Preview: align DT tables to the left */
      #", ns("raw_preview_wrap"), " table.dataTable {
        margin-left: 0 !important;
        margin-right: auto !important;
      }
    "))),

    shiny::fluidRow(

      # ---- Raw Data (xlsx / xls / csv) ----
      shinydashboard::box(
        title = "Raw Data Inputs (Excel / CSV)",
        width = 12,
        shiny::wellPanel(
          shiny::tags$div(
            style = "margin-bottom: 3px;",
            shiny::tagAppendAttributes(
              shiny::fileInput(
                ns("raw_xlsx_files"),
                "Upload Raw Data Files (.xlsx, .xls, .csv) — 1 file = 1 plate",
                multiple = TRUE,
                accept   = c(".xlsx", ".xls", ".csv")
              ),
              `aria-label` = "Upload Excel or CSV files"
            )
          ),
          shiny::tags$div(
            style = "margin-top: 0px; margin-bottom: 18px;",
            shiny::actionButton(ns("load_raw_xlsx"), "Load Raw Data")
          ),

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

    shiny::fluidRow(
      shinydashboard::box(
        title = "Data Preview",
        width = 12,
        shiny::tags$div(
          id = ns("raw_preview_wrap"),
          shiny::uiOutput(ns("raw_data_tabs"))
        )
      )
    )
  )
}

# ----------------------------------------------------------------------
# Server
# ----------------------------------------------------------------------
data_importation_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    dt_options <- list(
      pageLength   = 25,
      autoWidth    = TRUE,
      orderClasses = TRUE,
      scrollX      = TRUE
    )

    # ---------------- Enable/Disable Load Buttons ----------------
    shiny::observe({
      shinyjs::toggleState("load_raw_xlsx", !is.null(input$raw_xlsx_files))
    })

    # ---------------- Load Raw Data (xlsx / xls / csv) ----------------
    shiny::observeEvent(input$load_raw_xlsx, {
      tryCatch({
        shiny::req(input$raw_xlsx_files)

        files <- input$raw_xlsx_files
        ord   <- order(tolower(files$name))
        files <- files[ord, , drop = FALSE]

        shiny::withProgress(message = "Loading files...", value = 0, {
          n    <- nrow(files)
          step <- if (n > 0) 1 / n else 1

          rv$raw_xlsx_list <- lapply(seq_len(n), function(i) {
            shiny::incProgress(step, detail = files$name[i])
            read_raw_file(files$datapath[i], files$name[i])
          })
        })

        notify("Raw data loaded successfully.", type = "message")
      }, error = function(e) {
        notify(conditionMessage(e), type = "error", duration = NULL)
      })
    })

    # ---------------- Canonical raw_data_list for downstream processing ----------------
    shiny::observe({
      rv$raw_data_list <- rv$raw_xlsx_list %||% list()
    })

    # ---------------- Robust modes updater ----------------
    shiny::observe({
      normalize_sel <- function(x) {
        if (is.null(x)) return(NULL)
        x <- as.character(x)
        if (!nzchar(trimws(x))) return(NULL)
        x
      }

      p <- normalize_sel(input$primary_mode)
      s <- normalize_sel(input$secondary_mode)

      prev_p <- rv$primary_mode %||% NULL
      prev_s <- rv$secondary_mode %||% NULL

      if (!identical(p, prev_p) || !identical(s, prev_s)) {
        rv$primary_mode   <- p
        rv$secondary_mode <- s
        if (!is.null(p) && !is.null(s)) notify("Modes updated successfully.", type = "message")
      }
    })

    # ---------------- Preview Tabs UI (3 tabs + dropdown) ----------------
    output$raw_data_tabs <- shiny::renderUI({
      all_files <- rv$raw_data_list %||% list()
      if (!length(all_files)) return(NULL)

      file_names <- vapply(
        all_files,
        function(x) attr(x, "file_name") %||% "unknown",
        character(1)
      )

      n_total <- length(all_files)
      n_tabs  <- min(3, n_total)

      tabs <- lapply(seq_len(n_tabs), function(i) {
        shiny::tabPanel(
          title = file_names[i],
          DT::dataTableOutput(session$ns(paste0("raw_preview_table_", i)))
        )
      })

      if (n_total > 3) {
        more_choices <- stats::setNames(as.character(4:n_total), file_names[4:n_total])

        tabs <- c(tabs, list(
          shiny::tabPanel(
            title = "More",
            shiny::tagList(
              shiny::selectInput(
                session$ns("raw_more_file"),
                "Select a file",
                choices  = more_choices,
                selected = as.character(4)
              ),
              DT::dataTableOutput(session$ns("raw_preview_table_more"))
            )
          )
        ))
      }

      do.call(shiny::tabsetPanel, c(tabs, list(id = session$ns("raw_data_tabset"))))
    })

    # ---------------- Render Preview Tables (first 3) ----------------
    shiny::observe({
      all_files <- rv$raw_data_list %||% list()
      if (!length(all_files)) return(NULL)

      n_tabs <- min(3, length(all_files))

      for (i in seq_len(n_tabs)) {
        local({
          ii <- i
          output[[paste0("raw_preview_table_", ii)]] <- DT::renderDataTable({
            files2 <- rv$raw_data_list %||% list()
            if (length(files2) < ii) return(NULL)
            DT::datatable(files2[[ii]], filter = "top", options = dt_options)
          })
        })
      }
    })

    # ---------------- Render Preview Table (dropdown) ----------------
    output$raw_preview_table_more <- DT::renderDataTable({
      all_files <- rv$raw_data_list %||% list()
      if (length(all_files) <= 3) return(NULL)

      idx <- suppressWarnings(as.integer(input$raw_more_file))
      if (is.na(idx) || idx < 4 || idx > length(all_files)) return(NULL)

      DT::datatable(all_files[[idx]], filter = "top", options = dt_options)
    })
  })
}

# ======================================================================
# End of import_data.R
# ======================================================================
