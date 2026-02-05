# ======================================================================
# modules/import_raw_data.R
# Raw Data Module
# Allows users to upload raw Zebrabox data as:
#   - .xlsx (1 file = 1 plate)
#   - .zip  (1 zip  = 1 plate, containing many .txt where 1 txt = 1 well)
# Builds rv$raw_data_list as the canonical "one dataframe per plate" list.
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

    shiny::tags$style(shiny::HTML(paste0("
      /* Raw Data Preview: align DT tables to the left (avoid default centering) */
      #", ns("raw_preview_wrap"), " table.dataTable {
        margin-left: 0 !important;
        margin-right: auto !important;
      }
    "))),

    shiny::fluidRow(
      # ---- Excel inputs ----
      shinydashboard::box(
        title = "Raw Data Inputs (Excel)",
        width = 6,
        shiny::wellPanel(
          shiny::tags$div(
            style = "margin-bottom: 3px;",
            shiny::tagAppendAttributes(
              shiny::fileInput(
                ns("raw_xlsx_files"),
                "Upload Raw Data Files (.xlsx) — 1 file = 1 plate",
                multiple = TRUE,
                accept = c(".xlsx")
              ),
              `aria-label` = "Upload Excel .xlsx files"
            )
          ),
          shiny::tags$div(
            style = "margin-top: 0px; margin-bottom: 18px;",
            shiny::actionButton(ns("load_raw_xlsx"), "Load Excel Data")
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
      ),

      # ---- ZIP inputs (Wintrack TXT export grouped per plate) ----
      shinydashboard::box(
        title = "Raw Data Inputs (Wintrack TXT Export as ZIP)",
        width = 6,
        shiny::wellPanel(
          shiny::tags$div(
            style = "margin-bottom: 3px;",
            shiny::tagAppendAttributes(
              shiny::fileInput(
                ns("raw_zip_files"),
                "Upload Plate Archives (.zip) — 1 zip = 1 plate (contains many .txt)",
                multiple = TRUE,
                accept = c(".zip")
              ),
              `aria-label` = "Upload plate archive .zip files"
            )
          ),
          shiny::tags$div(
            style = "margin-top: 0px; margin-bottom: 0px;",
            shiny::actionButton(ns("load_raw_zip"), "Load ZIP Data")
          )
        )
      )
    ),

    shiny::fluidRow(
      shinydashboard::box(
        title = "Raw Data Preview",
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
raw_data_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    `%||%` <- function(a, b) if (is.null(a)) b else a

    dt_options <- list(
      pageLength   = 25,
      autoWidth    = TRUE,
      orderClasses = TRUE,
      scrollX      = TRUE
    )

    # ---------------- Enable/Disable Load Buttons ----------------
    shiny::observe({
      shinyjs::toggleState("load_raw_xlsx", !is.null(input$raw_xlsx_files))
      shinyjs::toggleState("load_raw_zip",  !is.null(input$raw_zip_files))
    })

    # ---------------- Load Excel Data ----------------
    shiny::observeEvent(input$load_raw_xlsx, {
      tryCatch({
        shiny::req(input$raw_xlsx_files)

        files <- input$raw_xlsx_files
        ord   <- order(tolower(files$name))              # <--- alphabetical, case-insensitive
        files <- files[ord, , drop = FALSE]

        shiny::withProgress(message = "Loading .xlsx files...", value = 0, {
          n <- nrow(files)
          step <- if (n > 0) 1 / n else 1

          rv$raw_xlsx_list <- lapply(seq_len(n), function(i) {
            shiny::incProgress(step, detail = files$name[i])
            read_file(files$datapath[i], files$name[i])
          })
        })

        notify("Excel data loaded successfully.", type = "message")
      }, error = function(e) {
        notify(conditionMessage(e), type = "error", duration = NULL)
      })
    })

    # ---------------- Load ZIP Data ----------------
    shiny::observeEvent(input$load_raw_zip, {
      tryCatch({
        shiny::req(input$raw_zip_files)

        files <- input$raw_zip_files
        ord   <- order(tolower(files$name))              # <--- alphabetical, case-insensitive
        files <- files[ord, , drop = FALSE]

        shiny::withProgress(message = "Loading .zip plates...", value = 0, {
          n <- nrow(files)
          step <- if (n > 0) 1 / n else 1

          rv$raw_zip_list <- lapply(seq_len(n), function(i) {
            shiny::incProgress(step, detail = files$name[i])
            read_file(files$datapath[i], files$name[i])
          })
        })

        notify("ZIP data loaded successfully.", type = "message")
      }, error = function(e) {
        notify(conditionMessage(e), type = "error", duration = NULL)
      })
    })

    # ---------------- Canonical raw_data_list for downstream processing ----------------
    shiny::observe({
      xlsx <- rv$raw_xlsx_list %||% list()
      zip  <- rv$raw_zip_list  %||% list()
      rv$raw_data_list <- c(xlsx, zip)
    })

    # ---------------- Robust modes updater (module-level) ----------------
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
# End of import_raw_data.R
# ======================================================================
