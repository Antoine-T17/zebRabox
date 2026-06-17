# ======================================================================
# modules/import_data.R
# Data Importation Module
# Two side-by-side input boxes:
#   - "Formatted Data": upload .xlsx/.xls/.csv (1 file = 1 plate)
#   - "Raw Data":       upload .zip archives (1 zip = 1 plate); each archive
#                       may contain .xlsx/.xls/.csv OR .txt (Wintrack export).
#                       All archives must use the same file type — mixing is
#                       not allowed.
# Sets rv$raw_xlsx_list, rv$raw_zip_list, rv$raw_data_list,
#      rv$primary_mode, rv$secondary_mode.
# ======================================================================

# ----------------------------------------------------------------------
# Helper: inspect a zip archive without extracting it.
# Returns one of:
#   "tabular" — all files are .xlsx / .xls / .csv
#   "txt"     — all files are .txt  (Wintrack trajectory export)
#   "mixed"   — multiple types present (not allowed)
#   NA        — empty or unreadable archive
# ----------------------------------------------------------------------
detect_zip_content_type <- function(zippath) {
  entries <- tryCatch(unzip(zippath, list = TRUE)$Name, error = function(e) character(0))
  entries <- entries[!grepl("^__MACOSX/|\\/\\.", entries)]  # strip macOS artifacts
  entries <- entries[nzchar(tools::file_ext(entries))]      # keep files only (not dirs)
  exts    <- tolower(tools::file_ext(entries))
  if (length(exts) == 0L)                      return(NA_character_)
  if (all(exts == "txt"))                      return("txt")
  if (all(exts %in% c("xlsx", "xls", "csv"))) return("tabular")
  return("mixed")
}

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

      # ---- Formatted Data (xlsx / xls / csv) ----
      shinydashboard::box(
        title = "Formatted Data Inputs (Excel / CSV)",
        width = 6,
        shiny::wellPanel(
          shiny::tags$div(
            style = "margin-bottom: 3px;",
            shiny::tagAppendAttributes(
              shiny::fileInput(
                ns("raw_xlsx_files"),
                "Upload Formatted Data Files (.xlsx, .xls, .csv) — 1 file = 1 plate",
                multiple = TRUE,
                accept   = c(".xlsx", ".xls", ".csv")
              ),
              `aria-label` = "Upload Excel or CSV files"
            )
          ),
          shiny::tags$div(
            style = "margin-top: 0px; margin-bottom: 18px;",
            shiny::actionButton(ns("load_raw_xlsx"), "Load Formatted Data")
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

      # ---- Raw Data (zip archives — tabular or Wintrack .txt) ----
      shinydashboard::box(
        title = "Raw Data Inputs (ZIP)",
        width = 6,
        shiny::wellPanel(
          shiny::tags$div(
            style = "margin-bottom: 3px;",
            shiny::tagAppendAttributes(
              shiny::fileInput(
                ns("raw_zip_files"),
                shiny::tagList(
                  "Upload Plate Archives (.zip) — 1 zip = 1 plate",
                  shiny::tags$div(
                    style = "color: #777; font-size: 0.9em; margin-top: 3px;",
                    "Each archive may contain .xlsx / .xls / .csv or .txt files.",
                    shiny::tags$br(),
                    "If .txt files are present, they must come from a ",
                    shiny::tags$strong("Wintrack export"),
                    " (one .txt per well).",
                    shiny::tags$br(),
                    shiny::tags$em("All archives must use the same file type.")
                  )
                ),
                multiple = TRUE,
                accept   = c(".zip")
              ),
              `aria-label` = "Upload plate archive .zip files"
            )
          ),
          shiny::tags$div(
            style = "margin-top: 0px; margin-bottom: 0px;",
            shiny::actionButton(ns("load_raw_zip"), "Load ZIP Data")
          ),
          shiny::uiOutput(ns("wintrack_confirm_ui"))
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

    # Files waiting for Wintrack confirmation before being loaded
    pending_zip_files <- shiny::reactiveVal(NULL)
    zip_content_type  <- shiny::reactiveVal(NULL)

    # ---------------- Enable/Disable Load Buttons ----------------
    shiny::observe({
      shinyjs::toggleState("load_raw_xlsx", !is.null(input$raw_xlsx_files))
      shinyjs::toggleState("load_raw_zip",  !is.null(input$raw_zip_files))
    })

    # ---------------- Load Formatted Data (xlsx / xls / csv) ----------------
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

        notify("Formatted data loaded successfully.", type = "message")
      }, error = function(e) {
        notify(conditionMessage(e), type = "error", duration = NULL)
      })
    })

    # Internal helper: read and store zip files once all checks have passed
    do_load_zip <- function(files) {
      shiny::withProgress(message = "Loading .zip archives...", value = 0, {
        n    <- nrow(files)
        step <- if (n > 0) 1 / n else 1

        rv$raw_zip_list <- lapply(seq_len(n), function(i) {
          shiny::incProgress(step, detail = files$name[i])
          read_raw_file(files$datapath[i], files$name[i])
        })
      })
      notify("ZIP data loaded successfully.", type = "message")
      pending_zip_files(NULL)
      zip_content_type(NULL)
    }

    # ---------------- Load ZIP Data (inspection + guards) ----------------
    shiny::observeEvent(input$load_raw_zip, {
      tryCatch({
        shiny::req(input$raw_zip_files)

        files <- input$raw_zip_files
        ord   <- order(tolower(files$name))
        files <- files[ord, , drop = FALSE]

        # Inspect content type of every archive before loading anything
        types <- vapply(files$datapath, detect_zip_content_type, character(1))

        # Guard 1: mixed content within a single archive
        if (any(!is.na(types) & types == "mixed")) {
          bad <- files$name[!is.na(types) & types == "mixed"]
          notify(
            paste0(
              "Mixed file types detected inside: ", paste(bad, collapse = ", "), ". ",
              "Each archive must contain only one file type (.xlsx/.xls/.csv or .txt)."
            ),
            type = "error", duration = NULL
          )
          return()
        }

        # Guard 2: incompatible types across archives (e.g. one xlsx zip + one txt zip)
        unique_types <- unique(types[!is.na(types)])
        if (length(unique_types) > 1L) {
          notify(
            paste0(
              "Incompatible ZIP contents: some archives contain .txt files (Wintrack export) ",
              "while others contain tabular files (.xlsx/.xls/.csv). ",
              "All archives must use the same file format."
            ),
            type = "error", duration = NULL
          )
          return()
        }

        content_type <- if (length(unique_types) == 1L) unique_types else NA_character_
        pending_zip_files(files)
        zip_content_type(content_type)

        if (identical(content_type, "tabular")) {
          # Tabular archives need no extra confirmation — load immediately
          do_load_zip(files)
        }
        # .txt archives: hold and wait for the user's Wintrack confirmation

      }, error = function(e) {
        notify(conditionMessage(e), type = "error", duration = NULL)
      })
    })

    # ---------------- Wintrack Confirmation UI ----------------
    output$wintrack_confirm_ui <- shiny::renderUI({
      if (!identical(zip_content_type(), "txt") || is.null(pending_zip_files())) return(NULL)

      shiny::tags$div(
        style = paste0(
          "margin-top: 12px; padding: 10px; ",
          "background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 5px;"
        ),
        shiny::tags$strong(
          shiny::icon("triangle-exclamation"), " Wintrack .txt files detected"
        ),
        shiny::tags$p(
          "The ZIP archives contain .txt files. ",
          "These must be trajectory files exported from ",
          shiny::tags$strong("Wintrack"),
          " (one .txt file per well). ",
          "Please confirm before loading.",
          style = "margin: 6px 0 10px 0; font-size: 0.92em;"
        ),
        shiny::actionButton(
          session$ns("confirm_wintrack"),
          "Confirm — these are Wintrack export files",
          icon  = shiny::icon("check"),
          style = "background-color: #ffc107; color: #212529; border-color: #d39e00;"
        )
      )
    })

    # ---------------- Wintrack Confirmation Handler ----------------
    shiny::observeEvent(input$confirm_wintrack, {
      files <- pending_zip_files()
      shiny::req(files)
      tryCatch({
        do_load_zip(files)
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
