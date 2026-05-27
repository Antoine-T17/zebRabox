# ======================================================================
# modules/processing_module.R
# Generic processing module (TM / QM, LDM / VM) driven by config
# ======================================================================

processing_module_ui <- function(id, config) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        title = paste0("Match Raw Data to Plate Plans (", config$ui_title, ")"),
        width = 12,
        shiny::h4("Associate Raw Data with Plate Plans"),
        shiny::uiOutput(ns("file_plate_selectors")),
        shiny::actionButton(ns("confirm_mapping"), "Confirm Mapping"),

        shiny::h4("Additional Processing Parameters"),
        shiny::fileInput(ns("period_file"),  "Upload Period Transitions File (Excel)", accept = ".xlsx"),
        shiny::fileInput(ns("removal_file"), "Upload Removal Specifications File (Excel)", accept = ".xlsx"),
        shiny::actionButton(ns("run_processing"), "Run Full Processing")
      )
    ),

    shiny::fluidRow(
      shinydashboard::box(
        title = "Processing Results",
        width = 12,
        shiny::div(
          style = "margin-bottom: 10px;",
          shiny::actionButton(ns("clear_console"), "Clear Console", icon = shiny::icon("trash"))
        ),
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Console Output",
            shiny::div(
              style = "height: 800px; overflow-y: auto; padding: 10px; border-radius: 6px;",
              class = "console-container",
              shiny::verbatimTextOutput(ns("console_output"), placeholder = TRUE)
            )
          ),
          shiny::tabPanel(
            "Processed Data",
            shiny::uiOutput(ns("tables_with_periods")),
            shiny::downloadButton(ns("download_all_results"), "Download all results (.zip)")
          ),
          shiny::tabPanel(
            "Boundary Associations",
            DT::dataTableOutput(ns("boundary_associations_table"))
          )
        )
      )
    )
  )
}

processing_module_server <- function(id, rv, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Helper: get concrete config (accepts function or list) ----
    get_cfg <- function() {
      if (is.function(config)) return(config())
      config
    }

    # ---- local helpers ----
    `%||%` <- function(a, b) if (is.null(a)) b else a

    # ---- console aggregator ----
    console_messages <- shiny::reactiveVal(character())
    add_console_message <- function(message) {
      console_messages(c(console_messages(), message))
    }
    shiny::observeEvent(input$clear_console, {
      console_messages(character())
      notify("Console cleared.", type = "message")
    })

    shiny::observeEvent(rv$app_reset_trigger, {
      console_messages(character())
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ======================================================================
    # 0) Import period/removal files WITH progress + store in rv
    # ======================================================================
    shiny::observeEvent(input$period_file, {
      shiny::req(input$period_file)

      tryCatch({
        shiny::withProgress(message = "Importing period transitions…", value = 0, {
          shiny::incProgress(0.25, detail = "Reading Excel…")
          period_df <- readxl::read_excel(input$period_file$datapath)

          shiny::incProgress(0.30, detail = "Validating required columns…")
          shiny::req(all(c("start", "transition") %in% names(period_df)))

          shiny::incProgress(0.25, detail = "Converting & sorting…")
          period_df$start <- as.numeric(period_df$start)
          period_df <- period_df[order(period_df$start), , drop = FALSE]

          shiny::incProgress(0.20, detail = "Storing in memory…")
          rv$period_df <- period_df

          # Invalidate results if inputs changed
          rv$processing_results <- NULL
        })

        add_console_message("✅ Period transitions imported & stored (rv$period_df).")
        notify("Period transitions imported.", type = "message")

      }, error = function(e) {
        rv$period_df <- NULL
        add_console_message(paste("❌ Period file error:", conditionMessage(e)))
        notify(conditionMessage(e), type = "error", duration = NULL)
      })
    })

    shiny::observeEvent(input$removal_file, {
      shiny::req(input$removal_file)

      tryCatch({
        shiny::withProgress(message = "Importing removal specifications…", value = 0, {
          shiny::incProgress(0.25, detail = "Reading Excel…")
          removal_df <- readxl::read_excel(input$removal_file$datapath)

          shiny::incProgress(0.25, detail = "Validating required columns…")
          shiny::req("plate_id" %in% names(removal_df))

          shiny::incProgress(0.30, detail = "Normalizing removal fields…")
          for (colname in c("remove_time_codes", "remove_wells", "remove_conditions", "remove_periods")) {
            if (!(colname %in% names(removal_df))) removal_df[[colname]] <- NA_character_
            removal_df[[colname]] <- as.character(removal_df[[colname]])
            removal_df[[colname]] <- ifelse(
              is.na(removal_df[[colname]]) | tolower(trimws(removal_df[[colname]])) %in% c("", "no"),
              NA_character_,
              removal_df[[colname]]
            )
          }

          shiny::incProgress(0.20, detail = "Parsing plate_id…")
          removal_df$plate_id <- suppressWarnings(as.numeric(gsub("Plate", "", removal_df$plate_id)))

          shiny::incProgress(0.00, detail = "Storing in memory…")
          rv$removal_df <- removal_df

          # Invalidate results if inputs changed
          rv$processing_results <- NULL
        })

        add_console_message("✅ Removal specifications imported & stored (rv$removal_df).")
        notify("Removal specifications imported.", type = "message")

      }, error = function(e) {
        rv$removal_df <- NULL
        add_console_message(paste("❌ Removal file error:", conditionMessage(e)))
        notify(conditionMessage(e), type = "error", duration = NULL)
      })
    })

    # ======================================================================
    # 1) UI selectors: map RAW XLSX -> Plate Plans (ZIP excluded)
    # ======================================================================
    output$file_plate_selectors <- shiny::renderUI({
      shiny::req(rv$raw_xlsx_list, rv$plate_plan_df_list)

      raw_list  <- rv$raw_xlsx_list
      plan_list <- rv$plate_plan_df_list

      if (length(raw_list) != length(plan_list)) {
        return(shiny::div(
          shiny::tags$b("❌ Error: "),
          sprintf(
            "Number of raw .xlsx files (%d) must match number of plate plans (%d).",
            length(raw_list), length(plan_list)
          )
        ))
      }

      file_names <- vapply(
        raw_list,
        function(x) as.character((attr(x, "file_name") %||% "")),
        FUN.VALUE = character(1)
      )

      plate_file_names <- vapply(
        plan_list,
        function(df) as.character((attr(df, "file_name") %||% df$plate_id[1]) %||% ""),
        FUN.VALUE = character(1)
      )

      plate_ids <- vapply(plan_list, function(df) as.character(df$plate_id[1]), FUN.VALUE = character(1))

      lapply(seq_along(file_names), function(i) {
        shiny::fluidRow(
          shiny::column(6, shiny::strong(file_names[i])),
          shiny::column(
            6,
            shiny::selectInput(
              ns(paste0("plate_select_", i)),
              label    = NULL,
              choices  = stats::setNames(plate_ids, plate_file_names),
              selected = plate_ids[i]
            )
          )
        )
      })
    })

    # ======================================================================
    # 2) Confirm mapping RAW XLSX -> Plate IDs
    # ======================================================================
    shiny::observeEvent(input$confirm_mapping, {
      tryCatch({
        shiny::req(rv$raw_xlsx_list, rv$plate_plan_df_list)

        raw_list  <- rv$raw_xlsx_list
        plan_list <- rv$plate_plan_df_list

        file_names <- vapply(
          raw_list,
          function(x) as.character((attr(x, "file_name") %||% "")),
          FUN.VALUE = character(1)
        )

        plate_ids <- vapply(plan_list, function(df) as.character(df$plate_id[1]), FUN.VALUE = character(1))

        selected_plates <- vapply(
          seq_along(file_names),
          function(i) input[[paste0("plate_select_", i)]],
          FUN.VALUE = character(1)
        )

        mapping_df <- data.frame(
          Raw_Data_File = file_names,
          Plate_ID      = selected_plates,
          stringsAsFactors = FALSE
        )

        if (length(unique(mapping_df$Plate_ID)) != nrow(mapping_df)) {
          stop("Each raw .xlsx file must be associated with a unique Plate ID.")
        }

        rv$ordered_plate_plans <- plan_list[match(selected_plates, plate_ids)]
        rv$mapping             <- mapping_df

        add_console_message("✅ Mapping confirmed successfully!")
        notify("Mapping confirmed successfully.", type = "message")
      }, error = function(e) {
        add_console_message(paste("❌ Error:", e$message))
        notify(conditionMessage(e), type = "error", duration = NULL)
      })
    })

    # ======================================================================
    # 3) MAIN processing action
    # ======================================================================
    shiny::observeEvent(input$run_processing, {

      if (is.null(rv$mapping) || is.null(rv$ordered_plate_plans)) {
        notify("Please match raw data to plate plans before processing!", type = "error")
        return()
      }

      local_log <- character()

      tryCatch({
        cfg <- get_cfg()

        if (is.null(rv$period_df))  stop("Please upload the Period Transitions File (Excel).")
        if (is.null(rv$removal_df)) stop("Please upload the Removal Specifications File (Excel).")

        shiny::req(rv$raw_xlsx_list, rv$ordered_plate_plans)

        n_plates <- length(rv$ordered_plate_plans)

        shiny::withProgress(message = "Processing pipeline…", value = 0, {
          rv$processing_results <- run_processing(
            raw_xlsx_list = rv$raw_xlsx_list,
            plate_plans   = rv$ordered_plate_plans,
            period_df     = rv$period_df,
            removal_df    = rv$removal_df,
            cfg           = cfg,
            raw_zip_list  = rv$raw_zip_list,
            log_fn        = function(msg) { local_log <<- c(local_log, msg) },
            progress_fn   = function(i, n, name) {
              shiny::setProgress(
                value  = i / n,
                detail = sprintf("Plate %d/%d — %s", i, n, name)
              )
            }
          )

          shiny::setProgress(value = 1, detail = sprintf("%d plate(s) done.", n_plates))
        })

        console_messages(c(console_messages(), local_log))
        add_console_message("✅ Results stored in rv$processing_results.")
        notify("Processing completed successfully.", type = "message")

      }, error = function(e) {
        if (length(local_log) > 0) console_messages(c(console_messages(), local_log))
        add_console_message(paste("❌ Error:", conditionMessage(e)))
        notify(conditionMessage(e), type = "error", duration = NULL)
      })
    })

    # ======================================================================
    # 6) Outputs (tables, download, console)
    # ======================================================================
    output$tables_with_periods <- shiny::renderUI({
      shiny::req(rv$processing_results)

      xlsx_list <- rv$processing_results$processed_data_list
      if (length(xlsx_list) == 0) return(NULL)

      tabs <- list()

      # ---- XLSX tabs ----
      tabs <- c(tabs, lapply(seq_along(xlsx_list), function(i) {
        shiny::tabPanel(
          paste0("Plate ", i, ".xlsx"),
          DT::dataTableOutput(ns(paste0("tbl_xlsx_plate_", i)))
        )
      }))

      tabs <- c(tabs, list(
        shiny::tabPanel("All Plates .xlsx", DT::dataTableOutput(ns("tbl_xlsx_all")))
      ))

      # ---- TXT tabs ----
      has_txt <- !is.null(rv$processing_results$txt_by_plate) &&
        length(rv$processing_results$txt_by_plate) == length(xlsx_list) &&
        any(vapply(rv$processing_results$txt_by_plate, function(x) !is.null(x) && nrow(x) > 0, logical(1)))

      if (has_txt) {
        tabs <- c(tabs, lapply(seq_along(xlsx_list), function(i) {
          shiny::tabPanel(
            paste0("Plate ", i, ".txt"),
            DT::dataTableOutput(ns(paste0("tbl_txt_plate_", i)))
          )
        }))

        tabs <- c(tabs, list(
          shiny::tabPanel("All Plates .txt", DT::dataTableOutput(ns("tbl_txt_all")))
        ))
      }

      do.call(shiny::tabsetPanel, tabs)
    })

    shiny::observe({
      shiny::req(rv$processing_results)

      xlsx_list <- rv$processing_results$processed_data_list

      # XLSX per-plate
      lapply(seq_along(xlsx_list), function(i) local({
        ii <- i
        output[[paste0("tbl_xlsx_plate_", ii)]] <- DT::renderDataTable({
          DT::datatable(xlsx_list[[ii]], options = list(scrollX = TRUE, pageLength = 25))
        })
      }))

      # XLSX all
      output$tbl_xlsx_all <- DT::renderDataTable({
        DT::datatable(dplyr::bind_rows(xlsx_list), options = list(scrollX = TRUE, pageLength = 25))
      })

      # TXT per-plate + all
      if (!is.null(rv$processing_results$txt_by_plate)) {
        txt_by_plate <- rv$processing_results$txt_by_plate

        lapply(seq_along(txt_by_plate), function(i) local({
          ii <- i
          output[[paste0("tbl_txt_plate_", ii)]] <- DT::renderDataTable({
            df <- txt_by_plate[[ii]]
            if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame()))
            DT::datatable(df, options = list(scrollX = TRUE, pageLength = 25))
          })
        }))

        output$tbl_txt_all <- DT::renderDataTable({
          df <- rv$processing_results$txt_all
          if (is.null(df) || nrow(df) == 0) return(DT::datatable(data.frame()))
          DT::datatable(df, options = list(scrollX = TRUE, pageLength = 25))
        })
      }
    })

    output$boundary_associations_table <- DT::renderDataTable({
      shiny::req(rv$processing_results)
      dbl <- rv$processing_results$boundary_associations_list
      if (length(dbl) == 0) return(NULL)
      combined <- dplyr::bind_rows(dbl) |>
        dplyr::distinct()
      DT::datatable(combined, options = list(scrollX = TRUE))
    })

    output$download_all_results <- shiny::downloadHandler(
      filename = function() paste0("processing_results_", Sys.Date(), ".zip"),
      contentType = "application/zip",
      content = function(file) {
        shiny::req(rv$processing_results)

        # unique temp folder to avoid collisions
        temp_dir <- file.path(tempdir(), paste0("export_", as.integer(Sys.time())))
        dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

        files_to_zip <- character()

        write_csv_safe <- function(df, out_path) {
          if (is.null(df) || nrow(df) == 0) return(character())
          utils::write.csv(df, out_path, row.names = FALSE)
          out_path
        }

        # processed data (from XLSX processing)
        if (!is.null(rv$processing_results$processed_data_list) &&
            length(rv$processing_results$processed_data_list) > 0) {

          df_proc <- dplyr::bind_rows(rv$processing_results$processed_data_list)
          files_to_zip <- c(files_to_zip, write_csv_safe(df_proc, file.path(temp_dir, "processed_data.csv")))
        }

        # boundary associations
        if (!is.null(rv$processing_results$boundary_associations_list) &&
            length(rv$processing_results$boundary_associations_list) > 0) {

          boundary_df <- dplyr::bind_rows(rv$processing_results$boundary_associations_list) |>
            dplyr::distinct()

          files_to_zip <- c(files_to_zip, write_csv_safe(boundary_df, file.path(temp_dir, "boundary_associations.csv")))
        }

        # txt all + per plate
        if (!is.null(rv$processing_results$txt_all) &&
            nrow(rv$processing_results$txt_all) > 0) {

          files_to_zip <- c(files_to_zip, write_csv_safe(rv$processing_results$txt_all, file.path(temp_dir, "txt_all.csv")))

          if (!is.null(rv$processing_results$txt_by_plate)) {
            for (i in seq_along(rv$processing_results$txt_by_plate)) {
              df <- rv$processing_results$txt_by_plate[[i]]
              if (!is.null(df) && nrow(df) > 0) {
                fn <- file.path(temp_dir, paste0("txt_plate_", i, ".csv"))
                files_to_zip <- c(files_to_zip, write_csv_safe(df, fn))
              }
            }
          }
        }

        # zip ↔ xlsx mapping
        if (!is.null(rv$processing_results$zip_xlsx_match) &&
            nrow(rv$processing_results$zip_xlsx_match) > 0) {

          files_to_zip <- c(
            files_to_zip,
            write_csv_safe(rv$processing_results$zip_xlsx_match, file.path(temp_dir, "zip_xlsx_match.csv"))
          )
        }

        files_to_zip <- unique(files_to_zip)
        files_to_zip <- files_to_zip[file.exists(files_to_zip)]

        if (length(files_to_zip) == 0) {
          stop("Nothing to export: no output files were generated.")
        }

        # ensure target file is clean
        if (file.exists(file)) unlink(file)

        zip::zip(zipfile = file, files = files_to_zip, mode = "cherry-pick")
      }
    )

    output$console_output <- shiny::renderPrint({
      msgs <- console_messages()
      if (length(msgs) == 0) cat("👋 Ready.") else cat(paste(msgs, collapse = "\n"))
    })
  })
}

# ======================================================================
# End of processing_module.R
# ======================================================================
