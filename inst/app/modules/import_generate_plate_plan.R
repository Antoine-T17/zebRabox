# ======================================================================
# Plate Plan Module.R
# Allows users to create or import plate plans, preview them, and download.
# ======================================================================

# ----------------------------------------------------------------------
# UI
# ----------------------------------------------------------------------
plate_plan_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),

    # ---------------- Inputs ----------------
    shiny::fluidRow(
      shinydashboard::box(
        title = "Plate Plan Inputs",
        width = 12,
        shiny::wellPanel(

          # Create or Import
          shiny::selectInput(
            ns("create_plate_plan"), "Create New Plate Plan?",
            choices = c("Select an option" = "", "Yes" = "yes", "No" = "no")
          ),

          # Create workflow
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'yes'", ns("create_plate_plan")),
            shiny::numericInput(ns("plate_number"), "Number of Plates", value = 1, min = 1),
            shiny::selectInput(ns("plate_type"), "Plate Type (wells)",
                               choices = c("Select a type" = "", "12", "24", "48", "96")),
            shiny::numericInput(ns("conditions_number"), "Number of Conditions", value = 1, min = 1),
            shiny::textInput(ns("conditions_name"), "Condition Names (semicolon-separated, e.g., pH8.1;CT)", value = ""),
            shiny::numericInput(ns("replicates_number"), "Replicates per Condition", value = 1, min = 1),
            shiny::numericInput(ns("units_per_replicate"), "Units per Replicate", value = 1, min = 1),
            shiny::selectInput(ns("keep_border_wells"), "Include Border Wells?",
                               choices = c("Select an option" = "", "Yes" = "yes", "No" = "no")),
            shiny::numericInput(ns("seed_value"), "Seed for Randomization", value = 42),
            shiny::textInput(ns("plate_plan_name_xlsx"), "Excel File Name Base", value = "plate_plan")
          ),

          # Import workflow
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'no'", ns("create_plate_plan")),
            shiny::tagAppendAttributes(
              shiny::fileInput(
                ns("plate_plan_files"), "Upload Plate Plan Files (Excel)",
                multiple = TRUE, accept = c(".csv", ".xlsx")
              ),
              `aria-label` = "Upload CSV or Excel files"
            )
          ),

          shiny::actionButton(ns("generate_plate_plan"), "Generate/Load Plate Plan")
        )
      )
    ),

    # ---------------- Preview + Download ----------------
    shiny::fluidRow(
      shinydashboard::box(
        title = "Plate Plan Preview",
        width = 12,
        shiny::uiOutput(ns("plate_plan_tabs")),
        shiny::selectInput(ns("download_plate_id"), "Select Plate ID to Download", choices = NULL),
        shiny::downloadButton(ns("download_plate_plan"), "Download plate plan (.xlsx)"),
        shiny::downloadButton(ns("download_all_plate_plans"), "Download all plate plans (.zip)")
      )
    )
  )
}

# ----------------------------------------------------------------------
# Server
# ----------------------------------------------------------------------
plate_plan_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---------------- Validation (enable button) ----------------
    shiny::observe({
      valid <- input$create_plate_plan != "" &&
        (input$create_plate_plan == "no" ||
           (input$plate_type != "" && input$keep_border_wells != "" &&
              input$plate_number > 0 && input$conditions_number > 0 &&
              input$replicates_number > 0 && input$units_per_replicate > 0 &&
              !is.na(input$seed_value) && input$conditions_name != ""))
      shinyjs::toggleState("generate_plate_plan", valid)
    })

    # ---------------- Generate or Import Plate Plan ----------------
    shiny::observeEvent(input$generate_plate_plan, {
      tryCatch({
        shiny::req(input$create_plate_plan)

        shiny::withProgress(message = "Generating / loading plate plans...", value = 0, {

          if (input$create_plate_plan == "yes") {

            shiny::incProgress(0.2, detail = "Parsing inputs")
            condition_names <- trimws(unlist(strsplit(input$conditions_name, ";")))
            if (length(condition_names) != input$conditions_number || any(condition_names == "")) {
              stop("Invalid condition names: must match number of conditions and not be empty.")
            }

            shiny::incProgress(0.6, detail = "Generating plate plan(s)")
            rv$plate_plan_df_list <- create_plate_plan(
              plate_type      = input$plate_type,
              conditions      = condition_names,
              n_replicates    = input$replicates_number,
              n_units         = input$units_per_replicate,
              n_plates        = input$plate_number,
              include_borders = tolower(input$keep_border_wells) %in% c("yes", "y"),
              seed            = input$seed_value,
              name            = input$plate_plan_name_xlsx
            )

            shiny::incProgress(0.2, detail = "Finalizing")
            rv$plate_plan_type <- rep(as.integer(input$plate_type), length(rv$plate_plan_df_list))

            notify("Plate plan generated successfully.", type = "message")

          } else {

            shiny::req(input$plate_plan_files)

            shiny::incProgress(0.8, detail = "Reading and sorting files")
            plate_plan_list       <- read_plate_plan_files(
              input$plate_plan_files$datapath,
              input$plate_plan_files$name
            )
            rv$plate_plan_type    <- lapply(plate_plan_list, zebRabox:::detect_plate_type)
            rv$plate_plan_df_list <- plate_plan_list
            notify("Plate plans loaded successfully.", type = "message")
          }
        })

      }, error = function(e) {
        notify(conditionMessage(e), type = "error", duration = NULL)
        message("Error in generate_plate_plan_shiny: ", conditionMessage(e))
      })
    })

    # ---------------- Update Download Choices ----------------
    shiny::observe({
      lst <- rv$plate_plan_df_list

      if (!is.null(lst) && length(lst) > 0) {
        # valeur stable pour le serveur
        values <- vapply(lst, function(df) {
          if (!"plate_id" %in% names(df) || nrow(df) == 0) return(NA_character_)
          as.character(df$plate_id[1])
        }, character(1))

        # label affiché à l'utilisateur
        labels <- vapply(seq_along(lst), function(i) {
          file_name <- attr(lst[[i]], "file_name")
          if (!is.null(file_name) && nzchar(file_name)) {
            as.character(file_name)
          } else {
            as.character(values[i])
          }
        }, character(1))

        ok <- !is.na(values) & nzchar(values)
        choices <- stats::setNames(values[ok], labels[ok])  # label -> value

        shiny::updateSelectInput(
          session,
          "download_plate_id",
          choices = choices,
          selected = if (length(values[ok]) > 0) values[ok][1] else character(0)
        )
      } else {
        shiny::updateSelectInput(
          session,
          "download_plate_id",
          choices  = character(0),
          selected = character(0)
        )
      }
    })


    # ---------------- Preview Tabs ----------------
    output$plate_plan_tabs <- shiny::renderUI({
      shiny::req(rv$plate_plan_df_list)
      if (length(rv$plate_plan_df_list) == 0) {
        return(shiny::div("No plate plans generated yet."))
      }

      tabs <- lapply(seq_along(rv$plate_plan_df_list), function(i) {
        file_name <- attr(rv$plate_plan_df_list[[i]], "file_name")
        title     <- if (!is.null(file_name)) file_name else rv$plate_plan_df_list[[i]]$plate_id[1]

        type_i  <- rv$plate_plan_type[[i]]
        n_r_i   <- if (!is.null(type_i) && !is.na(type_i))
                     length(zebRabox:::get_well_config(as.character(type_i))$rows) else 8L
        fig_h_i <- paste0(n_r_i * 38L + 100L, "px")

        shiny::tabPanel(
          title,
          shiny::tabsetPanel(
            shiny::tabPanel("Table", DT::dataTableOutput(session$ns(paste0("plate_plan_table_", i)))),
            shiny::tabPanel(
              "Figure",
              shiny::fluidRow(
                shiny::column(
                  width = 7,
                  style = "overflow-x: auto;",
                  plotly::plotlyOutput(
                    session$ns(paste0("plate_plan_figure_", i)),
                    height = fig_h_i,
                    width  = "100%"
                  )
                ),
                shiny::column(
                  width = 5,
                  shiny::tags$div(
                    style = "text-align:center; font-weight:600; font-size:18px; margin-top:10px; margin-bottom:10px;",
                    "Wells per condition"
                  ),
                  DT::dataTableOutput(session$ns(paste0("plate_plan_counts_", i))))
              )
            )
          )
        )
      })
      do.call(shiny::tabsetPanel, tabs)
    })

    # ---------------- Render Tables ----------------
    shiny::observe({
      shiny::req(rv$plate_plan_df_list)
      lapply(seq_along(rv$plate_plan_df_list), function(i) {
        output[[paste0("plate_plan_table_", i)]] <- DT::renderDataTable({
          if (is.null(rv$plate_plan_df_list) || length(rv$plate_plan_df_list) < i) return(NULL)
          DT::datatable(rv$plate_plan_df_list[[i]],
                        filter = "top",
                        options = list(pageLength = 25, autoWidth = TRUE, orderClasses = TRUE)
          )
        })
      })
    })

    # ---------------- Render Counts (wells per condition) ----------------
    shiny::observe({
      shiny::req(rv$plate_plan_df_list)

      lapply(seq_along(rv$plate_plan_df_list), function(i) {

        output[[paste0("plate_plan_counts_", i)]] <- DT::renderDataTable({

          if (is.null(rv$plate_plan_df_list) || length(rv$plate_plan_df_list) < i) {
            return(NULL)
          }

          df <- rv$plate_plan_df_list[[i]]
          df <- zebRabox:::parse_well_coords(df)
          df <- zebRabox:::normalize_plate_conditions(df)

          # Count wells per condition (exclude X)
          counts <- sort(table(df$Condition[df$Condition != "X"]), decreasing = TRUE)

          out <- data.frame(
            Condition = names(counts),
            Wells     = as.integer(counts),
            stringsAsFactors = FALSE
          )

          # Build the same color mapping as the plot
          conditions <- unique(df$Condition[df$Condition != "X"])
          colors <- zebRabox:::plate_condition_colors(conditions)

          dt <- DT::datatable(
            out,
            rownames = FALSE,
            class = "compact stripe hover",
            style = "bootstrap",
            selection = "single",
            options = list(
              dom = "t",
              paging = FALSE,
              searching = FALSE,
              ordering = FALSE,
              info = FALSE,
              autoWidth = TRUE,
              columnDefs = list(
                list(className = "dt-center", targets = c(0, 1))
              )
            )
          )

          dt <- DT::formatStyle(
            dt,
            "Condition",
            color = DT::styleEqual(names(colors), unname(colors)),
            fontWeight = "600"
          )

          dt
        })
      })
    })


    # ---------------- Render Figures ----------------
    shiny::observe({
      shiny::req(rv$plate_plan_df_list)

      lapply(seq_along(rv$plate_plan_df_list), function(i) {

        output[[paste0("plate_plan_figure_", i)]] <- plotly::renderPlotly({

          if (is.null(rv$plate_plan_df_list) || length(rv$plate_plan_df_list) < i) return(NULL)
          df <- rv$plate_plan_df_list[[i]]

          if (!("condition" %in% names(df) || "Condition_Base" %in% names(df))) {
            return(plotly::layout(
              plotly::plot_ly(),
              title       = paste("Plate", i, "Layout"),
              annotations = list(list(text = "No valid conditions", showarrow = FALSE))
            ))
          }

          df <- zebRabox:::parse_well_coords(df)
          df <- zebRabox:::normalize_plate_conditions(df)

          conditions_no_x <- unique(df$Condition[df$Condition != "X"])
          if (length(conditions_no_x) == 0) {
            return(plotly::layout(
              plotly::plot_ly(),
              title       = paste("Plate", i, "Layout"),
              annotations = list(list(text = "No valid conditions found", showarrow = FALSE))
            ))
          }

          plate_type_i <- as.character(rv$plate_plan_type[[i]])
          if (is.na(plate_type_i)) {
            return(plotly::layout(plotly::plot_ly(),
                                  title = paste("Plate", i, "- Unknown format")))
          }

          config <- zebRabox:::get_well_config(plate_type_i)

          grid        <- expand.grid(
            Row    = factor(config$rows, levels = config$rows),
            Column = factor(seq_along(config$cols), levels = seq_along(config$cols))
          )
          df$Column   <- factor(df$Column, levels = seq_along(config$cols))
          df          <- merge(grid, df, by = c("Row", "Column"), all.x = TRUE)
          df$Condition <- ifelse(is.na(df$Condition), "X", df$Condition)
          df$well_id   <- ifelse(
            is.na(df$well_id),
            paste0(as.character(df$Row),
                   sprintf("%02d", as.integer(as.character(df$Column)))),
            df$well_id
          )

          colors      <- zebRabox:::plate_condition_colors(conditions_no_x)
          all_conds   <- c(conditions_no_x, if ("X" %in% df$Condition) "X")

          cell_px     <- 38L
          fig_w       <- length(config$cols) * cell_px + 220L
          fig_h       <- length(config$rows) * cell_px + 100L
          marker_size <- cell_px - 4L

          fig <- plotly::plot_ly(width = fig_w, height = fig_h)
          for (cond in all_conds) {
            sub_df <- df[df$Condition == cond, ]
            fig <- plotly::add_trace(
              fig,
              x            = as.character(sub_df$Column),
              y            = as.character(sub_df$Row),
              type         = "scatter",
              mode         = "markers+text",
              name         = cond,
              showlegend   = (cond != "X"),
              marker       = list(size   = marker_size,
                                  symbol = "square",
                                  color  = colors[[cond]],
                                  line   = list(color = "black", width = 1)),
              text         = sub_df$well_id,
              textfont     = list(size = 10, color = "black"),
              textposition = "middle center",
              hoverinfo    = "text",
              hovertext    = paste0(sub_df$well_id, "<br>", cond)
            )
          }

          plotly::layout(
            fig,
            title  = paste("Plate", i, "Layout"),
            xaxis  = list(title         = "Column",
                          type          = "category",
                          categoryorder = "array",
                          categoryarray = as.character(seq_along(config$cols))),
            yaxis  = list(title         = "Row",
                          type          = "category",
                          categoryorder = "array",
                          categoryarray = rev(config$rows)),
            margin = list(l = 50, r = 170, b = 50, t = 50)
          )
        })
      })
    })

    # ---------------- Download (Single) ----------------
    output$download_plate_plan <- shiny::downloadHandler(
      filename = function() {
        shiny::req(input$download_plate_id)
        base <- input$plate_plan_name_xlsx
        if (is.null(base) || !nzchar(base)) base <- "plate_plan"
        sprintf("%s_%s.xlsx", base, input$download_plate_id)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      content = function(file) {
        shiny::req(rv$plate_plan_df_list, input$download_plate_id)

        ids <- vapply(rv$plate_plan_df_list, function(df) {
          if (!"plate_id" %in% names(df) || nrow(df) == 0) return(NA_character_)
          as.character(df$plate_id[1])
        }, character(1))

        idx <- which(ids == as.character(input$download_plate_id))
        shiny::validate(shiny::need(length(idx) == 1, "Plate introuvable pour le téléchargement."))

        openxlsx::write.xlsx(rv$plate_plan_df_list[[idx]], file, rowNames = FALSE)
      }
    )


    # ---------------- Download (All as ZIP) ----------------
    output$download_all_plate_plans <- shiny::downloadHandler(
      filename = function() paste0(input$plate_plan_name_xlsx, "_all.zip"),
      content = function(file) {
        temp_dir <- tempdir()
        xlsx_files <- vapply(seq_along(rv$plate_plan_df_list), function(i) {
          path <- file.path(temp_dir, sprintf("%s_plate_%d.xlsx", input$plate_plan_name_xlsx, i))
          openxlsx::write.xlsx(rv$plate_plan_df_list[[i]], path, rowNames = FALSE)
          path
        }, character(1))
        zip::zip(file, files = xlsx_files, mode = "cherry-pick")
        notify("All plate plans downloaded as ZIP.", type = "message")
      }
    )
  })
}

# ======================================================================
# End of import_generate_plate_plan.R
# ======================================================================
