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
    
    # ---------------- Utility: Detect Plate Type ----------------
    detect_plate_type <- function(df) {
      wells <- sub("_plate_.*", "", df$animal)
      rows  <- unique(sub("([A-Z]).*", "\\1", wells))
      cols  <- unique(as.integer(sub("[A-Z](\\d{2})", "\\1", wells)))
      total <- length(rows) * length(cols)
      if (total %in% c(12, 24, 48, 96)) total else NA
    }
    
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
        
        if (input$create_plate_plan == "yes") {
          # Parse condition names
          condition_names <- trimws(unlist(strsplit(input$conditions_name, ";")))
          if (length(condition_names) != input$conditions_number || any(condition_names == "")) {
            stop("Invalid condition names: must match number of conditions and not be empty.")
          }
          
          inputs <- list(
            create_plate_plan     = input$create_plate_plan,
            plate_type            = input$plate_type,
            conditions_number     = input$conditions_number,
            conditions_name       = condition_names,
            replicates_number     = input$replicates_number,
            units_per_replicate   = input$units_per_replicate,
            plate_number          = input$plate_number,
            keep_border_wells     = input$keep_border_wells,
            seed_value            = input$seed_value,
            plate_plan_name_xlsx  = input$plate_plan_name_xlsx,
            plate_plan_files      = input$plate_plan_files
          )
          
          rv$plate_plan_df_list <- generate_plate_plan_shiny(inputs, write_files = FALSE)
          rv$plate_plan_type    <- rep(as.integer(input$plate_type), length(rv$plate_plan_df_list))
          notify("Plate plan generated successfully.", type = "message")
          
        } else {
          shiny::req(input$plate_plan_files)
          plate_plan_list <- Map(read_file, input$plate_plan_files$datapath, input$plate_plan_files$name)
          
          rv$plate_plan_type <- lapply(plate_plan_list, detect_plate_type)
          for (i in seq_along(plate_plan_list)) {
            if (!"plate_id" %in% colnames(plate_plan_list[[i]])) {
              plate_plan_list[[i]]$plate_id <- paste0("plate_", i)
            }
          }
          rv$plate_plan_df_list <- plate_plan_list
          notify("Plate plans loaded successfully.", type = "message")
        }
      }, error = function(e) {
        notify(conditionMessage(e), type = "error", duration = NULL)
        message("Error in generate_plate_plan_shiny: ", conditionMessage(e))
      })
    })
    
    # ---------------- Update Download Choices ----------------
    shiny::observe({
      if (!is.null(rv$plate_plan_df_list) && length(rv$plate_plan_df_list) > 0) {
        plate_ids <- vapply(seq_along(rv$plate_plan_df_list), function(i) {
          file_name <- attr(rv$plate_plan_df_list[[i]], "file_name")
          if (!is.null(file_name)) file_name else rv$plate_plan_df_list[[i]]$plate_id[1]
        }, character(1))
        shiny::updateSelectInput(session, "download_plate_id", choices = plate_ids, selected = plate_ids[1])
      } else {
        shiny::updateSelectInput(session, "download_plate_id", choices = NULL, selected = NULL)
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
        shiny::tabPanel(
          title,
          shiny::tabsetPanel(
            shiny::tabPanel("Table", DT::dataTableOutput(session$ns(paste0("plate_plan_table_", i)))),
            shiny::tabPanel("Figure", plotly::plotlyOutput(session$ns(paste0("plate_plan_figure_", i))))
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
    
    # ---------------- Utility: Process Figures ----------------
    extract_row_col <- function(df) {
      df$Row     <- sub("([A-Z])\\d{2}_plate_\\d", "\\1", df$animal)
      df$Column  <- as.integer(sub("[A-Z](\\d{2})_plate_\\d", "\\1", df$animal))
      df$well_id <- sub("_plate_.*", "", df$animal)
      df
    }
    
    get_conditions <- function(df) {
      if ("Condition_Base" %in% colnames(df)) {
        df$Condition     <- df$Condition_Base
        df$Condition_Base <- NULL
      } else {
        df$Condition <- ifelse(is.na(df$condition) | df$condition == "",
                               "X", sub("_\\d+$", "", df$condition))
      }
      df
    }
    
    generate_colors <- function(conditions) {
      cols <- scales::hue_pal()(length(conditions))
      stats::setNames(c(cols, "#FFFFFF"), c(conditions, "X"))
    }
    
    # ---------------- Render Figures ----------------
    shiny::observe({
      shiny::req(rv$plate_plan_df_list)
      lapply(seq_along(rv$plate_plan_df_list), function(i) {
        output[[paste0("plate_plan_figure_", i)]] <- plotly::renderPlotly({c
          if (is.null(rv$plate_plan_df_list) || length(rv$plate_plan_df_list) < i) return(NULL)
          df <- rv$plate_plan_df_list[[i]]
          
          # Condition check
          if (!("condition" %in% names(df) || "Condition_Base" %in% names(df))) {
            return(plotly::plot_ly() %>%
                     plotly::layout(title = paste("Plate", i, "Layout"),
                                    annotations = list(text = "No valid conditions", showarrow = FALSE)))
          }
          
          df <- extract_row_col(df) |> get_conditions()
          conditions <- unique(df$Condition[df$Condition != "X"])
          if (length(conditions) == 0) {
            return(plotly::plot_ly() %>%
                     plotly::layout(title = paste("Plate", i, "Layout"),
                                    annotations = list(text = "No valid conditions found", showarrow = FALSE)))
          }
          
          plate_type <- as.character(rv$plate_plan_type[[i]])
          if (is.na(plate_type)) {
            return(plotly::plot_ly() %>%
                     plotly::layout(title = paste("Plate", i, "- Unknown format")))
          }
          
          # Well configs
          config <- list(
            "12" = list(rows = LETTERS[1:3], cols = 1:4),
            "24" = list(rows = LETTERS[1:4], cols = 1:6),
            "48" = list(rows = LETTERS[1:6], cols = 1:8),
            "96" = list(rows = LETTERS[1:8], cols = 1:12)
          )[[plate_type]]
          
          grid <- expand.grid(Row = config$rows, Column = config$cols)
          grid$Row    <- factor(grid$Row, levels = rev(config$rows))
          grid$Column <- factor(grid$Column, levels = as.character(seq_along(config$cols)))
          df$Column   <- factor(df$Column, levels = as.character(seq_along(config$cols)))
          df          <- merge(grid, df, by = c("Row", "Column"), all.x = TRUE)
          df$Condition <- ifelse(is.na(df$Condition), "X", df$Condition)
          
          colors <- generate_colors(conditions)
          
          p <- ggplot2::ggplot(df, ggplot2::aes(x = Column, y = Row, fill = Condition)) +
            ggplot2::geom_tile(color = "black") +
            ggplot2::geom_text(ggplot2::aes(label = well_id), size = 2, color = "black") +
            ggplot2::scale_fill_manual(values = colors) +
            ggplot2::labs(title = paste("Plate", i, "Layout"), x = "Column", y = "Row") +
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0),
                           panel.grid = ggplot2::element_blank())
          
          plotly::ggplotly(p, width = 600, height = 600 * (length(config$rows) / length(config$cols))) %>%
            plotly::layout(margin = list(l = 50, r = 50, b = 50, t = 50))
        })
      })
    })
    
    # ---------------- Download (Single) ----------------
    output$download_plate_plan <- shiny::downloadHandler(
      filename = function() sprintf("%s_%s.xlsx", input$plate_plan_name_xlsx, input$download_plate_id),
      content = function(file) {
        shiny::req(input$download_plate_id)
        idx <- which(vapply(rv$plate_plan_df_list, function(df) df$plate_id[1], "") == input$download_plate_id)
        openxlsx::write.xlsx(rv$plate_plan_df_list[[idx]], file, rowNames = FALSE)
        notify("Plate plan downloaded.", type = "message")
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

# ----------------------------------------------------------------------
# Utility: Generate Plate Plan
# ----------------------------------------------------------------------
generate_plate_plan_shiny <- function(inputs, plan_dir = "inputs/plate_plans", write_files = FALSE) {
  if (write_files && !dir.exists(plan_dir)) dir.create(plan_dir, recursive = TRUE)
  
  if (inputs$create_plate_plan == "yes") {
    # --- Setup
    plate_type   <- as.integer(inputs$plate_type)
    cond_names   <- inputs$conditions_name
    cond_n       <- inputs$conditions_number
    repl_n       <- inputs$replicates_number
    units_n      <- inputs$units_per_replicate
    plate_number <- inputs$plate_number
    border_pref  <- inputs$keep_border_wells
    seed_val     <- inputs$seed_value
    base_xlsx    <- inputs$plate_plan_name_xlsx
    
    # --- Plate configs
    well_configs <- list(
      "12" = list(rows = LETTERS[1:3], cols = 1:4,
                  border = c("A01","A02","A03","A04","B01","B04","C01","C02","C03","C04")),
      "24" = list(rows = LETTERS[1:4], cols = 1:6,
                  border = c("A01","A02","A03","A04","A05","A06","B01","B06","C01","C06",
                             "D01","D02","D03","D04","D05","D06")),
      "48" = list(rows = LETTERS[1:6], cols = 1:8,
                  border = c("A01","A02","A03","A04","A05","A06","A07","A08",
                             "B01","B08","C01","C08","D01","D08","E01","E08",
                             "F01","F02","F03","F04","F05","F06","F07","F08")),
      "96" = list(rows = LETTERS[1:8], cols = 1:12,
                  border = c(paste0("A", sprintf("%02d", 1:12)),
                             paste0(rep(c("B","C","D","E","F","G"), each = 2), sprintf("%02d", c(1,12))),
                             paste0("H", sprintf("%02d", 1:12))))
    )
    config <- well_configs[[as.character(plate_type)]]
    
    wells_template  <- with(expand.grid(Row = config$rows, Column = config$cols), paste0(Row, sprintf("%02d", Column)))
    available_wells <- if (tolower(border_pref) %in% c("no", "n")) setdiff(wells_template, config$border) else wells_template
    available_total <- length(available_wells) * plate_number
    total_units     <- cond_n * repl_n * units_n
    if (total_units > available_total) stop("Total units exceed available wells.")
    
    set.seed(seed_val)
    
    # --- Distribute conditions across plates
    cond_by_plate <- lapply(cond_names, function(cond) {
      total_c <- repl_n * units_n
      base <- floor(total_c / plate_number)
      rem  <- total_c %% plate_number
      counts <- rep(base, plate_number)
      if (rem > 0) counts[1:rem] <- counts[1:rem] + 1
      counts
    })
    names(cond_by_plate) <- cond_names
    
    plate_plan_list <- vector("list", plate_number)
    for (i in seq_len(plate_number)) {
      labels <- unlist(mapply(function(cond, counts) {
        unlist(mapply(function(r, cnt) rep(paste0(cond, "_", r), cnt),
                      seq_len(repl_n), rep(counts[i] / repl_n, repl_n), SIMPLIFY = FALSE))
      }, cond_names, cond_by_plate, SIMPLIFY = FALSE))
      
      plate_assign <- rep("X", length(wells_template))
      avail_idx    <- if (tolower(border_pref) %in% c("no", "n")) which(!wells_template %in% config$border) else seq_along(wells_template)
      choice_idx   <- sample(avail_idx, length(labels))
      plate_assign[choice_idx] <- labels
      
      df <- data.frame(
        animal    = paste0(wells_template, "_plate_", i),
        condition = plate_assign,
        plate_id  = paste0("plate_", i),
        stringsAsFactors = FALSE
      )
      attr(df, "file_name") <- sprintf("%s_plate_%d.xlsx", base_xlsx, i)
      
      if (write_files) {
        xlsx_path <- file.path(plan_dir, sprintf("%s_plate_%d.xlsx", base_xlsx, i))
        openxlsx::write.xlsx(df, xlsx_path, rowNames = FALSE)
      }
      plate_plan_list[[i]] <- df
    }
    return(plate_plan_list)
  } else {
    shiny::req(inputs$plate_plan_files)
    Map(read_file, inputs$plate_plan_files$datapath, inputs$plate_plan_files$name)
  }
}