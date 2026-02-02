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
        shiny::fileInput(ns("period_file"), "Upload Period Transitions File (Excel)", accept = ".xlsx"),
        shiny::fileInput(ns("removal_file"), "Upload Removal Specifications File (Excel)", accept = ".xlsx"),
        shiny::actionButton(ns("run_processing"), "Run Full Processing")
      )
    ),
    
    shiny::fluidRow(
      shinydashboard::box(
        title = "Processing Results",
        width = 12,
        shiny::div(style = "margin-bottom: 10px;",
                   shiny::actionButton(ns("clear_console"), "Clear Console", icon = shiny::icon("trash"))),
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Console Output",
            shiny::div(
              style = "height: 800px; overflow-y: auto; padding: 10px; border-radius: 6px;",
              class = "console-container",
              verbatimTextOutput(ns("console_output"), placeholder = TRUE)
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
    
    # console aggregator
    console_messages <- shiny::reactiveVal(character())
    add_console_message <- function(message) {
      console_messages(c(console_messages(), message))
    }
    shiny::observeEvent(input$clear_console, {
      console_messages(character())
      notify("Console cleared.", type = "message")
    })
    
    # helpers
    should_remove <- function(x) {
      # safe scalar check, returns FALSE for vectors / NA / empty string
      if (length(x) != 1) return(FALSE)
      if (is.na(x)) return(FALSE)
      nzchar(trimws(as.character(x)))
    }
    `%||%` <- function(a, b) if (is.null(a)) b else a
    
    # UI selectors to map raw files -> plate plans
    output$file_plate_selectors <- shiny::renderUI({
      shiny::req(rv$raw_data_list, rv$plate_plan_df_list)
      if (length(rv$raw_data_list) != length(rv$plate_plan_df_list)) {
        add_console_message(sprintf("❌ Error: Number of raw data files (%d) must match number of plate plans (%d).",
                                    length(rv$raw_data_list), length(rv$plate_plan_df_list)))
        notify(
          sprintf("Number of raw data files (%d) must match number of plate plans (%d).",
                  length(rv$raw_data_list), length(rv$plate_plan_df_list)),
          type = "error",
          duration = NULL
        )
        return(NULL)
      }
      
      file_names       <- sapply(rv$raw_data_list, attr, "file_name")
      plate_file_names <- sapply(rv$plate_plan_df_list, function(df) attr(df, "file_name") %||% df$plate_id[1])
      plate_ids        <- sapply(rv$plate_plan_df_list, function(df) df$plate_id[1])
      
      lapply(seq_along(file_names), function(i) {
        shiny::fluidRow(
          shiny::column(6, shiny::strong(file_names[i])),
          shiny::column(
            6,
            shiny::selectInput(
              ns(paste0("plate_select_", i)),
              label   = NULL,
              choices = stats::setNames(plate_ids, plate_file_names),
              selected = plate_ids[i]
            )
          )
        )
      })
    })
    
    # mapping confirmation
    shiny::observeEvent(input$confirm_mapping, {
      tryCatch({
        shiny::req(rv$raw_data_list, rv$plate_plan_df_list)
        file_names <- sapply(rv$raw_data_list, attr, "file_name")
        plate_ids  <- sapply(rv$plate_plan_df_list, function(df) df$plate_id[1])
        
        selected_plates <- sapply(seq_along(file_names), function(i) input[[paste0("plate_select_", i)]])
        
        mapping_df <- data.frame(
          Raw_Data_File = file_names,
          Plate_ID      = selected_plates,
          stringsAsFactors = FALSE
        )
        
        if (length(unique(mapping_df$Plate_ID)) != nrow(mapping_df)) {
          stop("Each raw data file must be associated with a unique Plate ID.")
        }
        
        rv$ordered_plate_plans <- rv$plate_plan_df_list[match(selected_plates, plate_ids)]
        rv$mapping <- mapping_df
        
        add_console_message("✅ Mapping confirmed successfully!")
        notify("Mapping confirmed successfully.", type = "message")
      }, error = function(e) {
        add_console_message(paste("❌ Error:", e$message))
        notify(conditionMessage(e), type = "error", duration = NULL)
      })
    })
    
    # ------------------ shared helpers ------------------
    generate_conditions <- function(current_data, current_plan, i) {
      clean_id <- function(x) sub("_.*$", "", x)
      
      current_data$condition <- sapply(current_data$animal, function(a) {
        vals <- current_plan$condition[clean_id(current_plan$animal) == a]
        if (length(vals) == 0) NA else vals
      })
      if (all(is.na(current_data$condition))) {
        add_console_message(sprintf("❌ Error: Plate %d - No conditions could be assigned. Check animal ID matching between raw data and plate plan.", i))
        stop("Condition assignment failed for Plate ", i)
      }
      current_data$plate_id <- current_plan$plate_id[1]
      
      if (!"condition_grouped" %in% names(current_plan)) {
        current_plan$condition_grouped <- sapply(current_plan$condition, function(cond) {
          if (is.na(cond)) NA else sub("_.*$", "", cond)
        })
      }
      current_data$condition_grouped <- sapply(current_data$animal, function(a) {
        vals <- current_plan$condition_grouped[clean_id(current_plan$animal) == a]
        if (length(vals) == 0) NA else vals
      })
      
      if (!"condition_tagged" %in% names(current_plan)) {
        current_plan <- current_plan %>%
          dplyr::group_by(.data$condition_grouped) %>%
          dplyr::mutate(condition_tagged = ifelse(.data$condition == "X", "X",
                                                  paste0(.data$condition_grouped, "_", dplyr::row_number()))) %>%
          dplyr::ungroup()
      }
      current_data$condition_tagged <- sapply(current_data$animal, function(a) {
        vals <- current_plan$condition_tagged[clean_id(current_plan$animal) == a]
        if (length(vals) == 0) NA else vals
      })
      
      list(data = current_data, plan = current_plan)
    }
    
    assign_periods <- function(current_data, period_df, i) {
      cfg <- get_cfg()
      current_data$start <- as.numeric(current_data$start)
      if (any(is.na(current_data$start))) {
        add_console_message(sprintf("⚠️ Warning: Plate %d - Some values in 'start' column could not be converted to numeric.", i))
      }
      
      add_console_message(sprintf("Plate %d - Range of current_data$start (s): [%.2f, %.2f]", i, min(current_data$start, na.rm = TRUE), max(current_data$start, na.rm = TRUE)))
      add_console_message(sprintf("Plate %d - Start time codes (s): %s", i, paste(period_df$start, collapse = ", ")))
      add_console_message(sprintf("Plate %d - Transitions: %s", i, paste(period_df$transition, collapse = "; ")))
      
      boundaries  <- period_df$start
      transitions <- period_df$transition
      split_list  <- strsplit(transitions, "-")
      split_list  <- lapply(split_list, function(parts) {
        if (length(parts) == 1) c(parts, parts)
        else if (length(parts) == 2) parts
        else stop("Each transition must contain a hyphen.")
      })
      
      current_data$period_with_numbers <- NA_character_
      for (j in seq_len(nrow(period_df))) {
        start_time    <- period_df$start[j]
        transition    <- split_list[[j]]
        period_before <- transition[1]
        period_after  <- transition[2]
        
        add_console_message(sprintf("Plate %d - Transition %d: %s -> %s at %.2f s", i, j, period_before, period_after, start_time))
        
        if (j == 1) {
          current_data$period_with_numbers[current_data$start < start_time] <- period_before
          add_console_message(sprintf("Plate %d - Assigned '%s' to rows where start < %.2f", i, period_before, start_time))
        }
        if (j < nrow(period_df)) {
          next_start_time <- period_df$start[j + 1]
          current_data$period_with_numbers[current_data$start >= start_time & current_data$start < next_start_time] <- period_after
          add_console_message(sprintf("Plate %d - Assigned '%s' to rows where %.2f <= start < %.2f", i, period_after, start_time, next_start_time))
        } else {
          current_data$period_with_numbers[current_data$start >= start_time] <- period_after
          add_console_message(sprintf("Plate %d - Assigned '%s' to rows where start >= %.2f", i, period_after, start_time))
        }
      }
      
      # use the mapping provided by config (LDM or VM)
      cfg <- get_cfg()
      if (!is.null(cfg$period_map) && is.function(cfg$period_map)) {
        current_data$period_without_numbers <- cfg$period_map(current_data$period_with_numbers)
      } else {
        current_data$period_without_numbers <- current_data$period_with_numbers
      }
      
      list(data = current_data, boundaries = boundaries, transitions = transitions)
    }
    
    remove_time_codes <- function(current_data, removal_row, i) {
      add_console_message("-")
      add_console_message(sprintf("Plate %d - Range of 'start' column: [%.2f, %.2f]", i, min(current_data$start, na.rm = TRUE), max(current_data$start, na.rm = TRUE)))
      
      if (should_remove(removal_row$remove_time_codes)) {
        time_codes_str <- as.character(removal_row$remove_time_codes)
        time_codes_to_remove <- suppressWarnings(as.numeric(unlist(strsplit(time_codes_str, ","))))
        if (length(time_codes_to_remove) > 0 && !any(is.na(time_codes_to_remove))) {
          invalid_time_codes <- setdiff(time_codes_to_remove, current_data$start)
          if (length(invalid_time_codes) > 0) {
            add_console_message(sprintf("⚠️ Warning: Plate %d - These time codes do not match any 'start': %s", i, paste(invalid_time_codes, collapse = ", ")))
          }
          time_codes_to_remove <- intersect(time_codes_to_remove, current_data$start)
          if (length(time_codes_to_remove) > 0) {
            add_console_message(sprintf("Plate %d - Removing time codes: %s", i, paste(time_codes_to_remove, collapse = ", ")))
            current_data <- current_data[!current_data$start %in% time_codes_to_remove, ]
          }
        } else {
          add_console_message(sprintf("⚠️ Warning: Plate %d - Invalid/empty 'remove_time_codes': %s", i, time_codes_str))
        }
      } else {
        add_console_message(sprintf("Plate %d - No time codes to remove.", i))
      }
      add_console_message(sprintf("✅ Plate %d - Done.", i))
      current_data
    }
    
    remove_periods <- function(current_data, removal_row, i) {
      add_console_message("-")
      unique_periods <- unique(current_data$period_with_numbers)
      add_console_message(sprintf("Plate %d - Unique periods assigned: %s", i, paste(unique_periods, collapse = ", ")))
      
      if (should_remove(removal_row$remove_periods)) {
        periods_to_remove <- trimws(unlist(strsplit(removal_row$remove_periods, ",")))
        if (length(periods_to_remove) > 0) {
          invalid_periods <- setdiff(periods_to_remove, current_data$period_with_numbers)
          if (length(invalid_periods) > 0) {
            add_console_message(sprintf("⚠️ Warning: Plate %d - These periods do not match any 'period_with_numbers': %s", i, paste(invalid_periods, collapse = ", ")))
          }
          periods_to_remove <- intersect(periods_to_remove, current_data$period_with_numbers)
          if (length(periods_to_remove) > 0) {
            add_console_message(sprintf("Plate %d - Removing periods: %s", i, paste(periods_to_remove, collapse = ", ")))
            current_data <- current_data[!current_data$period_with_numbers %in% periods_to_remove, ]
          }
        }
      } else {
        add_console_message(sprintf("Plate %d - No periods to remove.", i))
      }
      add_console_message(sprintf("✅ Plate %d - Done.", i))
      current_data
    }
    
    remove_wells <- function(current_data, removal_row, i) {
      add_console_message("-")
      if (should_remove(removal_row$remove_wells)) {
        wells_str <- as.character(removal_row$remove_wells)
        wells_to_remove <- trimws(unlist(strsplit(wells_str, ",")))
        if (length(wells_to_remove) > 0 && !any(is.na(wells_to_remove))) {
          invalid_wells <- setdiff(wells_to_remove, current_data$animal)
          if (length(invalid_wells) > 0) {
            add_console_message(sprintf("⚠️ Warning: Plate %d - These wells do not match any 'animal': %s", i, paste(invalid_wells, collapse = ", ")))
          }
          wells_to_remove <- intersect(wells_to_remove, current_data$animal)
          if (length(wells_to_remove) > 0) {
            add_console_message(sprintf("Plate %d - Wells to remove: %s", i, paste(wells_to_remove, collapse = ", ")))
            current_data <- current_data[!current_data$animal %in% wells_to_remove, ]
          }
        } else {
          add_console_message(sprintf("⚠️ Warning: Plate %d - Invalid/empty 'remove_wells': %s", i, wells_str))
        }
      } else {
        add_console_message(sprintf("Plate %d - No wells to remove.", i))
      }
      add_console_message(sprintf("✅ Plate %d - Done.", i))
      current_data
    }
    
    remove_conditions <- function(current_data, removal_row, i) {
      add_console_message("-")
      unique_condition_grouped <- unique(current_data$condition_grouped)
      add_console_message(sprintf("Plate %d - Unique 'condition_grouped': %s", i, paste(unique_condition_grouped, collapse = ", ")))
      
      if (should_remove(removal_row$remove_conditions)) {
        conditions_str <- as.character(removal_row$remove_conditions)
        conditions_to_remove <- trimws(unlist(strsplit(conditions_str, ",")))
        if (length(conditions_to_remove) > 0 && !any(is.na(conditions_to_remove))) {
          if (!"condition" %in% colnames(current_data) || all(is.na(current_data$condition))) {
            add_console_message(sprintf("❌ Error: Plate %d - 'condition' column missing/empty. Cannot remove conditions.", i))
          } else {
            invalid_conditions <- setdiff(conditions_to_remove, current_data$condition)
            if (length(invalid_conditions) > 0) {
              add_console_message(sprintf("⚠️ Warning: Plate %d - These conditions do not match any 'condition': %s", i, paste(invalid_conditions, collapse = ", ")))
            }
            conditions_to_remove <- intersect(conditions_to_remove, current_data$condition)
            if (length(conditions_to_remove) > 0) {
              add_console_message(sprintf("Plate %d - Removing conditions: %s", i, paste(conditions_to_remove, collapse = ", ")))
              current_data <- current_data[!current_data$condition %in% conditions_to_remove, ]
            }
          }
        } else {
          add_console_message(sprintf("⚠️ Warning: Plate %d - Invalid/empty 'remove_conditions': %s", i, conditions_str))
        }
      } else {
        add_console_message(sprintf("Plate %d - No conditions to remove.", i))
      }
      add_console_message(sprintf("✅ Plate %d - Done.", i))
      current_data
    }
    
    process_zones <- function(current_data, i) {
      cfg <- get_cfg()
      add_console_message("---")
      add_console_message(sprintf("🔄 Plate %d - Processing zones…", i))
      add_console_message("---")
      
      zones <- unique(current_data$an)
      zone_data <- purrr::map(as.character(zones), ~ dplyr::filter(current_data, .data$an == .x))
      names(zone_data) <- as.character(zones)
      
      # If 0 and 2 present compute zone 1 using cfg$zone_num_cols
      if (all(c(0, 2) %in% zones) && length(cfg$zone_num_cols) > 0) {
        
        z0 <- zone_data[["0"]] |> dplyr::mutate(.row0 = dplyr::row_number())
        z2 <- zone_data[["2"]]
        
        # Clés d’alignement : à adapter si tu as mieux.
        # Ici : même animal + même start (temps) dans une même plaque.
        keys <- intersect(c("plate_id", "animal", "start"), names(z0))
        keys <- intersect(keys, names(z2))
        
        if (length(keys) < 2) {
          add_console_message(sprintf("⚠️ Plate %d - Not enough keys to align zones 0 and 2; skipping zone 1.", i))
        } else {
          
          # on ne garde que les colonnes utiles côté zone 2
          z2_small <- z2 |> dplyr::select(dplyr::all_of(c(keys, cfg$zone_num_cols)))
          
          joined <- dplyr::left_join(
            z0,
            z2_small,
            by = keys,
            suffix = c("_0", "_2")
          )
          
          for (col in cfg$zone_num_cols) {
            c0 <- paste0(col, "_0")
            c2 <- paste0(col, "_2")
            if (c0 %in% names(joined) && c2 %in% names(joined)) {
              joined[[col]] <- joined[[c0]] - joined[[c2]]
            } else {
              add_console_message(sprintf("⚠️ Plate %d - zone column '%s' missing after join; skipping.", i, col))
            }
          }
          
          # reconstruire zone 1 au format “zone_data[[..]]”
          zone_data[["1"]] <- joined |>
            dplyr::arrange(.row0) |>
            dplyr::select(-.row0, -dplyr::any_of(paste0(cfg$zone_num_cols, "_0")), -dplyr::any_of(paste0(cfg$zone_num_cols, "_2")))
          
          add_console_message(sprintf("✅ Plate %d - Zone 1 calculated by key-join (no recycling).", i))
        }
      }
      
      processed_zones <- purrr::map(names(zone_data), function(z) {
        cols_to_select <- unique(c(
          "plate_id","period","animal","condition","condition_grouped","condition_tagged",
          "period_with_numbers","period_without_numbers","zone","start","end"
          , cfg$zone_num_cols))
        zd <- zone_data[[z]] %>%
          dplyr::mutate(zone = z) %>%
          dplyr::select(dplyr::any_of(cols_to_select))
        add_console_message(sprintf("✅ Plate %d - Zone %s processed.", i, z))
        zd
      })
      
      dplyr::bind_rows(processed_zones)
    }
    
    # main processing action
    shiny::observeEvent(input$run_processing, {
      # === mapping between plate plan and raw data ===
      if (is.null(rv$mapping) || is.null(rv$ordered_plate_plans)) {
        notify("Please match raw data to plate plans, transitions and removal files before processing!", type = "error")
        return()
      }
      tryCatch({
        cfg <- get_cfg() # concrete config at runtime
        if (is.null(input$period_file))  stop("Please upload the Period Transitions File (Excel).")
        if (is.null(input$removal_file)) stop("Please upload the Removal Specifications File (Excel).")
        
        shiny::req(rv$raw_data_list, rv$ordered_plate_plans)
        
        # numeric conversion helper (use cfg$convert_cols)
        convert_numeric_cols <- function(df, cols) {
          for (col in intersect(names(df), cols)) {
            df[[col]] <- as.numeric(gsub(",", ".", as.character(df[[col]])))
          }
          df
        }
        
        period_df <- readxl::read_excel(input$period_file$datapath)
        shiny::req(all(c("start", "transition") %in% colnames(period_df)))
        period_df$start <- as.numeric(period_df$start)
        period_df <- period_df[order(period_df$start), ]
        add_console_message("✅ Period transitions file loaded successfully.")
        
        removal_df <- readxl::read_excel(input$removal_file$datapath)
        shiny::req("plate_id" %in% colnames(removal_df))
        for (colname in c("remove_time_codes","remove_wells","remove_conditions","remove_periods")) {
          if (!(colname %in% colnames(removal_df))) removal_df[[colname]] <- NA_character_
          removal_df[[colname]] <- as.character(removal_df[[colname]])
          removal_df[[colname]] <- ifelse(
            is.na(removal_df[[colname]]) | tolower(trimws(removal_df[[colname]])) %in% c("", "no"),
            NA_character_,
            removal_df[[colname]]
          )
        }
        removal_df$plate_id <- suppressWarnings(as.numeric(gsub("Plate", "", removal_df$plate_id)))
        add_console_message("✅ Removal specifications file loaded successfully.")
        
        extracted_data_list <- rv$raw_data_list
        plate_plans         <- rv$ordered_plate_plans
        n_plates            <- length(plate_plans)
        
        add_console_message("🔄 Starting data extraction, enrichment, and period assignment...")
        
        processed_data_list        <- list()
        boundary_associations_list <- list()
        
        for (i in seq_len(n_plates)) {
          add_console_message("-----")
          add_console_message(sprintf("Processing plate %d", i))
          add_console_message("-----")
          
          current_plan <- plate_plans[[i]]
          current_data <- extracted_data_list[[i]]
          
          # apply optional filter (QM modes)
          if (!is.null(cfg$filter_fn) && is.function(cfg$filter_fn)) {
            current_data <- cfg$filter_fn(current_data)
            add_console_message(sprintf("✅ Plate %d - Applied mode-specific filter (if any).", i))
          }
          
          add_console_message("🔄 Column & condition validation...")
          required_columns <- c("animal","condition","plate_id")
          missing_cols     <- setdiff(required_columns, colnames(current_plan))
          if (length(missing_cols) > 0) {
            stop(sprintf("Plate %d is missing required columns: %s", i, paste(missing_cols, collapse = ", ")))
          }
          add_console_message(sprintf("✅ Plate %d - Plate plan validated.", i))
          
          cond_res    <- generate_conditions(current_data, current_plan, i)
          current_data <- cond_res$data
          current_plan <- cond_res$plan
          add_console_message(sprintf("✅ Plate %d - Conditions assigned.", i))
          
          per_res <- assign_periods(current_data, period_df, i)
          current_data <- per_res$data
          boundaries   <- per_res$boundaries
          transitions  <- per_res$transitions
          add_console_message(sprintf("✅ Plate %d - Periods assigned.", i))
          
          plate_id <- suppressWarnings(as.numeric(current_plan$plate_id[1]))
          removal_row <- removal_df[removal_df$plate_id == plate_id, , drop = FALSE]
          
          if (nrow(removal_row) > 0) {
            add_console_message("🔄 Applying removal specifications...")
            # pass a 1-row data.frame (safe)
            rr <- removal_row[1, , drop = FALSE]
            current_data <- remove_time_codes(current_data, rr, i)
            current_data <- remove_periods(current_data, rr, i)
            current_data <- remove_wells(current_data, rr, i)
            current_data <- remove_conditions(current_data, rr, i)
            add_console_message("✅ Removal steps completed.")
          } else {
            add_console_message("ℹ️ No removal rules for this plate (continuing).")
          }
          
          current_data <- convert_numeric_cols(current_data, cfg$convert_cols)
          add_console_message(sprintf("✅ Plate %d - Numeric columns converted.", i))
          
          zone_combined <- process_zones(current_data, i)
          
          processed_data_list[[i]] <- zone_combined
          boundary_associations_list[[i]] <- data.frame(
            plate_id    = as.character(current_plan$plate_id[1]),
            time_switch = boundaries,
            transition  = transitions,
            stringsAsFactors = FALSE
          )
        }
        
        add_console_message("\n✅ Data processing completed for all plates!")
        
        rv$processing_results <- list(
          processed_data_list = processed_data_list,
          boundary_associations_list = boundary_associations_list
        )
        
        add_console_message("✅ Results stored in rv$processing_results.")
        notify("Processing completed successfully.", type = "message")
        
      }, error = function(e) {
        add_console_message(paste("❌ Error:", e$message))
        notify(conditionMessage(e), type = "error", duration = NULL)
      })
    })
    
    # Outputs (tables, download, console)
    output$tables_with_periods <- shiny::renderUI({
      shiny::req(rv$processing_results)
      dfs <- rv$processing_results$processed_data_list
      if (length(dfs) == 0) return(NULL)
      tabs <- lapply(seq_along(dfs), function(i) {
        shiny::tabPanel(paste0("Plate ", i), DT::dataTableOutput(ns(paste0("tbl_plate_", i))))
      })
      tabs <- append(tabs, list(shiny::tabPanel("All Plates", DT::dataTableOutput(ns("tbl_all_plates")))))
      do.call(shiny::tabsetPanel, tabs)
    })
    
    shiny::observe({
      shiny::req(rv$processing_results)
      dfs <- rv$processing_results$processed_data_list
      lapply(seq_along(dfs), function(i) local({
        ii <- i
        output[[paste0("tbl_plate_", ii)]] <- DT::renderDataTable({
          DT::datatable(dfs[[ii]], options = list(scrollX = TRUE, pageLength = 25))
        })
      }))
      output$tbl_all_plates <- DT::renderDataTable({
        DT::datatable(dplyr::bind_rows(dfs), options = list(scrollX = TRUE, pageLength = 25))
      })
    })
    
    output$boundary_associations_table <- DT::renderDataTable({
      shiny::req(rv$processing_results)
      dbl <- rv$processing_results$boundary_associations_list
      if (length(dbl) == 0) return(NULL)
      combined <- dplyr::bind_rows(dbl) %>% dplyr::distinct()
      DT::datatable(combined, options = list(scrollX = TRUE))
    })
    
    output$download_all_results <- shiny::downloadHandler(
      filename = function() paste0("processing_results_", Sys.Date(), ".zip"),
      content = function(file) {
        shiny::req(rv$processing_results)
        temp_dir <- tempdir()
        files_to_zip <- c()
        if (length(rv$processing_results$processed_data_list) > 0) {
          writexl::write_xlsx(dplyr::bind_rows(rv$processing_results$processed_data_list),
                              file.path(temp_dir, "processed_data.xlsx"))
          files_to_zip <- c(files_to_zip, file.path(temp_dir, "processed_data.xlsx"))
        }
        if (length(rv$processing_results$boundary_associations_list) > 0) {
          boundary_df <- dplyr::bind_rows(rv$processing_results$boundary_associations_list) %>% dplyr::distinct()
          writexl::write_xlsx(boundary_df, file.path(temp_dir, "boundary_associations.xlsx"))
          files_to_zip <- c(files_to_zip, file.path(temp_dir, "boundary_associations.xlsx"))
        }
        zip::zip(file, files_to_zip, mode = "cherry-pick")
      }
    )
    
    output$console_output <- renderPrint({
      msgs <- console_messages()
      if (length(msgs) == 0) {
        cat("👋 Ready.")
      } else {
        cat(paste(msgs, collapse = "\n"))
      }
    })
  })
}

# ======================================================================
# End of processing_module.R
# ======================================================================
