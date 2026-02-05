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

    should_remove <- function(x) {
      if (length(x) != 1) return(FALSE)
      if (is.na(x)) return(FALSE)
      nzchar(trimws(as.character(x)))
    }

    # ---- console aggregator ----
    console_messages <- shiny::reactiveVal(character())
    add_console_message <- function(message) {
      console_messages(c(console_messages(), message))
    }
    shiny::observeEvent(input$clear_console, {
      console_messages(character())
      notify("Console cleared.", type = "message")
    })

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
    # 3) Shared helpers used by processing
    # ======================================================================
    generate_conditions <- function(current_data, current_plan, i) {
      clean_id <- function(x) sub("_.*$", "", x)

      current_data$condition <- vapply(current_data$animal, function(a) {
        vals <- current_plan$condition[clean_id(current_plan$animal) == a]
        if (length(vals) == 0) NA_character_ else vals[1]
      }, FUN.VALUE = character(1))

      if (all(is.na(current_data$condition))) {
        add_console_message(sprintf(
          "❌ Error: Plate %d - No conditions could be assigned. Check animal ID matching between raw data and plate plan.",
          i
        ))
        stop("Condition assignment failed for Plate ", i)
      }

      current_data$plate_id <- current_plan$plate_id[1]

      if (!"condition_grouped" %in% names(current_plan)) {
        current_plan$condition_grouped <- vapply(current_plan$condition, function(cond) {
          if (is.na(cond)) NA_character_ else sub("_.*$", "", cond)
        }, FUN.VALUE = character(1))
      }

      current_data$condition_grouped <- vapply(current_data$animal, function(a) {
        vals <- current_plan$condition_grouped[clean_id(current_plan$animal) == a]
        if (length(vals) == 0) NA_character_ else vals[1]
      }, FUN.VALUE = character(1))

      if (!"condition_tagged" %in% names(current_plan)) {
        current_plan <- current_plan |>
          dplyr::group_by(.data$condition_grouped) |>
          dplyr::mutate(
            condition_tagged = ifelse(
              .data$condition == "X",
              "X",
              paste0(.data$condition_grouped, "_", dplyr::row_number())
            )
          ) |>
          dplyr::ungroup()
      }

      current_data$condition_tagged <- vapply(current_data$animal, function(a) {
        vals <- current_plan$condition_tagged[clean_id(current_plan$animal) == a]
        if (length(vals) == 0) NA_character_ else vals[1]
      }, FUN.VALUE = character(1))

      list(data = current_data, plan = current_plan)
    }

    assign_periods <- function(current_data, period_df, i) {
      cfg <- get_cfg()

      current_data$start <- as.numeric(current_data$start)
      if (any(is.na(current_data$start))) {
        add_console_message(sprintf(
          "⚠️ Warning: Plate %d - Some values in 'start' column could not be converted to numeric.",
          i
        ))
      }

      add_console_message(sprintf(
        "Plate %d - Range of current_data$start (s): [%.2f, %.2f]",
        i, min(current_data$start, na.rm = TRUE), max(current_data$start, na.rm = TRUE)
      ))
      add_console_message(sprintf("Plate %d - Start time codes (s): %s", i, paste(period_df$start, collapse = ", ")))
      add_console_message(sprintf("Plate %d - Transitions: %s", i, paste(period_df$transition, collapse = "; ")))

      boundaries  <- period_df$start
      transitions <- period_df$transition

      split_list <- strsplit(transitions, "-", fixed = TRUE)
      split_list <- lapply(split_list, function(parts) {
        if (length(parts) == 1) c(parts, parts)
        else if (length(parts) == 2) parts
        else stop("Each transition must contain a single hyphen.")
      })

      current_data$period_with_numbers <- NA_character_
      for (j in seq_len(nrow(period_df))) {
        start_time    <- period_df$start[j]
        parts         <- split_list[[j]]
        period_before <- parts[1]
        period_after  <- parts[2]

        add_console_message(sprintf(
          "Plate %d - Transition %d: %s -> %s at %.2f s",
          i, j, period_before, period_after, start_time
        ))

        if (j == 1) {
          current_data$period_with_numbers[current_data$start < start_time] <- period_before
          add_console_message(sprintf("Plate %d - Assigned '%s' to rows where start < %.2f", i, period_before, start_time))
        }

        if (j < nrow(period_df)) {
          next_start <- period_df$start[j + 1]
          current_data$period_with_numbers[current_data$start >= start_time & current_data$start < next_start] <- period_after
          add_console_message(sprintf(
            "Plate %d - Assigned '%s' to rows where %.2f <= start < %.2f",
            i, period_after, start_time, next_start
          ))
        } else {
          current_data$period_with_numbers[current_data$start >= start_time] <- period_after
          add_console_message(sprintf("Plate %d - Assigned '%s' to rows where start >= %.2f", i, period_after, start_time))
        }
      }

      if (!is.null(cfg$period_map) && is.function(cfg$period_map)) {
        current_data$period_without_numbers <- cfg$period_map(current_data$period_with_numbers)
      } else {
        current_data$period_without_numbers <- current_data$period_with_numbers
      }

      list(data = current_data, boundaries = boundaries, transitions = transitions)
    }

    remove_time_codes <- function(current_data, removal_row, i) {
      add_console_message("-")
      add_console_message(sprintf(
        "Plate %d - Range of 'start' column: [%.2f, %.2f]",
        i, min(current_data$start, na.rm = TRUE), max(current_data$start, na.rm = TRUE)
      ))

      if (should_remove(removal_row$remove_time_codes)) {
        time_codes_str <- as.character(removal_row$remove_time_codes)
        time_codes_to_remove <- suppressWarnings(as.numeric(unlist(strsplit(time_codes_str, ","))))
        if (length(time_codes_to_remove) > 0 && !any(is.na(time_codes_to_remove))) {
          invalid <- setdiff(time_codes_to_remove, current_data$start)
          if (length(invalid) > 0) {
            add_console_message(sprintf(
              "⚠️ Warning: Plate %d - These time codes do not match any 'start': %s",
              i, paste(invalid, collapse = ", ")
            ))
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
        periods_to_remove <- trimws(unlist(strsplit(as.character(removal_row$remove_periods), ",")))
        if (length(periods_to_remove) > 0) {
          invalid <- setdiff(periods_to_remove, current_data$period_with_numbers)
          if (length(invalid) > 0) {
            add_console_message(sprintf(
              "⚠️ Warning: Plate %d - These periods do not match any 'period_with_numbers': %s",
              i, paste(invalid, collapse = ", ")
            ))
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
          invalid <- setdiff(wells_to_remove, current_data$animal)
          if (length(invalid) > 0) {
            add_console_message(sprintf(
              "⚠️ Warning: Plate %d - These wells do not match any 'animal': %s",
              i, paste(invalid, collapse = ", ")
            ))
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
      unique_grouped <- unique(current_data$condition_grouped)
      add_console_message(sprintf("Plate %d - Unique 'condition_grouped': %s", i, paste(unique_grouped, collapse = ", ")))

      if (should_remove(removal_row$remove_conditions)) {
        conditions_str <- as.character(removal_row$remove_conditions)
        conditions_to_remove <- trimws(unlist(strsplit(conditions_str, ",")))
        if (length(conditions_to_remove) > 0 && !any(is.na(conditions_to_remove))) {
          if (!"condition" %in% names(current_data) || all(is.na(current_data$condition))) {
            add_console_message(sprintf("❌ Error: Plate %d - 'condition' column missing/empty. Cannot remove conditions.", i))
          } else {
            invalid <- setdiff(conditions_to_remove, current_data$condition)
            if (length(invalid) > 0) {
              add_console_message(sprintf(
                "⚠️ Warning: Plate %d - These conditions do not match any 'condition': %s",
                i, paste(invalid, collapse = ", ")
              ))
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

        keys <- intersect(c("plate_id", "animal", "start"), names(z0))
        keys <- intersect(keys, names(z2))

        if (length(keys) < 2) {
          add_console_message(sprintf("⚠️ Plate %d - Not enough keys to align zones 0 and 2; skipping zone 1.", i))
        } else {
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

          zone_data[["1"]] <- joined |>
            dplyr::arrange(.row0) |>
            dplyr::select(
              -.row0,
              -dplyr::any_of(paste0(cfg$zone_num_cols, "_0")),
              -dplyr::any_of(paste0(cfg$zone_num_cols, "_2"))
            )

          add_console_message(sprintf("✅ Plate %d - Zone 1 calculated by key-join (no recycling).", i))
        }
      }

      processed_zones <- purrr::map(names(zone_data), function(z) {
        cols_to_select <- unique(c(
          "plate_id", "period", "animal",
          "condition", "condition_grouped", "condition_tagged",
          "period_with_numbers", "period_without_numbers",
          "zone", "start", "end",
          cfg$zone_num_cols
        ))

        zd <- zone_data[[z]] |>
          dplyr::mutate(zone = z) |>
          dplyr::select(dplyr::any_of(cols_to_select))

        add_console_message(sprintf("✅ Plate %d - Zone %s processed.", i, z))
        zd
      })

      dplyr::bind_rows(processed_zones)
    }

    # ======================================================================
    # 4) TXT ZIP post-processing helpers
    # ======================================================================
    zip_base_name <- function(x) {
      x <- basename(x)
      sub("\\.zip$", "", x, ignore.case = TRUE)
    }

    xlsx_base_name <- function(x) {
      x <- basename(x)
      sub("\\.xlsx$", "", x, ignore.case = TRUE)
    }

    match_txt_zip_to_raw_xlsx <- function(raw_xlsx, zip_txt, n = 6) {
      needed_raw <- c("location", "animal")
      missing_raw <- setdiff(needed_raw, names(raw_xlsx))
      if (length(missing_raw) > 0) stop("Raw .xlsx missing required columns: ", paste(missing_raw, collapse = ", "))

      if (!("file_txt_name" %in% names(zip_txt))) stop("ZIP txt df missing 'file_txt_name' column.")
      needed_txt <- c("T", "X", "Y")
      missing_txt <- setdiff(needed_txt, names(zip_txt))
      if (length(missing_txt) > 0) stop("ZIP txt df missing required columns: ", paste(missing_txt, collapse = ", "))

      raw_map <- raw_xlsx |>
        dplyr::mutate(
          loc6 = substr(as.character(.data$location), 1, n),
          animal = as.character(.data$animal)
        ) |>
        dplyr::filter(!is.na(.data$loc6), .data$loc6 != "", !is.na(.data$animal), .data$animal != "") |>
        dplyr::distinct(.data$loc6, .data$animal)

      amb <- raw_map |>
        dplyr::count(.data$loc6, name = "n_animals") |>
        dplyr::filter(.data$n_animals > 1)

      if (nrow(amb) > 0) {
        add_console_message("⚠️ Warning: some location prefixes map to multiple DIFFERENT animals (keeping first).")
        raw_map <- raw_map |>
          dplyr::group_by(.data$loc6) |>
          dplyr::slice(1) |>
          dplyr::ungroup()
      }

      txt2 <- zip_txt |>
        dplyr::mutate(txt6 = substr(as.character(.data$file_txt_name), 1, n)) |>
        dplyr::left_join(raw_map, by = c("txt6" = "loc6"))

      txt2 |>
        dplyr::select(.data$T, .data$X, .data$Y, .data$file_txt_name, .data$animal)
    }

    assign_periods_to_txt <- function(txt_df, period_df, cfg) {
      txt_df$start <- suppressWarnings(as.numeric(txt_df$T))
      txt_df$period_with_numbers <- NA_character_

      split_list <- strsplit(as.character(period_df$transition), "-", fixed = TRUE)
      split_list <- lapply(split_list, function(parts) {
        if (length(parts) == 1) c(parts, parts)
        else if (length(parts) == 2) parts
        else stop("Each transition must contain a single hyphen.")
      })

      for (j in seq_len(nrow(period_df))) {
        start_time <- period_df$start[j]
        parts      <- split_list[[j]]
        before     <- parts[1]
        after      <- parts[2]

        if (j == 1) {
          txt_df$period_with_numbers[txt_df$start < start_time] <- before
        }
        if (j < nrow(period_df)) {
          next_start <- period_df$start[j + 1]
          txt_df$period_with_numbers[txt_df$start >= start_time & txt_df$start < next_start] <- after
        } else {
          txt_df$period_with_numbers[txt_df$start >= start_time] <- after
        }
      }

      if (!is.null(cfg$period_map) && is.function(cfg$period_map)) {
        txt_df$period_without_numbers <- cfg$period_map(txt_df$period_with_numbers)
      } else {
        txt_df$period_without_numbers <- txt_df$period_with_numbers
      }

      txt_df
    }

    # ======================================================================
    # 5) MAIN processing action (WITH progress)
    # ======================================================================
    shiny::observeEvent(input$run_processing, {

      if (is.null(rv$mapping) || is.null(rv$ordered_plate_plans)) {
        notify("Please match raw data to plate plans before processing!", type = "error")
        return()
      }

      tryCatch({
        cfg <- get_cfg()

        # use stored files (imported with progress)
        if (is.null(rv$period_df))  stop("Please upload the Period Transitions File (Excel).")
        if (is.null(rv$removal_df)) stop("Please upload the Removal Specifications File (Excel).")

        shiny::req(rv$raw_xlsx_list, rv$ordered_plate_plans)

        period_df  <- rv$period_df
        removal_df <- rv$removal_df

        shiny::withProgress(message = "Processing…", value = 0, {

          shiny::incProgress(0.02, detail = "Initializing…")

          convert_numeric_cols <- function(df, cols) {
            for (col in intersect(names(df), cols)) {
              df[[col]] <- as.numeric(gsub(",", ".", as.character(df[[col]])))
            }
            df
          }

          extracted_data_list <- rv$raw_xlsx_list
          plate_plans         <- rv$ordered_plate_plans
          n_plates            <- length(plate_plans)

          add_console_message("🔄 Starting data extraction, enrichment, and period assignment...")

          processed_data_list        <- vector("list", n_plates)
          boundary_associations_list <- vector("list", n_plates)

          # allocate ~70% for plates loop
          for (i in seq_len(n_plates)) {

            shiny::incProgress(
              amount = 0.70 / max(1, n_plates),
              detail = sprintf("Processing plate %d/%d (.xlsx)…", i, n_plates)
            )

            add_console_message("-----")
            add_console_message(sprintf("Processing plate %d (.xlsx)", i))
            add_console_message("-----")

            current_plan <- plate_plans[[i]]
            current_data <- extracted_data_list[[i]]

            if (!is.null(cfg$filter_fn) && is.function(cfg$filter_fn)) {
              current_data <- cfg$filter_fn(current_data)
              add_console_message(sprintf("✅ Plate %d - Applied mode-specific filter (if any).", i))
            }

            required_columns <- c("animal", "condition", "plate_id")
            missing_cols <- setdiff(required_columns, names(current_plan))
            if (length(missing_cols) > 0) {
              stop(sprintf("Plate %d is missing required columns: %s", i, paste(missing_cols, collapse = ", ")))
            }

            cond_res     <- generate_conditions(current_data, current_plan, i)
            current_data <- cond_res$data
            current_plan <- cond_res$plan

            per_res      <- assign_periods(current_data, period_df, i)
            current_data <- per_res$data
            boundaries   <- per_res$boundaries
            transitions  <- per_res$transitions

            plate_id_num <- suppressWarnings(as.numeric(current_plan$plate_id[1]))
            removal_row  <- removal_df[removal_df$plate_id == plate_id_num, , drop = FALSE]

            if (nrow(removal_row) > 0) {
              rr <- removal_row[1, , drop = FALSE]
              current_data <- remove_time_codes(current_data, rr, i)
              current_data <- remove_periods(current_data, rr, i)
              current_data <- remove_wells(current_data, rr, i)
              current_data <- remove_conditions(current_data, rr, i)
            }

            current_data   <- convert_numeric_cols(current_data, cfg$convert_cols)
            zone_combined  <- process_zones(current_data, i)

            processed_data_list[[i]] <- zone_combined
            boundary_associations_list[[i]] <- data.frame(
              plate_id    = as.character(current_plan$plate_id[1]),
              time_switch = boundaries,
              transition  = transitions,
              stringsAsFactors = FALSE
            )
          }

          rv$processing_results <- list(
            processed_data_list        = processed_data_list,
            boundary_associations_list = boundary_associations_list
          )

          shiny::incProgress(0.05, detail = "Finalizing XLSX results…")
          add_console_message("\n✅ Data processing completed for all plates!")

          # ---- TXT ZIP post-processing ----
          if (!is.null(rv$raw_zip_list) && length(rv$raw_zip_list) > 0) {

            zip_list  <- rv$raw_zip_list
            xlsx_list <- rv$raw_xlsx_list

            shiny::incProgress(0.05, detail = "Matching ZIP ↔ XLSX…")

            zip_names  <- vapply(zip_list,  function(x) as.character((attr(x, "file_name") %||% "")), FUN.VALUE = character(1))
            xlsx_names <- vapply(xlsx_list, function(x) as.character((attr(x, "file_name") %||% "")), FUN.VALUE = character(1))

            zip_bases  <- zip_base_name(zip_names)
            xlsx_bases <- xlsx_base_name(xlsx_names)

            idx_xlsx_for_zip <- match(zip_bases, xlsx_bases)
            if (any(is.na(idx_xlsx_for_zip))) {
              missing <- zip_names[is.na(idx_xlsx_for_zip)]
              stop(
                "Processing stopped: no exact match between ZIP and XLSX names for: ",
                paste(missing, collapse = ", "),
                ".\nExpected ZIP base name to exactly match a raw_data .xlsx base name."
              )
            }

            txt_by_plate <- vector("list", length(rv$ordered_plate_plans))
            names(txt_by_plate) <- paste0("Plate_", seq_along(txt_by_plate))

            zip_xlsx_match <- data.frame(
              zip_name   = zip_names,
              xlsx_name  = xlsx_names[idx_xlsx_for_zip],
              plate_idx  = idx_xlsx_for_zip,
              stringsAsFactors = FALSE
            )

            for (k in seq_along(zip_list)) {

              shiny::incProgress(
                amount = 0.18 / max(1, length(zip_list)),
                detail = sprintf("Processing TXT %d/%d (.zip)…", k, length(zip_list))
              )

              plate_idx  <- idx_xlsx_for_zip[k]
              raw_xlsx_k <- xlsx_list[[plate_idx]]
              zip_txt_k  <- zip_list[[k]]

              txt_k <- match_txt_zip_to_raw_xlsx(raw_xlsx_k, zip_txt_k, n = 6)

              proc_k <- rv$processing_results$processed_data_list[[plate_idx]]

              meta_k <- proc_k |>
                dplyr::select(.data$plate_id, .data$animal, .data$condition, .data$condition_grouped, .data$condition_tagged) |>
                dplyr::mutate(
                  animal            = trimws(as.character(.data$animal)),
                  plate_id          = as.character(.data$plate_id),
                  condition         = as.character(.data$condition),
                  condition_grouped = as.character(.data$condition_grouped),
                  condition_tagged  = as.character(.data$condition_tagged)
                ) |>
                dplyr::group_by(.data$animal) |>
                dplyr::slice(1) |>
                dplyr::ungroup()

              txt_k <- txt_k |>
                dplyr::mutate(animal = trimws(as.character(.data$animal))) |>
                dplyr::left_join(meta_k, by = "animal", relationship = "many-to-one") |>
                dplyr::mutate(zone = NA_character_)

              txt_k <- assign_periods_to_txt(txt_k, period_df, cfg)

              txt_k <- txt_k |>
                dplyr::select(
                  .data$T, .data$X, .data$Y, .data$file_txt_name, .data$animal,
                  .data$plate_id, .data$condition, .data$condition_grouped, .data$condition_tagged,
                  .data$period_with_numbers, .data$period_without_numbers,
                  .data$zone
                )

              txt_by_plate[[plate_idx]] <- dplyr::bind_rows(txt_by_plate[[plate_idx]], txt_k)
            }

            rv$processing_results$zip_xlsx_match <- zip_xlsx_match
            rv$processing_results$txt_by_plate   <- txt_by_plate
            rv$processing_results$txt_all        <- dplyr::bind_rows(txt_by_plate)

            shiny::incProgress(0.05, detail = "Finalizing TXT results…")
            add_console_message("✅ TXT results stored plate-by-plate (txt_by_plate) + combined (txt_all).")
          }

          shiny::incProgress(0.05, detail = "Done.")
        })

        add_console_message("✅ Results stored in rv$processing_results.")
        notify("Processing completed successfully.", type = "message")

      }, error = function(e) {
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
      content = function(file) {
        shiny::req(rv$processing_results)

        temp_dir <- tempdir()
        files_to_zip <- character()

        # processed data (xlsx)
        if (length(rv$processing_results$processed_data_list) > 0) {
          writexl::write_xlsx(
            dplyr::bind_rows(rv$processing_results$processed_data_list),
            file.path(temp_dir, "processed_data.xlsx")
          )
          files_to_zip <- c(files_to_zip, file.path(temp_dir, "processed_data.xlsx"))
        }

        # boundary associations
        if (length(rv$processing_results$boundary_associations_list) > 0) {
          boundary_df <- dplyr::bind_rows(rv$processing_results$boundary_associations_list) |>
            dplyr::distinct()
          writexl::write_xlsx(boundary_df, file.path(temp_dir, "boundary_associations.xlsx"))
          files_to_zip <- c(files_to_zip, file.path(temp_dir, "boundary_associations.xlsx"))
        }

        # txt all + per plate
        if (!is.null(rv$processing_results$txt_all) && nrow(rv$processing_results$txt_all) > 0) {
          writexl::write_xlsx(rv$processing_results$txt_all, file.path(temp_dir, "txt_all.xlsx"))
          files_to_zip <- c(files_to_zip, file.path(temp_dir, "txt_all.xlsx"))

          if (!is.null(rv$processing_results$txt_by_plate)) {
            for (i in seq_along(rv$processing_results$txt_by_plate)) {
              df <- rv$processing_results$txt_by_plate[[i]]
              if (!is.null(df) && nrow(df) > 0) {
                fn <- file.path(temp_dir, paste0("txt_plate_", i, ".xlsx"))
                writexl::write_xlsx(df, fn)
                files_to_zip <- c(files_to_zip, fn)
              }
            }
          }
        }

        # zip↔xlsx mapping
        if (!is.null(rv$processing_results$zip_xlsx_match) &&
            nrow(rv$processing_results$zip_xlsx_match) > 0) {
          writexl::write_xlsx(
            rv$processing_results$zip_xlsx_match,
            file.path(temp_dir, "zip_xlsx_match.xlsx")
          )
          files_to_zip <- c(files_to_zip, file.path(temp_dir, "zip_xlsx_match.xlsx"))
        }

        zip::zip(file, files_to_zip, mode = "cherry-pick")
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
