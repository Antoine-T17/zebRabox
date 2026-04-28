# ======================================================================
# modules/visualization_module.R
# Generic visualization module (TM/QM × LDM/VM) driven by config
# Migrated from plotly -> ggiraph (single interactive mode, no Output Mode)
# ======================================================================

# ---- Local themes -----------------------------------------------------
light_theme <- function(base_size = 11, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(color = "black", size = 14, hjust = .5),
      axis.text    = ggplot2::element_text(color = "black", size = 16),
      axis.title.x = ggplot2::element_text(color = "black", size = 16, margin = ggplot2::margin(t = 5, r = 15)),
      axis.title.y = ggplot2::element_text(color = "black", size = 16, angle = 90, margin = ggplot2::margin(r = 10)),
      legend.position = "right",
      legend.text  = ggplot2::element_text(color = "black", size = 16, face = "italic"),
      legend.title = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(size = 16),
      strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
      panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
      plot.caption = ggplot2::element_text(color = "black", size = 16, hjust = 1, margin = ggplot2::margin(t = 10)),
      panel.border = ggplot2::element_rect(color = "black", fill = NA)
    )
}

dark_theme <- function(base_size = 11, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(color = "white", size = 16, hjust = .5),
      axis.text    = ggplot2::element_text(color = "white", size = 16),
      axis.title.x = ggplot2::element_text(color = "white", size = 16, margin = ggplot2::margin(t = 5, r = 15)),
      axis.title.y = ggplot2::element_text(color = "white", size = 16, angle = 90, margin = ggplot2::margin(r = 10)),
      legend.position = "right",
      legend.text  = ggplot2::element_text(color = "white", size = 16, face = "italic"),
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = "black"),
      legend.key   = ggplot2::element_rect(fill = "black"),
      strip.text.x = ggplot2::element_text(color = "white", size = 16),
      strip.background = ggplot2::element_rect(fill = "black", color = "white"),
      plot.background  = ggplot2::element_rect(fill = "black"),
      panel.background = ggplot2::element_rect(fill = "black", colour = "white"),
      panel.border     = ggplot2::element_rect(color = "white", fill = NA),
      panel.grid.major = ggplot2::element_line(color = "grey30"),
      panel.grid.minor = ggplot2::element_line(color = "grey30"),
      plot.caption = ggplot2::element_text(color = "white", size = 16, hjust = 1, margin = ggplot2::margin(t = 10))
    )
}

# ---- UI ---------------------------------------------------------------
# ---- UI ---------------------------------------------------------------
visualization_module_ui <- function(id, config) {
  ns  <- shiny::NS(id)
  cfg <- if (is.function(config)) config() else config
  cond <- function(x, y) paste0("input['", ns(x), "'] == '", y, "'")

  shiny::tagList(
    # Ensure ggiraph stretches nicely inside shinydashboard boxes
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(
        sprintf("
          #%s .girafe_container, #%s .girafe_container svg { width:100%% !important; }
          #%s .girafe_container { overflow: visible; }
        ", id, id, id)
      ))
    ),

    shiny::fluidRow(
      shinydashboard::box(
        title = paste("Visualization Inputs —", cfg$ui_title), width = 4,

        # ==============================================================
        # SOURCE TABS ONLY: XLSX / TXT
        # ==============================================================
        shiny::tabsetPanel(
          id = ns("data_source_tabs"),

          # ---------------- XLSX ----------------
          shiny::tabPanel(
            title = "XLSX", value = "xlsx",

            shiny::selectInput(ns("plot_type"), "Plot Type",
                               c("boxplot_periods","boxplot_cumulate","boxplot_delta","lineplot"),
                               selected = "boxplot_periods"
            ),
            shiny::div(style = "margin-bottom:20px;"),
            shiny::selectInput(ns("response_var"), "Response Variable", choices = "", selected = ""),

            # --- Periods boxplots ---
            shiny::conditionalPanel(
              condition = cond("plot_type","boxplot_periods"),
              shiny::actionButton(ns("generate_periods_dfs"), paste0("Generate ", cfg$period_ui_name, " Datasets")),
              shiny::div(style = "margin-bottom:16px;"),

              shiny::radioButtons(
                ns("boxplot_periods_mode"), "Boxplot Mode",
                c("separated","pooled"), "separated", inline = TRUE
              ),

              shiny::textInput(
                ns("period_indices_keep"),
                "Keep only period indices (e.g. 1 or 1,2). Empty = keep all",
                value = ""
              ),

              shiny::conditionalPanel(
                condition = paste(cond("boxplot_periods_mode","pooled"), "&&", cond("plot_type","boxplot_periods")),
                shiny::textInput(
                  ns("boxplot_periods_colors"),
                  paste0(cfg$period_ui_name, " Colors (comma-separated hex)"),
                  value = cfg$period_default_colors
                )
              )
            ),

            # --- Cumulative ---
            shiny::conditionalPanel(
              condition = cond("plot_type","boxplot_cumulate"),
              shiny::actionButton(ns("generate_cumulate_dfs"), "Generate Cumulative Datasets"),
              shiny::div(style = "margin-bottom:16px;")
            ),

            # --- Delta ---
            shiny::conditionalPanel(
              condition = cond("plot_type","boxplot_delta"),
              shiny::uiOutput(ns("transition_select_ui")),
              shiny::sliderInput(ns("delta_time"), "Delta Time Window (seconds)",
                                 min = 5, max = 500, value = 60, step = 5, width = "100%"
              ),
              shiny::actionButton(ns("generate_delta_dfs"), "Generate Delta Datasets"),
              shiny::div(style = "margin-bottom:16px;"),
              shiny::radioButtons(ns("boxplot_delta_mode"), "Boxplot Mode",
                                  c("separated","pooled"), "separated", inline = TRUE
              ),
              shiny::conditionalPanel(
                condition = paste(cond("boxplot_delta_mode","pooled"), "&&", cond("plot_type","boxplot_delta")),
                shiny::textInput(
                  ns("boxplot_delta_phase_colors"),
                  "Phase Colors (Before, Switch, After; comma-separated hex)",
                  value = "#FF6F61, #40C4FF, #4CAF50"
                )
              )
            ),

            # --- Lineplot ---
            shiny::conditionalPanel(
              condition = cond("plot_type","lineplot"),
              shiny::radioButtons(ns("time_unit_convert"), "Convert Time Unit?",
                                  c("Yes","No"), "No", inline = TRUE
              ),
              shiny::conditionalPanel(
                condition = cond("time_unit_convert","Yes"),
                shiny::selectInput(ns("time_unit_original"), "Original Time Unit", c("seconds","minutes","hours","days"), "seconds"),
                shiny::selectInput(ns("time_unit_target"), "Target Time Unit",   c("seconds","minutes","hours","days"), "minutes")
              ),
              shiny::conditionalPanel(
                condition = cond("time_unit_convert","No"),
                shiny::selectInput(ns("time_unit_original"), "Time Unit", c("seconds","minutes","hours","days"), "seconds")
              ),
              shiny::uiOutput(ns("aggregation_period_label")),
              shiny::radioButtons(ns("lineplot_error_mode"), "Error Representation",
                                  c("Error Bars" = "error_bar", "95% CI" = "ci95"),
                                  "error_bar", inline = TRUE
              ),
              shiny::actionButton(ns("generate_lineplot_dfs"), "Generate Lineplot Datasets"),
              shiny::div(style = "margin-bottom:16px;")
            ),

            # --- Global ordering & colors ---
            shiny::textInput(ns("condition_grouped_order"), "Condition Order (comma-separated)",
                             value = "", placeholder = "e.g., cond1, cond2, cond3"
            ),
            shiny::textInput(ns("condition_grouped_color"), "Condition Colors (comma-separated hex)",
                             value = "", placeholder = "e.g., #FF0000, #00FF00, #0000FF"
            ),

            shiny::uiOutput(ns("conditions_filter_ui")),
            shiny::div(style = "margin-bottom:10px;"),

            shiny::uiOutput(ns("figure_selector")),
            shiny::div(style = "display:flex; flex-direction:column; gap:10px;",
                       shiny::actionButton(ns("generate_figure"), "Generate Figure", style = "width:100%;"),
                       shiny::conditionalPanel(
                         condition = cond("plot_type","boxplot_delta"),
                         shiny::actionButton(ns("generate_delta_tables"), "Generate Delta Percentage Tables", style = "width:100%;")
                       )
            )
          ),

          # ---------------- TXT ----------------
          # ---------------- TXT ----------------
          shiny::tabPanel(
            title = "TXT", value = "txt",

            shiny::selectInput(
              ns("txt_plot_type"),
              "TXT Plot Type",
              choices = c("trajectory_xy"),
              selected = "trajectory_xy"
            ),

            shiny::uiOutput(ns("txt_plate_ui")),
            shiny::uiOutput(ns("txt_conditions_ui")),
            shiny::div(
              style = "display:flex; gap:8px; margin-top:6px; margin-bottom:10px;",
              shiny::actionButton(ns("txt_conds_all"), "All",  style = "width:50%;"),
              shiny::actionButton(ns("txt_conds_none"), "None", style = "width:50%;")
            ),

            shiny::uiOutput(ns("txt_well_ui")),

            shiny::radioButtons(
              ns("txt_time_mode"),
              "Time selection mode",
              choices = c("Manual window" = "manual", "Period" = "period"),
              selected = "manual",
              inline = TRUE
            ),

            shiny::conditionalPanel(
              condition = paste0("input['", ns("txt_time_mode"), "'] == 'manual'"),
              shiny::fluidRow(
                shiny::column(
                  6,
                  shiny::numericInput(ns("txt_time_start"), "Start time (s)", value = 0, min = 0, step = 1)
                ),
                shiny::column(
                  6,
                  shiny::numericInput(ns("txt_time_end"), "End time (s)", value = 5, min = 0, step = 1)
                )
              )
            ),

            shiny::conditionalPanel(
              condition = paste0("input['", ns("txt_time_mode"), "'] == 'period'"),
              shiny::uiOutput(ns("txt_period_ui"))
            ),

            shiny::numericInput(
              ns("txt_target_points"),
              "Maximum displayed points per well (after averaging)",
              value = 250, min = 25, max = 5000, step = 25
            ),

            shiny::checkboxInput(
              ns("txt_show_points"),
              "Show averaged points",
              value = TRUE
            ),

            shiny::textOutput(ns("txt_selection_info")),
            shiny::textOutput(ns("txt_window_info")),
            shiny::textOutput(ns("txt_agg_info")),

            shiny::div(
              style = "display:flex; flex-direction:column; gap:10px; margin-top:10px;",
              shiny::actionButton(ns("generate_txt_dfs"), "Generate TXT Dataset"),
              shiny::actionButton(ns("generate_txt_figure"), "Generate Figure", style = "width:100%;")
            )
          )
        )
      ),

      # ------------------------------------------------------------------
      # Right output panel (inchangé)
      # ------------------------------------------------------------------
      shinydashboard::box(
        title = "Visualization Output", width = 8,
        shiny::div(style = "margin-bottom:10px;",
                   shiny::actionButton(ns("clear_console"), "Clear Console", icon = shiny::icon("trash"))
        ),
        shiny::tabsetPanel(
          id = ns("output_tabs"),

          shiny::tabPanel("Interactive Figure",
                          shinycssloaders::withSpinner(
                            ggiraph::girafeOutput(ns("girafe_plot"), width = "100%", height = "650px"),
                            type = 8,
                            color = "#339989",
                            size = 1.5,
                            proxy.height = "650px"
                          ),
                          shiny::div(style="margin-top:10px;"),
                          shiny::radioButtons(ns("theme_switch"), "Theme", c("Light","Dark"), "Light", inline = TRUE),
                          shiny::div(style="margin-top:10px;"),
                          shiny::downloadButton(ns("download_plot_script"), "Download R Script (.R)"),

                          shiny::conditionalPanel(
                            condition = cond("plot_type", "boxplot_delta"),
                            shiny::div(
                              style = "margin-top: 25px; border-top: 1px solid #ccc; padding-top: 15px;",
                              shiny::h5("Delta Time Explorer (Live Preview)", style = "text-align:center; margin-bottom:10px;"),
                              ggiraph::girafeOutput(ns("delta_time_explorer"), width = "100%", height = "320px"),
                              shiny::helpText(
                                "The delta time window defines three phases: Before [t-Δ,t), Switch [t,t+Δ), After [t+Δ,t+2Δ)",
                                style = "text-align:center; font-size:11px; margin-top:8px;"
                              )
                            )
                          )
          ),

          shiny::tabPanel("Datasets",
                          shiny::selectInput(ns("dataset_type"), "Dataset Type",
                                             c("Boxplot Periods","Boxplot Cumulative","Boxplot Delta","Lineplot"),
                                             selected = "Boxplot Periods"
                          ),
                          shiny::selectInput(ns("dataset_response_var"), "Response Variable", choices = "", selected = ""),
                          DT::dataTableOutput(ns("dataset_table")),
                          shiny::div(style="margin-top:10px; margin-bottom:10px;",
                                     shiny::downloadButton(ns("download_current_dataset"), "Download Current Dataset (.xlsx)")
                          )
          ),

          shiny::tabPanel("Console Output",
                          shiny::div(
                            style = "height: 600px; overflow-y: auto; padding: 10px; border-radius: 6px;",
                            class = "console-container",
                            shiny::verbatimTextOutput(ns("console_output"), placeholder = TRUE)
                          )
          ),

          shiny::tabPanel("Delta Percentage Tables", value = "delta_percentage_tables",
                          shiny::selectInput(ns("delta_table_type"), "Table Type",
                                             c("Momentum Comparisons","Condition Comparisons"), "Momentum Comparisons"
                          ),
                          shiny::selectInput(ns("delta_table_var"), "Response Variable", choices = "", selected = ""),
                          DT::dataTableOutput(ns("delta_percentage_table")),
                          shiny::div(style="margin-top:10px; margin-bottom:10px;",
                                     shiny::downloadButton(ns("download_current_delta_table"), "Download Current Table (.xlsx)"),
                                     shiny::downloadButton(ns("download_all_delta_tables"), "Download All Delta Tables (.zip)")
                          )
          )
        )
      )
    )
  )
}

# ---- Server -----------------------------------------------------------
visualization_module_server <- function(id, rv, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (is.null(a)) b else a

    cfg <- shiny::reactive({
      if (is.function(config)) tryCatch(config(), error = function(e) NULL) else config
    })
    EXPECTED_VARS <- shiny::reactive({ cfg()$expected_vars })

    console_messages <- shiny::reactiveVal("👋 Ready.")
    log <- function(...) {
      old <- shiny::isolate(console_messages())
      console_messages(c(old, paste(...)))
    }

    # Keep both last ggplot + last girafe widget
    rv$plot_gg     <- NULL
    rv$plot_girafe <- NULL

    convert_time <- function(x, from, to) {
      if (from == to) return(x)
      f <- c(seconds = 1, minutes = 60, hours = 3600, days = 86400)
      x * f[[from]] / f[[to]]
    }

    ensure_colors <- function(n, cols = character(0)) {
      cols <- cols[!is.na(cols) & nzchar(trimws(cols))]
      if (length(cols) == 0) {
        base <- tryCatch(RColorBrewer::brewer.pal(min(8, max(3, n)), "Set1"),
                         error = function(e) grDevices::rainbow(min(8, max(3, n)))
        )
        return(grDevices::colorRampPalette(base)(n))
      }
      cols <- trimws(cols)
      if (length(cols) < n) cols <- rep(cols, length.out = n)
      if (length(cols) > n) cols <- cols[seq_len(n)]
      cols
    }

    # ---- ggiraph wrapper (fills container + zoom + toolbar) ----
    to_girafe <- function(p, width_svg = 25, height_svg = 9) {
      ggiraph::girafe(ggobj = p, width_svg = width_svg, height_svg = height_svg) |>
        ggiraph::girafe_options(
          ggiraph::opts_sizing(rescale = TRUE),
          ggiraph::opts_toolbar(position = "topright", saveaspng = TRUE),
          ggiraph::opts_zoom(min = 0.5, max = 8)
        )
    }

    get_boundaries_list <- function() {
      if (is.null(rv$processing_results)) return(NULL)
      k <- names(rv$processing_results)
      k <- k[tolower(k) == "boundary_associations_list"]
      if (length(k)) rv$processing_results[[k[1]]] else NULL
    }

    update_response_choices <- function(vars, read_current = TRUE) {
      if (is.null(vars) || !length(vars)) return()
      rv_sel <- ""; ds_sel <- ""; dt_sel <- vars[1]
      if (isTRUE(read_current)) {
        if (!is.null(input$response_var) && isolate(input$response_var) %in% vars) rv_sel <- isolate(input$response_var)
        if (!is.null(input$dataset_response_var) && isolate(input$dataset_response_var) %in% vars) ds_sel <- isolate(input$dataset_response_var)
        if (!is.null(input$delta_table_var) && isolate(input$delta_table_var) %in% vars) dt_sel <- isolate(input$delta_table_var)
      }
      shiny::updateSelectInput(session, "response_var",         choices = c("", vars), selected = rv_sel)
      shiny::updateSelectInput(session, "dataset_response_var", choices = c("", vars), selected = ds_sel)
      shiny::updateSelectInput(session, "delta_table_var",      choices = c("", vars), selected = dt_sel)
    }

    shiny::observeEvent(cfg(), {
      vars <- EXPECTED_VARS(); if (is.null(vars)) return()
      update_response_choices(vars, read_current = FALSE)
    }, ignoreInit = FALSE)

    shiny::observeEvent(
      list(input$generate_periods_dfs, input$generate_cumulate_dfs, input$generate_delta_dfs, input$generate_lineplot_dfs),
      { vars <- EXPECTED_VARS(); if (is.null(vars)) return(); update_response_choices(vars, read_current = TRUE) },
      ignoreInit = TRUE
    )

    # ==================================================================
    # DATASET BUILDERS
    # ==================================================================
    prepare_all_zone <- function() {
      shiny::req(rv$processing_results, "processed_data_list" %in% names(rv$processing_results))
      mode  <- cfg()$mode %||% "unknown"
      is_qm <- grepl("^qm", mode)

      rv$processed_data_list <- purrr::map(
        rv$processing_results$processed_data_list,
        ~{
          .x <- dplyr::mutate(.x, plate_id = as.character(plate_id))

          if (all(c("smldist","lardist","inadist") %in% names(.x))) {
            .x <- .x |> dplyr::mutate(
              totaldist = smldist + lardist + inadist,
              totaldur  = smldur  + lardur  + inadur,
              totalct   = smlct   + larct   + inact,
              totalspeed = totaldist / pmax(totaldur, 1),
              smlspeed   = smldist / pmax(smldur, 1),
              larspeed   = lardist / pmax(lardur, 1),
              inaspeed   = inadist / pmax(inadur, 1)
            )
          }

          if (is_qm && all(c("frect","midct","burct","zerct") %in% names(.x))) {
            .x <- .x |> dplyr::mutate(
              totaldist  = frect + midct + burct + zerct,
              totaldur   = fredur + middur + burdur + zerdur,
              totalct    = 0,
              totalspeed = totaldist / pmax(totaldur, 1)
            )
          }
          .x
        }
      )

      rv$all_zone_combined <- dplyr::bind_rows(rv$processed_data_list)
      rv$all_zone_combined
    }

    build_periods_df <- function(az, v, cfg) {
      periods <- unique(az$period_without_numbers)
      keys    <- cfg$period_keys
      labels  <- cfg$period_labels %||% keys
      if (is.null(keys) || length(keys) == 0) stop("Configuration error: 'period_keys' is empty.")

      found_keys <- character(0)
      found_labels <- character(0)

      for (i in seq_along(keys)) {
        p_match <- grep(keys[i], periods, ignore.case = TRUE, value = TRUE)
        if (length(p_match) > 0) {
          found_keys   <- c(found_keys, p_match[1])
          found_labels <- c(found_labels, labels[i])
        }
      }

      if (length(found_keys) == 0) {
        stop(sprintf(
          "No periods matching any of %s found in data. Available: %s",
          paste("'", keys, "'", collapse = ", "),
          paste("'", periods, "'", collapse = ", ")
        ))
      }

      out <- az |>
        dplyr::filter(period_without_numbers %in% found_keys)

      idx_raw <- isolate(input$period_indices_keep)
      if (!is.null(idx_raw) && nzchar(trimws(idx_raw))) {
        idx_keep <- trimws(strsplit(idx_raw, ",")[[1]])
        idx_keep <- idx_keep[nzchar(idx_keep)]

        out <- out |>
          dplyr::mutate(
            .period_idx = sub(".*_([0-9]+)$", "\\1", as.character(period_with_numbers))
          ) |>
          dplyr::filter(.period_idx %in% idx_keep) |>
          dplyr::select(-.period_idx)
      }

      out |>
        dplyr::group_by(period_without_numbers, zone, condition_tagged, plate_id) |>
        dplyr::summarise(
          plate_id = dplyr::first(as.character(plate_id)),
          start = dplyr::first(start),
          period_with_numbers = dplyr::first(period_with_numbers),
          condition_grouped = dplyr::first(condition_grouped),
          condition = dplyr::first(condition),
          animal = dplyr::first(animal),
          mean_val = mean(.data[[v]], na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          period_without_numbers = factor(period_without_numbers, levels = found_keys, labels = found_labels)
        ) |>
        dplyr::select(dplyr::any_of(c(
          "zone", "condition_grouped", "condition_tagged", "condition",
          "start", "plate_id", "animal",
          "period_without_numbers", "period_with_numbers", "mean_val"
        )))
    }

    build_cumulate_df <- function(az, v) {
      az |>
        dplyr::group_by(condition_grouped, zone, plate_id, animal) |>
        dplyr::summarise(
          cum = sum(.data[[v]], na.rm = TRUE),
          condition_tagged = dplyr::first(condition_tagged),
          .groups = "drop"
        ) |>
        dplyr::mutate(plate_id = as.character(plate_id)) |>
        dplyr::select(dplyr::any_of(c(
          "zone","condition_grouped","condition_tagged","plate_id","animal","cum"
        )))
    }

    build_delta_split <- function(az, vars, transition, delta_sec, round_to = NULL) {
      shiny::req("boundary_associations_list" %in% names(rv$processing_results))
      b <- dplyr::bind_rows(get_boundaries_list()) |> dplyr::distinct()
      if (!nrow(b)) return(NULL)

      b_clean <- b |>
        dplyr::mutate(plate_id = as.character(plate_id)) |>
        dplyr::arrange(transition, plate_id, time_switch)|>
        dplyr::group_by(transition, plate_id) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup() |>
        dplyr::filter(transition == !!transition) |>
        dplyr::select(plate_id, time_switch)

      joined <- az |>
        dplyr::mutate(
          plate_id = as.character(plate_id),
          start_for_cut = if (!is.null(round_to)) floor(start / round_to) * round_to else start
        ) |>
        dplyr::inner_join(b_clean, by = "plate_id", relationship = "many-to-many") |>
        dplyr::mutate(
          phase_raw = dplyr::case_when(
            start_for_cut >= time_switch - delta_sec & start_for_cut < time_switch ~ "before",
            start_for_cut >= time_switch & start_for_cut < time_switch + delta_sec ~ "switch",
            start_for_cut >= time_switch + delta_sec & start_for_cut < time_switch + 2*delta_sec ~ "after",
            TRUE ~ NA_character_
          )
        ) |>
        dplyr::filter(!is.na(phase_raw)) |>
        dplyr::mutate(
          transition_phase = paste0(transition, "_", phase_raw),
          period_without_numbers = dplyr::recode(phase_raw, before = "Before", switch = "Switch", after = "After"),
          period_with_numbers = paste0(transition, "_", period_without_numbers)
        )

      if (!nrow(joined)) return(NULL)

      phased_long <- tidyr::pivot_longer(
        joined, cols = tidyselect::all_of(vars),
        names_to = "variable", values_to = "value"
      ) |>
        dplyr::group_by(
          transition_phase, period_without_numbers, period_with_numbers,
          zone, condition_tagged, plate_id, animal, variable
        ) |>
        dplyr::summarise(
          mean_val = mean(value, na.rm = TRUE),
          condition_grouped = dplyr::first(condition_grouped),
          start = dplyr::first(start),
          .groups = "drop"
        ) |>
        dplyr::select(dplyr::any_of(c(
          "zone","condition_grouped","condition_tagged",
          "plate_id","animal","mean_val",
          "period_without_numbers","period_with_numbers",
          "transition_phase","variable","start"
        )))

      split(phased_long, phased_long$variable)
    }

    build_lineplot_df <- function(az, v, agg_period, unit_from, unit_to, convert) {
      agg_unit <- if (identical(convert, "Yes")) unit_to else unit_from
      agg_s    <- convert_time(as.numeric(agg_period), agg_unit, "seconds")
      gvar     <- "condition_grouped"

      per_well <- az |>
        dplyr::mutate(
          plate_id = as.character(plate_id),
          start_rounded = floor(start / agg_s) * agg_s
        ) |>
        dplyr::group_by(.data[[gvar]], zone, start_rounded, plate_id, animal) |>
        dplyr::summarise(
          var_value_per_well = sum(.data[[v]], na.rm = TRUE),
          .groups = "drop"
        )

      summary_df <- per_well |>
        dplyr::group_by(.data[[gvar]], zone, start_rounded) |>
        dplyr::summarise(
          total_val     = sum(var_value_per_well, na.rm = TRUE),
          mean_per_well = mean(var_value_per_well, na.rm = TRUE),
          sd_per_well   = stats::sd(var_value_per_well, na.rm = TRUE),
          n_wells       = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::mutate(se_per_well = sd_per_well / sqrt(pmax(n_wells, 1)))

      per_well |>
        dplyr::left_join(summary_df, by = c(gvar, "zone", "start_rounded")) |>
        dplyr::mutate(
          start_rounded = if (!identical(unit_from, agg_unit))
            convert_time(start_rounded, "seconds", agg_unit) else start_rounded
        ) |>
        dplyr::rename(val_per_well = mean_per_well) |>
        dplyr::select(dplyr::any_of(c(
          "zone", gvar, "start_rounded",
          "plate_id","animal",
          "total_val","n_wells","val_per_well","se_per_well","sd_per_well"
        )))
    }

    # ==================================================================
    # UI bits
    # ==================================================================
    shiny::observeEvent(input$clear_console, {
      console_messages("👋 Ready.")
    })

    txt_base <- shiny::reactive({
      tryCatch(
        prepare_txt_spatial_base(),
        error = function(e) NULL
      )
    })

    txt_selected_wells <- shiny::reactive({
      df <- txt_base()
      if (is.null(df) || !nrow(df)) return(character(0))

      resolve_txt_selected_wells(
        df = df,
        plate_ids = input$txt_plate_select %||% character(0),
        selected_conditions = input$txt_condition_select %||% character(0),
        selected_wells = input$txt_well_select %||% character(0)
      )
    })

    txt_auto_bin <- shiny::reactive({
      df <- txt_base()
      wells <- txt_selected_wells()
      if (is.null(df) || !nrow(df) || !length(wells)) return(NULL)

      rng <- tryCatch(
        resolve_txt_time_range(
          df = df,
          well_keys = wells,
          mode = input$txt_time_mode,
          time_start = input$txt_time_start,
          time_end = input$txt_time_end,
          period_value = input$txt_period_select
        ),
        error = function(e) NULL
      )

      if (is.null(rng)) return(NULL)

      df <- add_txt_keys(df) |>
        dplyr::filter(
          well_key %in% wells,
          T >= rng[1],
          T <= rng[2]
        )

      if (!nrow(df)) return(NULL)

      native_dt <- estimate_txt_dt(df)
      window_len <- max(diff(rng), native_dt)

      target_points <- max(25, suppressWarnings(as.numeric(input$txt_target_points)))
      if (!is.finite(target_points)) target_points <- 250

      max(native_dt, window_len / target_points)
    })

    output$txt_plate_ui <- shiny::renderUI({
      df <- txt_base()
      if (is.null(df) || !nrow(df)) {
        return(shiny::helpText("No TXT data available."))
      }

      plates <- sort(unique(df$plate_id))

      shiny::selectizeInput(
        ns("txt_plate_select"),
        "Plate(s)",
        choices = plates,
        selected = plates[1],
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })

    output$txt_conditions_ui <- shiny::renderUI({
      df <- txt_base()
      if (is.null(df) || !nrow(df)) {
        return(shiny::helpText("No TXT data available."))
      }

      conds <- df |>
        dplyr::filter(!is.na(condition_grouped), condition_grouped != "") |>
        dplyr::distinct(condition_grouped) |>
        dplyr::arrange(condition_grouped) |>
        dplyr::pull(condition_grouped)

      shiny::tagList(
        shiny::div(style = "margin-top:10px; font-weight:600;", "Visible conditions"),
        shiny::checkboxGroupInput(
          ns("txt_condition_select"),
          NULL,
          choices = conds,
          selected = isolate(input$txt_condition_select) %||% conds
        )
      )
    })

    output$txt_well_ui <- shiny::renderUI({
      df <- txt_base()
      if (is.null(df) || !nrow(df)) {
        return(shiny::helpText("No TXT data available."))
      }

      shiny::req(input$txt_plate_select)

      sub <- add_txt_keys(df) |>
        dplyr::filter(plate_id %in% input$txt_plate_select)

      if (!is.null(input$txt_condition_select) && length(input$txt_condition_select)) {
        sub <- sub |>
          dplyr::filter(condition_grouped %in% input$txt_condition_select)
      }

      wells <- sub |>
        dplyr::distinct(plate_id, animal, well_key) |>
        dplyr::arrange(plate_id, animal) |>
        dplyr::mutate(label = paste0("Plate ", plate_id, " • ", animal))

      choices <- stats::setNames(wells$well_key, wells$label)
      current_sel <- isolate(input$txt_well_select) %||% character(0)
      selected <- intersect(current_sel, wells$well_key)

      shiny::selectizeInput(
        ns("txt_well_select"),
        "Well(s) (max 96, empty = use selected conditions)",
        choices = choices,
        selected = selected,
        multiple = TRUE,
        options = list(
          maxItems = 96,
          plugins = list("remove_button")
        )
      )
    })

    output$txt_period_ui <- shiny::renderUI({
      df <- txt_base()
      wells <- txt_selected_wells()

      if (is.null(df) || !nrow(df)) {
        return(shiny::helpText("No TXT data available."))
      }

      if (!length(wells)) {
        return(shiny::helpText("Select at least one well or one condition."))
      }

      df <- add_txt_keys(df) |>
        dplyr::filter(well_key %in% wells)

      periods <- df |>
        dplyr::filter(!is.na(period_with_numbers), period_with_numbers != "") |>
        dplyr::distinct(period_with_numbers) |>
        dplyr::arrange(period_with_numbers) |>
        dplyr::pull(period_with_numbers)

      if (!length(periods)) {
        return(shiny::helpText("No period_with_numbers values available for the current selection."))
      }

      shiny::selectInput(
        ns("txt_period_select"),
        "Period",
        choices = periods,
        selected = periods[1]
      )
    })

    output$txt_selection_info <- shiny::renderText({
      wells <- txt_selected_wells()
      if (!length(wells)) {
        return("No wells currently selected for display.")
      }
      paste0("Resolved wells to display: ", length(wells))
    })

    output$txt_window_info <- shiny::renderText({
      df <- txt_base()
      wells <- txt_selected_wells()

      if (is.null(df) || !nrow(df) || !length(wells)) return("")

      rng <- tryCatch(
        resolve_txt_time_range(
          df = df,
          well_keys = wells,
          mode = input$txt_time_mode,
          time_start = input$txt_time_start,
          time_end = input$txt_time_end,
          period_value = input$txt_period_select
        ),
        error = function(e) NULL
      )

      if (is.null(rng)) return("")

      paste0(
        "Resolved time window: ",
        format(round(rng[1], 2), nsmall = 2),
        " s → ",
        format(round(rng[2], 2), nsmall = 2),
        " s"
      )
    })

    output$txt_agg_info <- shiny::renderText({
      b <- txt_auto_bin()
      if (is.null(b)) return("")

      paste0(
        "Automatic temporal averaging bin: ",
        format(round(b, 3), nsmall = 3),
        " s"
      )
    })

    shiny::observeEvent(input$txt_conds_all, {
      df <- txt_base()
      if (is.null(df) || !nrow(df)) return()

      conds <- df |>
        dplyr::filter(!is.na(condition_grouped), condition_grouped != "") |>
        dplyr::distinct(condition_grouped) |>
        dplyr::arrange(condition_grouped) |>
        dplyr::pull(condition_grouped)

      shiny::updateCheckboxGroupInput(session, "txt_condition_select", selected = conds)
    })

    shiny::observeEvent(input$txt_conds_none, {
      shiny::updateCheckboxGroupInput(session, "txt_condition_select", selected = character(0))
    })

    output$txt_agg_info <- shiny::renderText({
      b <- txt_auto_bin()
      if (is.null(b)) return("")

      paste0(
        "Automatic temporal averaging bin: ",
        format(round(b, 3), nsmall = 3),
        " s"
      )
    })

    output$aggregation_period_label <- shiny::renderUI({
      unit <- if (input$time_unit_convert == "Yes") input$time_unit_target else input$time_unit_original
      shiny::textInput(ns("aggregation_period"), sprintf("Aggregation Period (in %s)", unit), value = "60")
    })

    output$transition_select_ui <- shiny::renderUI({
      shiny::req(rv$processing_results, get_boundaries_list())
      b  <- dplyr::bind_rows(get_boundaries_list()) |> dplyr::distinct()
      tr <- unique(b$transition)
      if (!length(tr)) return(shiny::div("No transitions available. Please run processing first."))
      shiny::selectInput(ns("transition_select"), "Select Transition", choices = tr, selected = tr[1])
    })

    # ---- Condition visibility filter (ggiraph replacement for plotly legend toggle)
    output$conditions_filter_ui <- shiny::renderUI({
      v  <- input$response_var
      pt <- input$plot_type

      df <- switch(
        pt,
        "boxplot_periods"  = rv$all_zone_combined_light_dark_boxplots[[v]],
        "boxplot_cumulate" = rv$all_zone_combined_cum_boxplots[[v]],
        "boxplot_delta"    = rv$all_zone_combined_delta_boxplots[[v]],
        "lineplot"         = rv$all_zone_combined_lineplots[[v]]
      )

      if (is.null(df) || is.null(v) || v == "") return(NULL)
      gvar <- "condition_grouped"
      if (!gvar %in% names(df)) return(NULL)

      conds <- sort(unique(as.character(df[[gvar]])))
      if (!length(conds)) return(NULL)

      shiny::tagList(
        shiny::div(style="margin-top:10px; font-weight:600;", "Visible conditions"),
        shiny::checkboxGroupInput(ns("visible_conditions"), NULL,
                                  choices = conds,
                                  selected = isolate(input$visible_conditions) %||% conds),
        shiny::div(style="display:flex; gap:8px; margin-top:6px;",
                   shiny::actionButton(ns("conds_all"),  "All",  style="width:50%;"),
                   shiny::actionButton(ns("conds_none"), "None", style="width:50%;"))
      )
    })

    shiny::observeEvent(input$conds_all, {
      v  <- isolate(input$response_var)
      pt <- isolate(input$plot_type)
      df <- switch(
        pt,
        "boxplot_periods"  = rv$all_zone_combined_light_dark_boxplots[[v]],
        "boxplot_cumulate" = rv$all_zone_combined_cum_boxplots[[v]],
        "boxplot_delta"    = rv$all_zone_combined_delta_boxplots[[v]],
        "lineplot"         = rv$all_zone_combined_lineplots[[v]]
      )
      if (!is.null(df) && "condition_grouped" %in% names(df)) {
        conds <- sort(unique(as.character(df$condition_grouped)))
        shiny::updateCheckboxGroupInput(session, "visible_conditions", selected = conds)
      }
    })

    shiny::observeEvent(input$conds_none, {
      shiny::updateCheckboxGroupInput(session, "visible_conditions", selected = character(0))
    })

    output$figure_selector <- shiny::renderUI({
      shiny::req(input$plot_type, input$response_var)
      df <- switch(
        input$plot_type,
        "boxplot_periods"  = rv$all_zone_combined_light_dark_boxplots[[input$response_var]],
        "boxplot_cumulate" = rv$all_zone_combined_cum_boxplots[[input$response_var]],
        "boxplot_delta"    = rv$all_zone_combined_delta_boxplots[[input$response_var]],
        "lineplot"         = rv$all_zone_combined_lineplots[[input$response_var]]
      )
      if (is.null(df)) return(shiny::helpText("Generate datasets first (click the button above)."))
      zones <- sort(unique(df$zone))
      choices <- stats::setNames(as.character(zones), paste("Zone", zones))
      shiny::selectInput(ns("selected_zone"), "Select Zone", choices = choices, selected = choices[1])
    })

    # ==================================================================
    # Delta Time Explorer (Live Preview) - ggplot + ggiraph
    # ==================================================================
    output$delta_time_explorer <- ggiraph::renderGirafe({
      if (is.null(input$transition_select) || input$transition_select == "") return(NULL)
      if (is.null(rv$all_zone_combined) || nrow(rv$all_zone_combined) == 0) return(NULL)
      if (is.null(get_boundaries_list())) return(NULL)
      if (is.null(input$response_var) || input$response_var == "") return(NULL)

      b <- dplyr::bind_rows(get_boundaries_list()) |> dplyr::distinct()
      ts_info <- b |>
        dplyr::filter(transition == input$transition_select) |>
        dplyr::group_by(plate_id) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup()
      if (!nrow(ts_info)) return(NULL)

      t_center <- mean(ts_info$time_switch, na.rm = TRUE)
      delta_sec <- suppressWarnings(as.numeric(input$delta_time))
      if (is.na(delta_sec) || delta_sec <= 0) delta_sec <- 60

      t1 <- t_center - delta_sec
      t2 <- t_center
      t3 <- t_center + delta_sec
      t4 <- t_center + 2 * delta_sec

      az <- rv$all_zone_combined
      v  <- input$response_var
      if (!v %in% names(az)) v <- names(az)[sapply(az, is.numeric)][1]

      line_df <- az |>
        dplyr::mutate(time_sec = floor(start)) |>
        dplyr::group_by(time_sec, condition_grouped) |>
        dplyr::summarise(mean_val = mean(.data[[v]], na.rm = TRUE), .groups = "drop")
      if (!nrow(line_df)) return(NULL)

      yr  <- range(line_df$mean_val, na.rm = TRUE)
      pad <- diff(yr) * 0.06
      ymin <- yr[1] - pad
      ymax <- yr[2] + pad

      theme_is_light <- tolower(input$theme_switch) == "light"
      theme_obj <- if (theme_is_light) ggplot2::theme_bw(base_size = 11) else dark_theme()
      edge_col  <- if (theme_is_light) "black" else "white"

      bands <- data.frame(
        x0 = c(t1, t2, t3),
        x1 = c(t2, t3, t4),
        phase = c("Before","Switch","After")
      )

      p <- ggplot2::ggplot(line_df, ggplot2::aes(x = time_sec, y = mean_val, colour = condition_grouped, group = condition_grouped)) +
        ggplot2::geom_rect(
          data = bands,
          ggplot2::aes(xmin = x0, xmax = x1, ymin = ymin, ymax = ymax, fill = phase),
          inherit.aes = FALSE, alpha = 0.12, colour = NA
        ) +
        ggplot2::geom_vline(xintercept = c(t1,t2,t3,t4), linetype = "dotted", linewidth = 0.6, colour = edge_col, alpha = 0.8) +
        ggplot2::annotate("text", x = (t1+t2)/2, y = ymax, label = "B", vjust = 1.2, colour = edge_col, size = 4) +
        ggplot2::annotate("text", x = (t2+t3)/2, y = ymax, label = "S", vjust = 1.2, colour = edge_col, size = 4) +
        ggplot2::annotate("text", x = (t3+t4)/2, y = ymax, label = "A", vjust = 1.2, colour = edge_col, size = 4) +
        ggplot2::geom_line(linewidth = 0.7, alpha = 0.9) +
        ggiraph::geom_point_interactive(
          ggplot2::aes(
            tooltip = paste0("t=", time_sec, "s\n", condition_grouped, "\nmean=", sprintf("%.3f", mean_val)),
            data_id = paste0(condition_grouped, "_", time_sec)
          ),
          size = 1.8, alpha = 0.8, colour = edge_col
        ) +
        ggplot2::scale_fill_manual(values = c(Before = "#FF9800", Switch = "#F44336", After = "#4CAF50")) +
        ggplot2::labs(x = "Time (s)", y = paste("Mean", v)) +
        theme_obj +
        ggplot2::theme(
          legend.position = "right",
          panel.grid.major = ggplot2::element_line(colour = scales::alpha(edge_col, 0.30)),
          panel.grid.minor = ggplot2::element_line(colour = scales::alpha(edge_col, 0.20))
        )

      to_girafe(p, width_svg = 12, height_svg = 9)
    })

    # ==================================================================
    # Dataset generators
    # ==================================================================
    shiny::observeEvent(input$generate_periods_dfs, {
      tryCatch({
        log("⏳ Generating periods datasets...")

        shiny::withProgress(message = "Generating periods datasets...", value = 0, {
          incProgress(0.2)

          az <- prepare_all_zone()
          current_cfg <- cfg()
          incProgress(0.6)

          rv$all_zone_combined_light_dark_boxplots <- stats::setNames(
            lapply(EXPECTED_VARS(), function(v) build_periods_df(az, v, current_cfg)),
            EXPECTED_VARS()
          )

          incProgress(1)
        })

        log("✅ Periods datasets created.")
      }, error = function(e) {
        log(paste("❌ Periods dataset generation failed:", e$message))
      })
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$period_indices_keep, {
      shiny::req(!is.null(rv$all_zone_combined_light_dark_boxplots))
      tryCatch({
        az <- prepare_all_zone()
        current_cfg <- cfg()

        rv$all_zone_combined_light_dark_boxplots <- stats::setNames(
          lapply(EXPECTED_VARS(), function(v) build_periods_df(az, v, current_cfg)),
          EXPECTED_VARS()
        )
        log("✅ Periods datasets updated (indices filter).")
        if (isolate(input$plot_type) == "boxplot_periods" &&
            !is.null(isolate(input$response_var)) && nzchar(isolate(input$response_var))) {
          make_plot(log_it = FALSE)
        }
      }, error = function(e) log(paste("❌ Periods update failed:", e$message)))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$generate_cumulate_dfs, {
      tryCatch({
        log("⏳ Generating cumulative datasets...")

        shiny::withProgress(message = "Generating cumulative datasets...", value = 0, {
          incProgress(0.3)

          az <- prepare_all_zone()
          incProgress(0.7)

          rv$all_zone_combined_cum_boxplots <- stats::setNames(
            lapply(EXPECTED_VARS(), function(v) build_cumulate_df(az, v)),
            EXPECTED_VARS()
          )

          incProgress(1)
        })

        log("✅ Cumulative datasets created.")
      }, error = function(e) {
        log(paste("❌ Cumulative generation failed:", e$message))
      })
    })

    validate_num_pos <- function(x, msg) {
      v <- suppressWarnings(as.numeric(x))
      if (is.na(v) || v <= 0) {
        shiny::showModal(shiny::modalDialog(title = "Warning", msg, easyClose = TRUE))
        return(FALSE)
      }
      TRUE
    }

    shiny::observeEvent(input$generate_delta_dfs, {
      tryCatch({
        log("⏳ Generating delta datasets...")

        shiny::withProgress(message = "Generating delta datasets...", value = 0, {
          shiny::req(validate_num_pos(input$delta_time, "Delta time window must be a positive number."))
          incProgress(0.2)

          az <- prepare_all_zone()
          delta_sec <- as.numeric(input$delta_time)
          incProgress(0.7)

          split_list <- build_delta_split(az, EXPECTED_VARS(), input$transition_select, delta_sec)
          rv$all_zone_combined_delta_boxplots <- split_list

          incProgress(1)
        })

        log(sprintf("✅ Delta datasets created for transition '%s' (±%ss).", input$transition_select, as.numeric(input$delta_time)))
      }, error = function(e) {
        log(paste("❌ Delta generation failed:", e$message))
      })
    })

    shiny::observeEvent(input$generate_lineplot_dfs, {
      tryCatch({
        log("⏳ Generating lineplot datasets...")

        shiny::withProgress(message = "Generating lineplot datasets...", value = 0, {
          shiny::req(validate_num_pos(input$aggregation_period, "Aggregation period must be a positive number."))
          incProgress(0.2)

          az <- prepare_all_zone()
          incProgress(0.6)

          rv$all_zone_combined_lineplots <- stats::setNames(
            lapply(EXPECTED_VARS(), function(v) {
              build_lineplot_df(
                az = az, v = v,
                agg_period = input$aggregation_period,
                unit_from  = input$time_unit_original,
                unit_to    = input$time_unit_target,
                convert    = input$time_unit_convert
              )
            }),
            EXPECTED_VARS()
          )

          incProgress(1)
        })

        log("✅ Lineplot datasets created (normalized per well, pooled).")
      }, error = function(e) {
        log(paste("❌ Lineplot generation failed:", e$message))
      })
    })

    shiny::observeEvent(input$generate_txt_dfs, {
      tryCatch({
        log("⏳ Generating TXT dataset...")

        shiny::withProgress(message = "Generating TXT dataset...", value = 0, {
          wells <- txt_selected_wells()
          shiny::req(length(wells) > 0)
          incProgress(0.2)

          rng <- resolve_txt_time_range(
            df = txt_base(),
            well_keys = wells,
            mode = input$txt_time_mode,
            time_start = input$txt_time_start,
            time_end = input$txt_time_end,
            period_value = input$txt_period_select
          )
          incProgress(0.6)

          rv$txt_spatial_current <- build_txt_trajectory_df(
            df = txt_base(),
            well_keys = wells,
            time_range = rng,
            target_points = input$txt_target_points
          )

          incProgress(1)
        })

        log(sprintf(
          "✅ TXT spatial dataset created (%s rows after aggregation, bin = %.3fs).",
          nrow(rv$txt_spatial_current),
          attr(rv$txt_spatial_current, "bin_s")
        ))
      }, error = function(e) {
        log(paste("❌ TXT dataset generation failed:", e$message))
      })
    }, ignoreInit = TRUE)

    # ==================================================================
    # TXT spatial helpers
    # ==================================================================
    add_txt_keys <- function(df) {
      dplyr::mutate(df, well_key = paste(plate_id, animal, sep = "__"))
    }

    get_txt_source_list <- function() {
      shiny::req(rv$processing_results)
      pr <- rv$processing_results

      if ("txt_all" %in% names(pr)) {
        if (inherits(pr$txt_all, "data.frame") &&
            nrow(pr$txt_all) > 0 &&
            all(c("T", "X", "Y", "animal", "plate_id") %in% names(pr$txt_all))) {
          return(list(pr$txt_all))
        }
      }

      if ("txt_by_plate" %in% names(pr)) {
        obj <- pr$txt_by_plate
        if (is.list(obj) && length(obj) > 0) {
          ok <- purrr::map_lgl(obj, function(.x) {
            inherits(.x, "data.frame") &&
              nrow(.x) > 0 &&
              all(c("T", "X", "Y", "animal", "plate_id") %in% names(.x))
          })
          if (any(ok)) return(obj[ok])
        }
      }

      stop("No TXT processed dataset with columns T, X, Y, animal and plate_id was found in rv$processing_results.")
    }

    prepare_txt_spatial_base <- function() {
      lst <- get_txt_source_list()

      dplyr::bind_rows(lst, .id = "txt_source_id") |>
        dplyr::mutate(
          T = suppressWarnings(as.numeric(T)),
          X = suppressWarnings(as.numeric(X)),
          Y = suppressWarnings(as.numeric(Y)),
          plate_id = as.character(plate_id),
          animal = as.character(animal),
          file_txt_name = as.character(file_txt_name),
          condition = as.character(condition),
          condition_grouped = as.character(condition_grouped),
          condition_tagged = as.character(condition_tagged),
          period_with_numbers = as.character(period_with_numbers),
          period_without_numbers = as.character(period_without_numbers),
          zone = as.character(zone)
        ) |>
        dplyr::filter(is.finite(T), is.finite(X), is.finite(Y)) |>
        dplyr::arrange(plate_id, animal, T)
    }

    estimate_txt_dt <- function(df) {
      dt <- df |>
        dplyr::summarise(dt = stats::median(diff(sort(unique(T))), na.rm = TRUE)) |>
        dplyr::pull(dt)

      if (!is.finite(dt) || dt <= 0) dt <- 1 / 25
      dt
    }

    resolve_txt_selected_wells <- function(df, plate_ids = NULL, selected_conditions = character(0), selected_wells = character(0)) {
      df <- add_txt_keys(df)

      if (!is.null(plate_ids) && length(plate_ids)) {
        df <- df |>
          dplyr::filter(plate_id %in% plate_ids)
      }

      if (!nrow(df)) return(character(0))

      if (!is.null(selected_conditions) && length(selected_conditions)) {
        df <- df |>
          dplyr::filter(condition_grouped %in% selected_conditions)
      }

      available_wells <- unique(df$well_key)

      if (!is.null(selected_wells) && length(selected_wells)) {
        return(intersect(selected_wells, available_wells))
      }

      if (!is.null(selected_conditions) && length(selected_conditions)) {
        return(available_wells)
      }

      character(0)
    }

    resolve_txt_time_range <- function(df, well_keys, mode, time_start = NULL, time_end = NULL, period_value = NULL) {
      df <- add_txt_keys(df)

      sub <- df |>
        dplyr::filter(well_key %in% well_keys)

      if (!nrow(sub)) stop("No TXT data for selected wells.")

      if (identical(mode, "manual")) {
        tmin <- suppressWarnings(as.numeric(time_start))
        tmax <- suppressWarnings(as.numeric(time_end))

        if (!is.finite(tmin) || !is.finite(tmax) || tmax <= tmin) {
          stop("Invalid manual time window. End time must be greater than start time.")
        }

        return(c(tmin, tmax))
      }

      if (identical(mode, "period")) {
        if (is.null(period_value) || !nzchar(period_value)) {
          stop("Please select a period.")
        }

        subp <- sub |>
          dplyr::filter(period_with_numbers == period_value)

        if (!nrow(subp)) {
          stop("No TXT data found for the selected period.")
        }

        return(range(subp$T, na.rm = TRUE))
      }

      stop("Unknown TXT time mode.")
    }

    build_txt_trajectory_df <- function(df, well_keys, time_range, target_points = 100) {
      shiny::req(length(well_keys) > 0, length(time_range) == 2)

      if (length(unique(well_keys)) > 96) {
        stop("Please select at most 96 wells for TXT trajectory visualization.")
      }

      df <- add_txt_keys(df)

      tmin <- min(time_range)
      tmax <- max(time_range)

      sub <- df |>
        dplyr::filter(
          well_key %in% well_keys,
          T >= tmin,
          T <= tmax
        ) |>
        dplyr::filter(!(X == 0 & Y == 0))

      if (!nrow(sub)) {
        stop("No TXT data available for the selected wells / time window.")
      }

      native_dt <- estimate_txt_dt(sub)
      window_len <- max(tmax - tmin, native_dt)

      target_points <- max(25, suppressWarnings(as.numeric(target_points)))
      if (!is.finite(target_points)) target_points <- 250

      bin_s <- max(native_dt, window_len / target_points)

      out <- sub |>
        dplyr::mutate(
          time_bin = floor((T - tmin) / bin_s),
          well_id = animal,
          condition_grouped = dplyr::if_else(
            is.na(condition_grouped) | condition_grouped == "",
            "Unknown",
            condition_grouped
          )
        ) |>
        dplyr::group_by(
          plate_id,
          animal, well_key, well_id,
          condition_grouped, condition_tagged, zone,
          time_bin
        ) |>
        dplyr::summarise(
          T = mean(T, na.rm = TRUE),
          X = mean(X, na.rm = TRUE),
          Y = mean(Y, na.rm = TRUE),
          n_raw = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::filter(
          is.finite(X), is.finite(Y),
          !(X == 0 & Y == 0)
        ) |>
        dplyr::arrange(condition_grouped, plate_id, animal, T) |>
        dplyr::group_by(plate_id, animal) |>
        dplyr::mutate(
          point_id = paste0("traj_", plate_id, "_", animal, "_", dplyr::row_number()),
          path_id  = paste0("path_", plate_id, "_", animal),
          tooltip  = paste0(
            "Plate: ", plate_id,
            "<br>Well: ", animal,
            "<br>Condition: ", condition_grouped,
            "<br>t = ", sprintf("%.2f", T), " s",
            "<br>X = ", sprintf("%.1f", X),
            "<br>Y = ", sprintf("%.1f", Y),
            "<br>Mean of ", n_raw, " raw points"
          )
        ) |>
        dplyr::ungroup()

      attr(out, "bin_s") <- bin_s
      attr(out, "native_dt") <- native_dt
      attr(out, "time_range") <- c(tmin, tmax)

      out
    }

    generate_txt_plot <- function(df, theme_choice, show_points = TRUE) {
      theme_is_light <- tolower(theme_choice) == "light"
      theme_obj <- if (theme_is_light) light_theme() else dark_theme()
      edge_col  <- if (theme_is_light) "black" else "white"

      cond_levels <- sort(unique(as.character(df$condition_grouped)))
      cols <- ensure_colors(length(cond_levels))
      names(cols) <- cond_levels

      start_df <- df |>
        dplyr::group_by(plate_id, animal) |>
        dplyr::slice_min(T, n = 1, with_ties = FALSE) |>
        dplyr::ungroup()

      end_df <- df |>
        dplyr::group_by(plate_id, animal) |>
        dplyr::slice_max(T, n = 1, with_ties = FALSE) |>
        dplyr::ungroup()

      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = X, y = Y,
          group = interaction(plate_id, animal),
          colour = condition_grouped
        )
      ) +
        ggiraph::geom_path_interactive(
          ggplot2::aes(
            data_id = path_id,
            tooltip = tooltip
          ),
          linewidth = 0.7,
          alpha = 0.85,
          hover_css = "stroke-width:2.2px!important;opacity:1!important;"
        )

      if (isTRUE(show_points)) {
        p <- p +
          ggiraph::geom_point_interactive(
            ggplot2::aes(
              tooltip = tooltip,
              data_id = point_id
            ),
            size = 1.3,
            alpha = 0.65,
            hover_css = "r:4!important;opacity:1!important;"
          )
      }

      p +
        ggplot2::geom_point(
          data = start_df,
          ggplot2::aes(x = X, y = Y),
          inherit.aes = FALSE,
          shape = 21,
          fill = "white",
          colour = edge_col,
          stroke = 0.9,
          size = 2.5
        ) +
        ggplot2::geom_point(
          data = end_df,
          ggplot2::aes(x = X, y = Y),
          inherit.aes = FALSE,
          shape = 4,
          colour = edge_col,
          stroke = 1.1,
          size = 2.8
        ) +
        ggplot2::scale_colour_manual(values = cols, limits = cond_levels, name = "Condition") +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.03, 0.03))) +
        ggplot2::coord_equal() +
        ggplot2::facet_wrap(~condition_grouped, ncol = 1, scales = "free_y") +
        ggplot2::labs(
          x = "X",
          y = "Y",
          caption = "Open circle = start of the selected window; cross = end."
        ) +
        theme_obj +
        ggplot2::theme(
          plot.caption.position = "plot",
          plot.caption = ggplot2::element_text(hjust = 1),
          legend.position = "none"
        )
    }

    # ==================================================================
    # Plot factory
    # - Points (except lineplot) use sina distribution + interactive tooltips
    # ==================================================================
    order_and_colors <- function(df) {
      gvar <- "condition_grouped"
      present <- unique(df[[gvar]])
      if (!is.null(input$visible_conditions) && length(input$visible_conditions)) {
        present <- intersect(present, input$visible_conditions)
      }

      ord_in <- if (nzchar(input$condition_grouped_order))
        trimws(strsplit(input$condition_grouped_order, ",")[[1]]) else present
      ord <- unique(c(intersect(ord_in, present), setdiff(present, ord_in)))

      raw_cols <- if (nzchar(input$condition_grouped_color))
        trimws(strsplit(input$condition_grouped_color, ",")[[1]]) else character(0)

      cols <- ensure_colors(length(ord), raw_cols)
      names(cols) <- ord
      list(order = ord, colors = cols)
    }

    add_sina_with_tooltip <- function(
    p, data, aes_x, aes_y, tooltip_expr,
    size = 1.6, alpha = 0.55,
    edge_col = "black",
    sina_maxwidth = 0.5,
    seed = 1,
    hover_css = "r:6!important;opacity:1!important;fill-opacity:1!important;stroke-opacity:1!important;"
    ) {
      if (is.null(data) || !nrow(data)) return(p)

      data <- dplyr::mutate(data, .gid = paste0("id_", dplyr::row_number()))

      p +
        ggiraph::geom_point_interactive(
          data = data,
          ggplot2::aes(
            x = {{ aes_x }}, y = {{ aes_y }},
            tooltip = !!tooltip_expr,
            data_id = .gid,
            group = {{ aes_x }}
          ),
          stat = ggforce::StatSina,
          maxwidth = sina_maxwidth, seed = seed,
          shape = 16, size = size, alpha = alpha, colour = edge_col,
          hover_css = hover_css
        )
    }

    add_boxplot_hover <- function(
    p, data, x_col, y_col,
    facet_col = NULL,
    dodge_col = NULL,
    x_order = NULL,
    dodge_levels = NULL,
    tooltip_digits = 2,
    hover_css = "fill:transparent!important;stroke:transparent!important;opacity:1!important;"
    ) {
      if (is.null(data) || !nrow(data)) return(p)
      d <- data[!is.na(data[[x_col]]) & !is.na(data[[y_col]]), , drop = FALSE]
      if (!nrow(d)) return(p)

      if (!is.null(dodge_col)) {
        gb <- ggplot2::ggplot_build(p)
        li <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomBoxplot"), logical(1)))[1]
        if (is.na(li) || !length(li)) return(p)
        ld <- gb$data[[li]]
        if (is.null(ld) || !nrow(ld)) return(p)

        if (is.null(x_order)) x_order <- levels(d[[x_col]])
        if (is.null(dodge_levels)) dodge_levels <- unique(as.character(d[[dodge_col]]))

        ld$x_base <- round(ld$x)
        ld$cond   <- x_order[pmax(1, pmin(length(x_order), ld$x_base))]
        ld$dodge_idx <- ave(ld$x, ld$x_base, FUN = function(z) rank(z, ties.method = "first"))
        ld$dodge  <- dodge_levels[pmax(1, pmin(length(dodge_levels), ld$dodge_idx))]

        mean_df <- dplyr::summarise(
          dplyr::group_by(d, .data[[x_col]], .data[[dodge_col]]),
          mean = mean(.data[[y_col]], na.rm = TRUE),
          .groups = "drop"
        )
        key_m <- paste(as.character(mean_df[[x_col]]), as.character(mean_df[[dodge_col]]), sep = "||")
        key_l <- paste(ld$cond, ld$dodge, sep = "||")
        ld$mean <- mean_df$mean[match(key_l, key_m)]

        rect_df <- data.frame(
          xmin = ld$xmin, xmax = ld$xmax,
          ymin = ld$lower, ymax = ld$upper,
          mean = ld$mean, median = ld$middle,
          .id = paste0("box_", seq_len(nrow(ld))),
          stringsAsFactors = FALSE
        )
        rect_df$tooltip <- paste0(
          "Mean: ", sprintf(paste0("%.", tooltip_digits, "f"), rect_df$mean),
          "<br>Median: ", sprintf(paste0("%.", tooltip_digits, "f"), rect_df$median)
        )

        return(
          p + ggiraph::geom_rect_interactive(
            data = rect_df,
            ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                         tooltip = tooltip, data_id = .id),
            inherit.aes = FALSE,
            fill = "transparent", colour = "transparent",
            hover_css = hover_css, show.legend = FALSE
          )
        )
      }

      grp <- c(facet_col, x_col)
      grp <- grp[!is.null(grp) & nzchar(grp)]

      stat_df <- dplyr::summarise(
        dplyr::group_by(d, dplyr::across(dplyr::all_of(grp))),
        mean = mean(.data[[y_col]], na.rm = TRUE),
        .stats = list(grDevices::boxplot.stats(.data[[y_col]])$stats),
        .groups = "drop"
      )
      stat_df$lower  <- vapply(stat_df$.stats, `[`, numeric(1), 2)
      stat_df$median <- vapply(stat_df$.stats, `[`, numeric(1), 3)
      stat_df$upper  <- vapply(stat_df$.stats, `[`, numeric(1), 4)
      stat_df$.stats <- NULL

      if (!is.null(facet_col)) {
        stat_df <- dplyr::group_modify(
          dplyr::group_by(stat_df, .data[[facet_col]]),
          ~{
            lev <- if (!is.null(x_order)) x_order[x_order %in% unique(as.character(.x[[x_col]]))] else unique(as.character(.x[[x_col]]))
            .x$x_center <- match(as.character(.x[[x_col]]), lev); .x
          }
        ) |> dplyr::ungroup()
      } else {
        lev <- if (!is.null(x_order)) x_order[x_order %in% unique(as.character(stat_df[[x_col]]))] else unique(as.character(stat_df[[x_col]]))
        stat_df$x_center <- match(as.character(stat_df[[x_col]]), lev)
      }

      rect_df <- dplyr::mutate(
        stat_df,
        xmin = x_center - 0.75/2, xmax = x_center + 0.75/2,
        .id = paste0("box_", dplyr::row_number()),
        tooltip = paste0(
          "Mean: ", sprintf(paste0("%.", tooltip_digits, "f"), mean),
          "<br>Median: ", sprintf(paste0("%.", tooltip_digits, "f"), median)
        )
      )

      p + ggiraph::geom_rect_interactive(
        data = rect_df,
        ggplot2::aes(xmin = xmin, xmax = xmax, ymin = lower, ymax = upper,
                     tooltip = tooltip, data_id = .id),
        inherit.aes = FALSE,
        fill = "transparent", colour = "transparent",
        hover_css = hover_css, show.legend = FALSE
      )
    }

    # ==================================================================
    # Generate Plot function
    # ==================================================================
    generate_plot <- function(df, response_var, plot_type, boxplot_mode,
                              selected_zone, theme_choice, condition_order, condition_colors) {

      sub <- droplevels(subset(df, zone == selected_zone))

      vis <- input$visible_conditions %||% character(0)
      if (!length(vis)) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0, y = 0, label = "No condition selected") +
                 ggplot2::theme_void())
      }
      sub <- sub[sub$condition_grouped %in% vis, , drop = FALSE]
      if (!nrow(sub)) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0, y = 0, label = "No data after filtering") +
                 ggplot2::theme_void())
      }

      theme_is_light <- tolower(theme_choice) == "light"
      theme_obj <- if (theme_is_light) light_theme() else dark_theme()
      edge_col  <- if (theme_is_light) "black" else "white"

      hover_css_pts <- "r:6!important;opacity:1!important;fill-opacity:1!important;stroke-opacity:1!important;"
      hover_css_box <- "fill:transparent!important;stroke:transparent!important;opacity:1!important;"

      gvar <- "condition_grouped"
      present_levels <- unique(sub[[gvar]])
      ord <- intersect(condition_order, present_levels); if (!length(ord)) ord <- present_levels
      ord <- unique(c(ord, setdiff(present_levels, ord)))
      sub[[gvar]] <- factor(sub[[gvar]], levels = ord)

      cols_named <- condition_colors
      if (is.null(names(cols_named)) || length(cols_named) != length(ord) || any(!(ord %in% names(cols_named)))) {
        cols_named <- ensure_colors(length(ord), cols_named); names(cols_named) <- ord
      }

      pt_size <- 2.3; pt_alpha <- 0.65; jit_w <- 0.18; box_lwd <- 0.55; sina_maxw <- 0.25
      box_w <- 0.60
      x_pad <- 0.60
      dodge_w    <- 0.65
      dodge_pos  <- ggplot2::position_dodge(width = dodge_w)
      point_pos  <- ggplot2::position_jitterdodge(dodge.width = dodge_w, jitter.width = jit_w)

      sub$.row_id <- paste0("r", seq_len(nrow(sub)))

      if (plot_type == "boxplot_periods") {

        if (identical(boxplot_mode, "separated")) {

          tooltip_pts <- rlang::expr(paste0(
            "Animal: ", animal,
            "<br>Plate: ", plate_id,
            "<br>Condition: ", .data[[gvar]],
            "<br>Value: ", sprintf("%.2f", mean_val)
          ))

          gg <- ggplot2::ggplot(sub, ggplot2::aes(x = .data[[gvar]], y = mean_val)) +
            ggplot2::geom_boxplot(ggplot2::aes(group = .data[[gvar]], fill = .data[[gvar]]),
                                  colour = edge_col, linewidth = box_lwd, width = box_w)

          gg <- add_boxplot_hover(gg, sub, x_col = gvar, y_col = "mean_val",
                                  facet_col = "period_without_numbers", x_order = ord, hover_css = hover_css_box)

          gg <- add_sina_with_tooltip(gg, sub, aes_x = .data[[gvar]], aes_y = mean_val,
                                      tooltip_expr = tooltip_pts, size = pt_size, alpha = pt_alpha,
                                      edge_col = edge_col, sina_maxwidth = sina_maxw, hover_css = hover_css_pts)

          return(
            gg +
              ggplot2::facet_wrap(~period_without_numbers, scales = "free_x") +
              ggplot2::scale_fill_manual(values = cols_named, limits = ord) +
              ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = x_pad)) +
              ggplot2::labs(y = sprintf("%s (Zone %s)", response_var, selected_zone),
                            caption = "Each point corresponds to the mean value for one animal.") +
              theme_obj +
              ggplot2::theme(legend.position = "none",
                             axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                             axis.title.x = ggplot2::element_blank(),
                             plot.caption.position = "plot",
                             plot.caption = ggplot2::element_text(hjust = 1))
          )
        }

        per_cols_in <- trimws(strsplit(input$boxplot_periods_colors, ",")[[1]])
        per_levels <- levels(sub$period_without_numbers)
        if (is.null(per_levels) || !length(per_levels)) {
          per_levels <- sort(unique(as.character(sub$period_without_numbers)))
        }

        per_cols_in <- trimws(strsplit(input$boxplot_periods_colors, ",")[[1]])
        per_cols    <- ensure_colors(length(per_levels), per_cols_in)
        names(per_cols) <- per_levels

        tooltip_pts <- rlang::expr(paste0(
          "Animal: ", animal,
          "<br>Plate: ", plate_id,
          "<br>Condition: ", .data[[gvar]],
          "<br>Period: ", period_without_numbers,
          "<br>Value: ", sprintf("%.2f", mean_val)
        ))

        gg <- ggplot2::ggplot(sub, ggplot2::aes(x = .data[[gvar]], y = mean_val)) +
          ggplot2::geom_boxplot(
            ggplot2::aes(group = interaction(.data[[gvar]], period_without_numbers), fill = period_without_numbers),
            colour = edge_col, linewidth = box_lwd, width = box_w, position = dodge_pos
          )

        gg <- add_boxplot_hover(gg, sub, x_col = gvar, y_col = "mean_val",
                                dodge_col = "period_without_numbers", x_order = ord,
                                dodge_levels = per_levels, hover_css = hover_css_box)

        return(
          gg +
            ggiraph::geom_point_interactive(
              ggplot2::aes(tooltip = !!tooltip_pts, data_id = .row_id,
                           group = interaction(.data[[gvar]], period_without_numbers),
                           colour = period_without_numbers),
              position = point_pos,
              size = pt_size, alpha = pt_alpha, hover_css = hover_css_pts
            ) +
            ggplot2::scale_fill_manual(values = per_cols, breaks = per_levels, limits = per_levels, name = "Period") +
            ggplot2::scale_colour_manual(values = rep(edge_col, length(per_levels)), guide = "none") +
            ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = x_pad)) +
            ggplot2::labs(y = sprintf("%s (Zone %s)", response_var, selected_zone),
                          caption = "Each point corresponds to the mean value for one animal.") +
            theme_obj +
            ggplot2::theme(legend.position = "right",
                           axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                           axis.title.x = ggplot2::element_blank(),
                           plot.caption.position = "plot",
                           plot.caption = ggplot2::element_text(hjust = 1))
        )
      }

      if (plot_type == "boxplot_cumulate") {

        n_animals <- sub |>
          dplyr::group_by(.data[[gvar]], zone, plate_id) |>
          dplyr::summarise(n_animals_plate = dplyr::n_distinct(animal), .groups = "drop") |>
          dplyr::group_by(.data[[gvar]], zone) |>
          dplyr::summarise(n = sum(n_animals_plate), .groups = "drop") |>
          dplyr::mutate(y = -Inf)

        tooltip_pts <- rlang::expr(paste0(
          "Animal: ", animal,
          "<br>Plate: ", plate_id,
          "<br>Condition: ", .data[[gvar]],
          "<br>Value: ", sprintf("%.2f", cum)
        ))

        gg <- ggplot2::ggplot(sub, ggplot2::aes(x = .data[[gvar]], y = cum)) +
          ggplot2::geom_boxplot(ggplot2::aes(group = .data[[gvar]], fill = .data[[gvar]]),
                                colour = edge_col, linewidth = box_lwd, width = box_w)

        gg <- add_boxplot_hover(gg, sub, x_col = gvar, y_col = "cum",
                                x_order = ord, hover_css = hover_css_box)

        gg <- add_sina_with_tooltip(gg, sub, aes_x = .data[[gvar]], aes_y = cum,
                                    tooltip_expr = tooltip_pts, size = pt_size, alpha = pt_alpha,
                                    edge_col = edge_col, sina_maxwidth = sina_maxw, hover_css = hover_css_pts)

        return(
          gg +
            ggplot2::geom_text(data = n_animals,
                               ggplot2::aes(x = .data[[gvar]], y = y, label = paste0("n=", n)),
                               inherit.aes = FALSE, vjust = -0.5, size = 3, colour = edge_col) +
            ggplot2::scale_fill_manual(values = cols_named, limits = ord) +
            ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = x_pad)) +
            ggplot2::labs(y = sprintf("Cumulative %s (Zone %s)", response_var, selected_zone),
                          caption = "Each point corresponds to the cumulative value for one animal.") +
            theme_obj +
            ggplot2::theme(legend.position = "none",
                           axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                           axis.title.x = ggplot2::element_blank(),
                           plot.caption.position = "plot",
                           plot.caption = ggplot2::element_text(hjust = 1))
        )
      }

      if (plot_type == "boxplot_delta") {

        tr <- input$transition_select
        sub <- dplyr::filter(sub, grepl(paste0("^", tr, "_"), transition_phase))
        sub$phase <- gsub(paste0("^", tr, "_"), "", sub$transition_phase)
        sub$phase <- dplyr::recode(sub$phase, before = "Before", switch = "Switch", after = "After", .default = NA_character_)
        present_phase <- intersect(c("Before","Switch","After"), unique(sub$phase))
        sub$phase <- factor(sub$phase, levels = present_phase, ordered = TRUE)
        if (!length(present_phase) || nrow(sub) == 0) return(ggplot2::ggplot() + ggplot2::geom_blank() + theme_obj)
        sub$.row_id <- paste0("r", seq_len(nrow(sub)))

        if (identical(boxplot_mode, "separated")) {

          tooltip_pts <- rlang::expr(paste0(
            "Animal: ", animal,
            "<br>Plate: ", plate_id,
            "<br>Condition: ", .data[[gvar]],
            "<br>Phase: ", phase,
            "<br>Value: ", sprintf("%.2f", mean_val)
          ))

          gg <- ggplot2::ggplot(sub, ggplot2::aes(x = .data[[gvar]], y = mean_val)) +
            ggplot2::geom_boxplot(ggplot2::aes(group = .data[[gvar]], fill = .data[[gvar]]),
                                  colour = edge_col, linewidth = box_lwd, width = box_w)

          gg <- add_boxplot_hover(gg, sub, x_col = gvar, y_col = "mean_val",
                                  facet_col = "phase", x_order = ord, hover_css = hover_css_box)

          gg <- add_sina_with_tooltip(gg, sub, aes_x = .data[[gvar]], aes_y = mean_val,
                                      tooltip_expr = tooltip_pts, size = pt_size, alpha = pt_alpha,
                                      edge_col = edge_col, sina_maxwidth = sina_maxw, hover_css = hover_css_pts)

          return(
            gg +
              ggplot2::facet_wrap(~phase, scales = "free_x") +
              ggplot2::scale_fill_manual(values = cols_named, limits = ord) +
              ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = x_pad)) +
              ggplot2::labs(y = sprintf("%s (Zone %s)", response_var, selected_zone),
                            caption = "Each point is the mean per animal in the Before / Switch / After windows.") +
              theme_obj +
              ggplot2::theme(legend.position = "none",
                             axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                             axis.title.x = ggplot2::element_blank(),
                             plot.caption.position = "plot",
                             plot.caption = ggplot2::element_text(hjust = 1))
          )
        }

        phase_cols_in <- trimws(strsplit(input$boxplot_delta_phase_colors, ",")[[1]])
        phase_cols <- ensure_colors(length(present_phase), phase_cols_in); names(phase_cols) <- present_phase

        tooltip_pts <- rlang::expr(paste0(
          "Animal: ", animal,
          "<br>Plate: ", plate_id,
          "<br>Condition: ", .data[[gvar]],
          "<br>Phase: ", phase,
          "<br>Value: ", sprintf("%.2f", mean_val)
        ))

        gg <- ggplot2::ggplot(sub, ggplot2::aes(x = .data[[gvar]], y = mean_val)) +
          ggplot2::geom_boxplot(
            ggplot2::aes(group = interaction(.data[[gvar]], phase), fill = phase),
            colour = edge_col, linewidth = box_lwd, width = box_w, position = dodge_pos
          )

        gg <- add_boxplot_hover(gg, sub, x_col = gvar, y_col = "mean_val",
                                dodge_col = "phase", x_order = ord,
                                dodge_levels = present_phase, hover_css = hover_css_box)

        return(
          gg +
            ggiraph::geom_point_interactive(
              ggplot2::aes(tooltip = !!tooltip_pts, data_id = .row_id,
                           group = interaction(.data[[gvar]], phase), colour = phase),
              position = point_pos,
              size = pt_size, alpha = pt_alpha, hover_css = hover_css_pts
            ) +
            ggplot2::scale_fill_manual(values = phase_cols, breaks = present_phase, limits = present_phase, name = "Phase") +
            ggplot2::scale_colour_manual(values = rep(edge_col, length(present_phase)), guide = "none") +
            ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = x_pad)) +
            ggplot2::labs(y = sprintf("%s (Zone %s)", response_var, selected_zone),
                          caption = "Each point is the mean per animal in the Before / Switch / After windows.") +
            theme_obj +
            ggplot2::theme(legend.position = "right",
                           axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                           axis.title.x = ggplot2::element_blank(),
                           plot.caption.position = "plot",
                           plot.caption = ggplot2::element_text(hjust = 1))
        )
      }

      if (plot_type == "lineplot") {

        gg <- ggplot2::ggplot(sub,
                              ggplot2::aes(x = start_rounded, y = val_per_well,
                                           colour = .data[[gvar]], group = .data[[gvar]]))

        if ("se_per_well" %in% names(sub)) {
          if (identical(input$lineplot_error_mode, "error_bar")) {
            gg <- gg + ggplot2::geom_errorbar(
              ggplot2::aes(ymin = val_per_well - se_per_well, ymax = val_per_well + se_per_well),
              width = 0.15, linewidth = 0.5, alpha = 0.7
            )
          } else if (identical(input$lineplot_error_mode, "ci95")) {
            gg <- gg + ggplot2::geom_ribbon(
              ggplot2::aes(ymin = val_per_well - 1.96 * se_per_well,
                           ymax = val_per_well + 1.96 * se_per_well,
                           fill = .data[[gvar]]),
              alpha = 0.2, color = NA
            ) + ggplot2::scale_fill_manual(values = cols_named, limits = ord)
          }
        }

        return(
          gg +
            ggplot2::geom_line(linewidth = 0.8) +
            ggiraph::geom_point_interactive(
              ggplot2::aes(
                tooltip = paste0("Condition: ", .data[[gvar]],
                                 "<br>Time: ", start_rounded,
                                 "<br>Value: ", sprintf("%.2f", val_per_well)),
                data_id = paste0(.data[[gvar]], "_", start_rounded, "_", seq_len(nrow(sub)))
              ),
              size = pt_size, alpha = pt_alpha, hover_css = hover_css_pts
            ) +
            ggplot2::scale_colour_manual(values = cols_named, limits = ord) +
            ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = 0.02)) +
            ggplot2::labs(
              x = sprintf("Time (%s)", if (input$time_unit_convert == "Yes") input$time_unit_target else input$time_unit_original),
              y = sprintf("%s (Zone %s)", response_var, selected_zone),
              caption = "Each line is the normalized response per condition over time."
            ) +
            theme_obj +
            ggplot2::theme(plot.caption.position = "plot",
                           plot.caption = ggplot2::element_text(hjust = 1, margin = ggplot2::margin(t = 10)),
                           axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
        )
      }

      ggplot2::ggplot() + ggplot2::geom_blank() + theme_obj
    }

    # ==================================================================
    # Reproduction script generator (simple, separated-only)
    # ==================================================================
    generate_r_script <- function(df, response_var, plot_type, boxplot_mode,
                                  selected_zone, theme_choice, condition_order, condition_colors,
                                  extra_params = list()) {

      `%||%` <- function(a, b) if (is.null(a)) b else a

      if (!plot_type %in% c("boxplot_periods", "boxplot_cumulate", "boxplot_delta", "lineplot")) {
        stop("generate_r_script() only supports XLSX plot types.")
      }

      visible_conditions <- extra_params$visible_conditions %||% character(0)
      transition         <- extra_params$transition %||% ""
      lineplot_error_mode <- extra_params$error_mode %||% "error_bar"
      time_unit_label     <- extra_params$time_unit %||% "seconds"

      dataset_type <- switch(
        plot_type,
        "boxplot_periods"  = "periods",
        "boxplot_cumulate" = "cumulative",
        "boxplot_delta"    = "delta",
        "lineplot"         = "lineplot"
      )

      xlsx_name <- paste0(dataset_type, "_dataset_", response_var, ".xlsx")

      visible_cond_txt <- if (length(visible_conditions)) {
        paste(sprintf("'%s'", visible_conditions), collapse = ", ")
      } else {
        ""
      }

      script_header <- c(
        "# ======================================================================",
        "# REPRODUCTION SCRIPT - Generated by Shiny App",
        paste("# Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        paste("# Plot Type:", plot_type),
        paste("# Boxplot Mode:", boxplot_mode %||% "NA"),
        paste("# Response Variable:", response_var),
        paste("# Zone:", selected_zone),
        paste("# Theme in app:", theme_choice),
        "# ======================================================================",
        "",
        "library(ggplot2)",
        "library(dplyr)",
        "library(readxl)",
        "",
        paste0("df <- readxl::read_excel('", xlsx_name, "')"),
        paste0("sub <- subset(df, zone == ", selected_zone, ")"),
        "sub <- droplevels(sub)",
        ""
      )

      if (length(visible_conditions)) {
        script_header <- c(
          script_header,
          paste0("visible_conditions <- c(", visible_cond_txt, ")"),
          "sub <- sub[sub$condition_grouped %in% visible_conditions, , drop = FALSE]",
          ""
        )
      }

      script_body <- switch(

        plot_type,

        "boxplot_periods" = {
          if (identical(boxplot_mode, "separated")) {
            c(
              "ggplot(sub, aes(x = condition_grouped, y = mean_val)) +",
              "  geom_boxplot(aes(fill = condition_grouped)) +",
              "  geom_point(position = position_jitter(width = 0.15), alpha = 0.7) +",
              "  facet_wrap(~period_without_numbers, scales = 'free_x') +",
              paste0("  labs(y = '", response_var, " (Zone ", selected_zone, ")') +"),
              "  theme_bw() +",
              "  theme(axis.text.x = element_text(angle = 45, hjust = 1),",
              "        axis.title.x = element_blank(),",
              "        legend.position = 'none')"
            )
          } else {
            c(
              "ggplot(sub, aes(x = condition_grouped, y = mean_val, fill = period_without_numbers)) +",
              "  geom_boxplot(position = position_dodge(width = 0.75)) +",
              "  geom_point(",
              "    aes(group = interaction(condition_grouped, period_without_numbers)),",
              "    position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.15),",
              "    alpha = 0.7",
              "  ) +",
              paste0("  labs(y = '", response_var, " (Zone ", selected_zone, ")', fill = 'Period') +"),
              "  theme_bw() +",
              "  theme(axis.text.x = element_text(angle = 45, hjust = 1),",
              "        axis.title.x = element_blank())"
            )
          }
        },

        "boxplot_cumulate" = c(
          "ggplot(sub, aes(x = condition_grouped, y = cum)) +",
          "  geom_boxplot(aes(fill = condition_grouped)) +",
          "  geom_point(position = position_jitter(width = 0.15), alpha = 0.7) +",
          paste0("  labs(y = 'Cumulative ", response_var, " (Zone ", selected_zone, ")') +"),
          "  theme_bw() +",
          "  theme(axis.text.x = element_text(angle = 45, hjust = 1),",
          "        axis.title.x = element_blank(),",
          "        legend.position = 'none')"
        ),

        "boxplot_delta" = {
          if (!nzchar(transition)) {
            c("stop('Missing transition for delta plot export.')")
          } else if (identical(boxplot_mode, "separated")) {
            c(
              paste0("sub <- sub[grepl('^", transition, "_', sub$transition_phase), , drop = FALSE]"),
              "sub$phase <- gsub('.*_', '', sub$transition_phase)",
              "sub$phase <- dplyr::recode(sub$phase,",
              "  before = 'Before',",
              "  switch = 'Switch',",
              "  after  = 'After'",
              ")",
              "",
              "ggplot(sub, aes(x = condition_grouped, y = mean_val)) +",
              "  geom_boxplot(aes(fill = condition_grouped)) +",
              "  geom_point(position = position_jitter(width = 0.15), alpha = 0.7) +",
              "  facet_wrap(~phase, scales = 'free_x') +",
              paste0("  labs(y = '", response_var, " (Zone ", selected_zone, ")') +"),
              "  theme_bw() +",
              "  theme(axis.text.x = element_text(angle = 45, hjust = 1),",
              "        axis.title.x = element_blank(),",
              "        legend.position = 'none')"
            )
          } else {
            c(
              paste0("sub <- sub[grepl('^", transition, "_', sub$transition_phase), , drop = FALSE]"),
              "sub$phase <- gsub('.*_', '', sub$transition_phase)",
              "sub$phase <- dplyr::recode(sub$phase,",
              "  before = 'Before',",
              "  switch = 'Switch',",
              "  after  = 'After'",
              ")",
              "",
              "ggplot(sub, aes(x = condition_grouped, y = mean_val, fill = phase)) +",
              "  geom_boxplot(position = position_dodge(width = 0.75)) +",
              "  geom_point(",
              "    aes(group = interaction(condition_grouped, phase)),",
              "    position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.15),",
              "    alpha = 0.7",
              "  ) +",
              paste0("  labs(y = '", response_var, " (Zone ", selected_zone, ")', fill = 'Phase') +"),
              "  theme_bw() +",
              "  theme(axis.text.x = element_text(angle = 45, hjust = 1),",
              "        axis.title.x = element_blank())"
            )
          }
        },

        "lineplot" = {
          if (identical(lineplot_error_mode, "ci95")) {
            c(
              "ggplot(sub, aes(x = start_rounded, y = val_per_well, colour = condition_grouped, group = condition_grouped)) +",
              "  geom_ribbon(",
              "    aes(ymin = val_per_well - 1.96 * se_per_well,",
              "        ymax = val_per_well + 1.96 * se_per_well,",
              "        fill = condition_grouped),",
              "    alpha = 0.2, colour = NA",
              "  ) +",
              "  geom_line() +",
              "  geom_point() +",
              paste0("  labs(x = 'Time (", time_unit_label, ")', y = '", response_var, " (Zone ", selected_zone, ")') +"),
              "  theme_bw() +",
              "  theme(axis.text.x = element_text(angle = 45, hjust = 1))"
            )
          } else {
            c(
              "ggplot(sub, aes(x = start_rounded, y = val_per_well, colour = condition_grouped, group = condition_grouped)) +",
              "  geom_errorbar(aes(ymin = val_per_well - se_per_well, ymax = val_per_well + se_per_well), width = 0.15) +",
              "  geom_line() +",
              "  geom_point() +",
              paste0("  labs(x = 'Time (", time_unit_label, ")', y = '", response_var, " (Zone ", selected_zone, ")') +"),
              "  theme_bw() +",
              "  theme(axis.text.x = element_text(angle = 45, hjust = 1))"
            )
          }
        }
      )

      paste(c(script_header, script_body), collapse = "\n")
    }

    # ==================================================================
    # Render (single mode: girafe only)
    # ==================================================================
    output$girafe_plot <- ggiraph::renderGirafe({
      tryCatch({

        input$generate_figure
        input$response_var

        if (identical(input$data_source_tabs, "xlsx")) {

          if (is.null(input$response_var) || !nzchar(input$response_var)) {
            p0 <- ggplot2::ggplot() +
              ggplot2::annotate("text", x = 0, y = 0, label = "Select a response variable") +
              ggplot2::theme_void()
            return(to_girafe(p0, width_svg = 12, height_svg = 9))
          }

          df <- switch(
            input$plot_type,
            "boxplot_periods"  = rv$all_zone_combined_light_dark_boxplots[[input$response_var]],
            "boxplot_cumulate" = rv$all_zone_combined_cum_boxplots[[input$response_var]],
            "boxplot_delta"    = rv$all_zone_combined_delta_boxplots[[input$response_var]],
            "lineplot"         = rv$all_zone_combined_lineplots[[input$response_var]]
          )

          if (is.null(df) || !nrow(df)) {
            p0 <- ggplot2::ggplot() +
              ggplot2::annotate("text", x = 0, y = 0, label = "Generate datasets first") +
              ggplot2::theme_void()
            return(to_girafe(p0, width_svg = 12, height_svg = 9))
          }

          log("⏳ Generating figure...")
          on.exit(log("🖼 Figure generated."), add = TRUE)

          selected_zone <- input$selected_zone
          if (is.null(selected_zone) || !length(selected_zone) || !selected_zone %in% unique(df$zone)) {
            zones <- sort(unique(df$zone))
            if (!length(zones)) {
              p0 <- ggplot2::ggplot() +
                ggplot2::annotate("text", x = 0, y = 0, label = "No zones available") +
                ggplot2::theme_void()
              return(to_girafe(p0, width_svg = 12, height_svg = 9))
            }
            selected_zone <- as.character(zones[1])
          }

          oc <- order_and_colors(df)

          boxplot_mode <- switch(
            input$plot_type,
            "boxplot_periods"  = input$boxplot_periods_mode,
            "boxplot_cumulate" = "separated",
            "boxplot_delta"    = input$boxplot_delta_mode,
            NULL
          )

          p <- generate_plot(
            df = df,
            response_var = input$response_var,
            plot_type = input$plot_type,
            boxplot_mode = boxplot_mode,
            selected_zone = selected_zone,
            theme_choice = input$theme_switch,
            condition_order = oc$order,
            condition_colors = oc$colors
          )

          rv$plot_gg <- p
          return(to_girafe(p, width_svg = 12, height_svg = 9))
        }

        if (identical(input$data_source_tabs, "txt")) {

          if (is.null(rv$txt_spatial_current) || !nrow(rv$txt_spatial_current)) {
            p0 <- ggplot2::ggplot() +
              ggplot2::annotate("text", x = 0, y = 0, label = "Generate a TXT dataset first") +
              ggplot2::theme_void()
            return(to_girafe(p0, width_svg = 12, height_svg = 9))
          }

          log("⏳ Generating TXT figure...")
          on.exit(log("🖼 TXT figure generated."), add = TRUE)

          p <- generate_txt_plot(
            df = rv$txt_spatial_current,
            theme_choice = input$theme_switch,
            show_points = isTRUE(input$txt_show_points)
          )

          rv$plot_gg <- p
          return(to_girafe(p, width_svg = 12, height_svg = 9))
        }

        p0 <- ggplot2::ggplot() + ggplot2::theme_void()
        to_girafe(p0, width_svg = 12, height_svg = 9)

      }, error = function(e) {
        log(paste("❌ Figure generation failed:", e$message))

        p_err <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0, y = 0, label = "Figure generation failed") +
          ggplot2::theme_void()

        to_girafe(p_err, width_svg = 12, height_svg = 9)
      })
    })

    output$console_output <- shiny::renderPrint({
      msgs <- console_messages()
      if (length(msgs) == 0 || all(msgs == "👋 Ready.")) cat("👋 Ready.") else cat(paste(msgs, collapse = "\n"))
    })

    # ==================================================================
    # Save / regenerate
    # ==================================================================
    save_current_state <- function(p) {
      rv$plot_gg <- p
      rv$plot_girafe <- to_girafe(p, width_svg = 12, height_svg = 9)
    }

    make_plot <- function(log_it = FALSE) {
      if (!identical(input$data_source_tabs, "txt")) {
        return(invisible(NULL))
      }

      tryCatch({
        wells <- txt_selected_wells()
        shiny::req(length(wells) > 0)

        rng <- resolve_txt_time_range(
          df = txt_base(),
          well_keys = wells,
          mode = input$txt_time_mode,
          time_start = input$txt_time_start,
          time_end = input$txt_time_end,
          period_value = input$txt_period_select
        )

        df_txt <- build_txt_trajectory_df(
          df = txt_base(),
          well_keys = wells,
          time_range = rng,
          target_points = input$txt_target_points
        )

        rv$txt_spatial_current <- df_txt

        if (log_it) {
          log(sprintf(
            "✅ TXT spatial dataset ready for figure generation (%s rows after aggregation, bin = %.3fs).",
            nrow(df_txt),
            attr(df_txt, "bin_s")
          ))
        }

        invisible(TRUE)
      }, error = function(e) {
        if (log_it) log(paste("❌ TXT figure preparation failed:", e$message))
        invisible(NULL)
      })
    }

    shiny::observeEvent(input$generate_txt_figure, { make_plot(log_it = TRUE) })

    shiny::observeEvent(
      list(
        input$theme_switch,
        input$txt_plate_select,
        input$txt_condition_select,
        input$txt_well_select,
        input$txt_time_mode,
        input$txt_time_start,
        input$txt_time_end,
        input$txt_period_select,
        input$txt_target_points,
        input$txt_show_points
      ),
      {
        if (identical(isolate(input$data_source_tabs), "txt") &&
            !is.null(rv$txt_spatial_current)) {
          make_plot(log_it = FALSE)
        }
      },
      ignoreInit = TRUE
    )

    # ==================================================================
    # Downloads
    # ==================================================================
    output$download_plot_script <- shiny::downloadHandler(
      filename = function() {
        var <- input$response_var
        zone <- input$selected_zone
        mode <- if (input$plot_type %in% c("boxplot_periods", "boxplot_delta")) {
          input[[paste0("boxplot_", sub("boxplot_", "", input$plot_type), "_mode")]] %||% "separated"
        } else "pooled"
        transition <- if (input$plot_type == "boxplot_delta") paste0("_", input$transition_select) else ""
        sprintf("%s_%s_zone%s_%s%s.R", input$plot_type, var, zone, mode, transition)
      },
      content = function(file) {
        df <- switch(
          input$plot_type,
          "boxplot_periods"  = rv$all_zone_combined_light_dark_boxplots[[input$response_var]],
          "boxplot_cumulate" = rv$all_zone_combined_cum_boxplots[[input$response_var]],
          "boxplot_delta"    = rv$all_zone_combined_delta_boxplots[[input$response_var]],
          "lineplot"         = rv$all_zone_combined_lineplots[[input$response_var]]
        )
        shiny::req(df, input$response_var, input$selected_zone)

        oc <- order_and_colors(df)
        boxplot_mode <- switch(input$plot_type,
                               "boxplot_periods" = input$boxplot_periods_mode,
                               "boxplot_delta"   = input$boxplot_delta_mode,
                               "separated"
        )

        extra <- list(
          visible_conditions = input$visible_conditions %||% character(0),
          pt_size  = 2.3,
          pt_alpha = 0.65,
          jit_w    = 0.18,
          box_lwd  = 0.55,
          box_w    = 0.60,
          x_pad    = 0.60,
          dodge_w  = 0.65
        )

        if (input$plot_type == "boxplot_periods" && boxplot_mode == "pooled")
          extra$period_colors <- input$boxplot_periods_colors

        if (input$plot_type == "boxplot_delta" && boxplot_mode == "pooled")
          extra$phase_colors <- input$boxplot_delta_phase_colors

        if (input$plot_type == "boxplot_delta")
          extra$transition <- input$transition_select

        if (input$plot_type == "lineplot") {
          unit <- if (input$time_unit_convert == "Yes") input$time_unit_target else input$time_unit_original
          extra$time_unit  <- unit
          extra$error_mode <- input$lineplot_error_mode
        }

        script_content <- generate_r_script(
          df = df,
          response_var = input$response_var,
          plot_type = input$plot_type,
          boxplot_mode = boxplot_mode,
          selected_zone = input$selected_zone,
          theme_choice = input$theme_switch,
          condition_order = oc$order,
          condition_colors = oc$colors,
          extra_params = extra
        )
        writeLines(script_content, file)
      }
    )

    # ==================================================================
    # Datasets table + download
    # ==================================================================
    output$dataset_table <- DT::renderDataTable({
      shiny::req(input$dataset_type, input$dataset_response_var)
      df <- if (input$dataset_type == "Boxplot Periods") {
        rv$all_zone_combined_light_dark_boxplots[[input$dataset_response_var]]
      } else if (input$dataset_type == "Boxplot Cumulative") {
        rv$all_zone_combined_cum_boxplots[[input$dataset_response_var]]
      } else if (input$dataset_type == "Boxplot Delta") {
        rv$all_zone_combined_delta_boxplots[[input$dataset_response_var]]
      } else {
        rv$all_zone_combined_lineplots[[input$dataset_response_var]]
      }
      shiny::req(df)
      DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$download_current_dataset <- shiny::downloadHandler(
      filename = function() {
        dtype <- switch(input$dataset_type,
                        "Boxplot Periods"     = "periods",
                        "Boxplot Cumulative"  = "cumulative",
                        "Boxplot Delta"       = "delta",
                        "Lineplot"            = "lineplot",
                        "dataset"
        )
        sprintf("%s_dataset_%s.xlsx", dtype, input$dataset_response_var)
      },
      content = function(file) {
        df <- switch(
          input$dataset_type,
          "Boxplot Periods"     = rv$all_zone_combined_light_dark_boxplots[[input$dataset_response_var]],
          "Boxplot Cumulative"  = rv$all_zone_combined_cum_boxplots[[input$dataset_response_var]],
          "Boxplot Delta"       = rv$all_zone_combined_delta_boxplots[[input$dataset_response_var]],
          "Lineplot"            = rv$all_zone_combined_lineplots[[input$dataset_response_var]]
        )
        shiny::req(df)
        openxlsx::write.xlsx(df, file)
      }
    )

    # ==================================================================
    # Delta Percentage Tables
    # ==================================================================
    compute_delta_percentages <- function(df, var) {
      shiny::req(df, var %in% names(df))
      tr <- input$transition_select
      df <- dplyr::filter(df, grepl(paste0("^", tr, "_"), transition_phase))
      df$phase <- gsub(paste0("^", tr, "_"), "", df$transition_phase)
      df$phase <- dplyr::recode(df$phase, before = "Before", switch = "Switch", after = "After")
      present <- intersect(c("Before","Switch","After"), unique(df$phase))
      if (length(present) < 2) return(NULL)
      df <- df |> dplyr::filter(phase %in% present)

      if (input$delta_table_type == "Momentum Comparisons") {
        comps <- list("Before_to_Switch" = c("Before","Switch"), "Switch_to_After" = c("Switch","After"))
        purrr::map_dfr(names(comps), function(name) {
          p1 <- comps[[name]][1]; p2 <- comps[[name]][2]
          df12 <- df |> dplyr::filter(phase %in% c(p1,p2))
          if (!nrow(df12)) return(NULL)
          df12 |>
            dplyr::group_by(condition_grouped, zone, plate_id, animal) |>
            dplyr::filter(n() == 2) |>
            dplyr::summarise(
              val1 = mean_val[phase == p1],
              val2 = mean_val[phase == p2],
              pct_change = ((val2 - val1) / pmax(val1, 1e-6)) * 100,
              .groups = "drop"
            ) |>
            dplyr::mutate(comparison = name)
        })
      } else {
        df |>
          dplyr::group_by(phase, zone) |>
          dplyr::do({
            d <- .
            conds <- unique(d$condition_grouped)
            if (length(conds) < 2) return(NULL)
            combos <- utils::combn(conds, 2, simplify = FALSE)
            purrr::map_dfr(combos, function(pair) {
              c1 <- pair[1]; c2 <- pair[2]
              d12 <- d |> dplyr::filter(condition_grouped %in% pair)
              if (!nrow(d12)) return(NULL)
              d12 |>
                dplyr::group_by(plate_id, animal) |>
                dplyr::filter(n() == 2) |>
                dplyr::summarise(
                  val1 = mean_val[condition_grouped == c1],
                  val2 = mean_val[condition_grouped == c2],
                  pct_change = ((val2 - val1) / pmax(val1, 1e-6)) * 100,
                  .groups = "drop"
                ) |>
                dplyr::mutate(comparison = paste0(c1, "_vs_", c2), phase = unique(d$phase))
            })
          }) |>
          dplyr::ungroup()
      }
    }

    output$delta_percentage_table <- DT::renderDataTable({
      shiny::req(input$delta_table_var, rv$all_zone_combined_delta_boxplots[[input$delta_table_var]])
      df <- rv$all_zone_combined_delta_boxplots[[input$delta_table_var]]
      tab <- compute_delta_percentages(df, input$delta_table_var)
      if (is.null(tab)) {
        DT::datatable(data.frame(Message = "Not enough data for percentage calculations."), options = list(dom = "t"))
      } else {
        DT::datatable(tab, options = list(pageLength = 15, scrollX = TRUE))
      }
    })

    output$download_current_delta_table <- shiny::downloadHandler(
      filename = function() {
        sprintf("delta_pct_%s_%s_%s.xlsx", input$delta_table_type, input$delta_table_var, input$transition_select)
      },
      content = function(file) {
        shiny::req(input$delta_table_var, rv$all_zone_combined_delta_boxplots[[input$delta_table_var]])
        df <- rv$all_zone_combined_delta_boxplots[[input$delta_table_var]]
        tab <- compute_delta_percentages(df, input$delta_table_var)
        if (!is.null(tab)) openxlsx::write.xlsx(tab, file)
      }
    )

    output$download_all_delta_tables <- shiny::downloadHandler(
      filename = function() sprintf("all_delta_tables_%s.zip", format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) {
        shiny::req(rv$all_zone_combined_delta_boxplots, input$transition_select)
        tmp <- tempdir()
        files <- c()
        for (v in names(rv$all_zone_combined_delta_boxplots)) {
          df <- rv$all_zone_combined_delta_boxplots[[v]]
          tab <- compute_delta_percentages(df, v)
          if (!is.null(tab)) {
            fn <- file.path(tmp, sprintf("delta_pct_%s_%s_%s.xlsx", input$delta_table_type, v, input$transition_select))
            openxlsx::write.xlsx(tab, fn)
            files <- c(files, fn)
          }
        }
        if (length(files)) zip::zip(file, files, mode = "cherry-pick")
      },
      contentType = "application/zip"
    )

    shiny::observeEvent(input$generate_delta_tables, {
      shiny::req(rv$all_zone_combined_delta_boxplots, input$delta_table_var)
      log("Delta percentage tables ready for viewing/download.")
    })

    shiny::observeEvent(rv$all_zone_combined_light_dark_boxplots, {
      if (is.null(input$selected_zone) || input$selected_zone == "") {
        zones <- sort(unique(rv$all_zone_combined_light_dark_boxplots[[1]]$zone))
        if (length(zones)) shiny::updateSelectInput(session, "selected_zone", selected = as.character(zones[1]))
      }
    }, ignoreNULL = TRUE)

    onStop(function() {
      rv$plot_gg <- NULL
      rv$plot_girafe <- NULL
    })
  })
}

# ======================================================================
# End of visualization_module.R
# ======================================================================
