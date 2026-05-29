# ======================================================================
# modules/visualization_module.R
# Generic visualization module (TM/QM × LDM/VM) driven by config
# Migrated from plotly -> ggiraph (single interactive mode, no Output Mode)
# ======================================================================

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
                       shiny::div(
                         style = "display:flex; align-items:center; gap:8px;",
                         shiny::actionButton(ns("generate_figure"), "Generate Figure", style = "flex:1;"),
                         shiny::actionLink(
                           ns("figure_info"),
                           label = NULL,
                           icon = shiny::icon("circle-info"),
                           title = "About this figure",
                           style = "font-size:20px; color:#337ab7; flex-shrink:0;"
                         )
                       ),
                       shiny::conditionalPanel(
                         condition = paste0(
                           "['boxplot_periods','boxplot_cumulate','boxplot_delta']",
                           ".indexOf(input['", ns("plot_type"), "']) !== -1"
                         ),
                         shiny::div(
                           style = "display:flex; align-items:center; gap:8px;",
                           shiny::actionButton(ns("generate_percentage"), "Generate Percentage", style = "flex:1;"),
                           shiny::actionLink(
                             ns("percentage_info"),
                             label = NULL,
                             icon  = shiny::icon("circle-info"),
                             title = "About percentage calculations",
                             style = "font-size:20px; color:#337ab7; flex-shrink:0;"
                           )
                         )
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
                              style = "margin-top: 25px; border-top: 1px solid #ccc; padding-top: 15px; width: 100%; box-sizing: border-box;",
                              shiny::h5("Delta Time Explorer (Live Preview)", style = "text-align:center; margin-bottom:10px;"),
                              ggiraph::girafeOutput(ns("delta_time_explorer"), width = "100%", height = "450px"),
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
                          shiny::div(
                            style = "margin-top:10px; margin-bottom:10px; display:flex; gap:10px; flex-wrap:wrap;",
                            shiny::downloadButton(ns("download_current_dataset"), "Download Current Variable (.xlsx)"),
                            shiny::downloadButton(ns("download_all_variables"),   "Download All Variables (.xlsx)")
                          )
          ),

          shiny::tabPanel("Console Output",
                          shiny::div(
                            style = "height: 600px; overflow-y: auto; padding: 10px; border-radius: 6px;",
                            class = "console-container",
                            shiny::verbatimTextOutput(ns("console_output"), placeholder = TRUE)
                          )
          ),

          shiny::tabPanel("Percentage Tables", value = "percentage_tables",
                          shiny::selectInput(ns("pct_table_var"), "Response Variable", choices = "", selected = ""),
                          shiny::helpText(
                            "Columns 'mean_A' / 'median_A' refer to the first condition in the comparison pair;",
                            "'mean_B' / 'median_B' to the second. Percentage: (B − A) / |A| × 100."
                          ),
                          DT::dataTableOutput(ns("percentage_table")),
                          shiny::div(style = "margin-top:10px; margin-bottom:10px;",
                                     shiny::downloadButton(ns("download_percentage_table"), "Download Table (.xlsx)")
                          )
          ),

          shiny::tabPanel("Statistics", value = "statistics",
            shiny::fluidRow(
              shiny::column(4,
                shiny::selectInput(ns("stat_plot_type"), "Plot Type",
                  choices = c(
                    "Boxplot Periods"    = "boxplot_periods",
                    "Boxplot Cumulative" = "boxplot_cumulate",
                    "Boxplot Delta"      = "boxplot_delta",
                    "Lineplot"           = "lineplot"
                  ),
                  selected = "boxplot_periods"
                )
              ),
              shiny::column(4,
                shiny::selectInput(ns("stat_response_var"), "Response Variable",
                  choices = "", selected = "")
              ),
              shiny::column(4,
                shiny::selectInput(ns("stat_zone"), "Zone",
                  choices = "", selected = "")
              )
            ),
            shiny::fluidRow(
              shiny::column(4,
                shiny::radioButtons(ns("stat_comparison_type"), "Comparison Type",
                  choices = c(
                    "All conditions vs each other" = "all_vs_all",
                    "All conditions vs a control"  = "vs_control"
                  ),
                  selected = "all_vs_all"
                )
              ),
              shiny::column(4,
                shiny::conditionalPanel(
                  condition = paste0("input['", ns("stat_comparison_type"), "'] == 'vs_control'"),
                  shiny::selectInput(ns("stat_control_condition"), "Control Condition",
                    choices = "", selected = "")
                )
              ),
              shiny::column(4,
                shiny::radioButtons(ns("stat_sample_type"), "Sample Type",
                  choices = c(
                    "Independent samples" = "independent",
                    "Paired samples"      = "paired"
                  ),
                  selected = "independent"
                )
              )
            ),
            shiny::div(
              style = "margin-bottom:12px;",
              shiny::actionButton(ns("run_statistics"), "Run Statistics",
                icon = shiny::icon("calculator"))
            ),
            shiny::hr(),
            shiny::tabsetPanel(
              id = ns("stats_output_tabs"),
              shiny::tabPanel("Tables",
                shiny::uiOutput(ns("stats_tables_ui"))
              ),
              shiny::tabPanel("Figures",
                shiny::uiOutput(ns("stats_figures_ui"))
              )
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

    cfg <- shiny::reactive({
      if (is.function(config)) tryCatch(config(), error = function(e) NULL) else config
    })
    EXPECTED_VARS <- shiny::reactive({ cfg()$expected_vars })

    console_messages <- shiny::reactiveVal("👋 Ready.")
    log <- function(...) {
      old <- shiny::isolate(console_messages())
      console_messages(c(old, paste(...)))
    }

    # Cached reactive: prepare_all_zone() recomputes only when processing_results changes
    az_cached <- shiny::reactive({
      shiny::req(rv$processing_results)
      prepare_all_zone(rv$processing_results$processed_data_list, cfg())
    })

    shiny::observe({
      az <- az_cached()
      rv$all_zone_combined <- az
    })

    # Keep both last ggplot + last girafe widget
    rv$plot_gg     <- NULL
    rv$plot_girafe <- NULL
    rv$percentage_tables_list      <- NULL
    rv$percentage_tables_plot_type <- NULL
    rv$stats_results               <- NULL

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
      rv_sel <- ""; ds_sel <- ""; pct_sel <- vars[1]; stat_sel <- vars[1]
      if (isTRUE(read_current)) {
        if (!is.null(input$response_var)         && isolate(input$response_var)         %in% vars) rv_sel   <- isolate(input$response_var)
        if (!is.null(input$dataset_response_var) && isolate(input$dataset_response_var) %in% vars) ds_sel   <- isolate(input$dataset_response_var)
        if (!is.null(input$pct_table_var)        && isolate(input$pct_table_var)        %in% vars) pct_sel  <- isolate(input$pct_table_var)
        if (!is.null(input$stat_response_var)    && isolate(input$stat_response_var)    %in% vars) stat_sel <- isolate(input$stat_response_var)
      }
      shiny::updateSelectInput(session, "response_var",         choices = c("", vars), selected = rv_sel)
      shiny::updateSelectInput(session, "dataset_response_var", choices = c("", vars), selected = ds_sel)
      shiny::updateSelectInput(session, "pct_table_var",        choices = c("", vars), selected = pct_sel)
      shiny::updateSelectInput(session, "stat_response_var",    choices = c("", vars), selected = stat_sel)
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
    # UI bits
    # ==================================================================
    shiny::observeEvent(input$clear_console, {
      console_messages("👋 Ready.")
    })

    shiny::observeEvent(rv$app_reset_trigger, {
      console_messages("👋 Ready.")
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    txt_base <- shiny::reactive({
      tryCatch(
        prepare_txt_spatial_base(),
        error = function(e) NULL
      )
    })

    txt_selected_wells <- shiny::reactive({
      df <- txt_base()
      if (is.null(df) || !nrow(df)) return(character(0))

      zebRabox:::resolve_txt_selected_wells(
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
        zebRabox:::resolve_txt_time_range(
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

      df <- zebRabox:::add_txt_keys(df) |>
        dplyr::filter(
          well_key %in% wells,
          T >= rng[1],
          T <= rng[2]
        )

      if (!nrow(df)) return(NULL)

      native_dt <- zebRabox:::estimate_txt_dt(df)
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

      sub <- zebRabox:::add_txt_keys(df) |>
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

      df <- zebRabox:::add_txt_keys(df) |>
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
        zebRabox:::resolve_txt_time_range(
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

      oc <- tryCatch(order_and_colors(line_df), error = function(e) list(order = NULL, colors = NULL))

      yr  <- range(line_df$mean_val, na.rm = TRUE)
      pad <- diff(yr) * 0.06
      ymin <- yr[1] - pad
      ymax <- yr[2] + pad

      th        <- get_theme(input$theme_switch)
      theme_obj <- th$obj
      edge_col  <- th$edge_col

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

      if (!is.null(oc$colors) && length(oc$colors)) {
        p <- p + ggplot2::scale_colour_manual(values = oc$colors, limits = oc$order, na.value = "grey70", name = "Condition")
      }

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

          az <- az_cached()
          current_cfg <- cfg()
          incProgress(0.6)

          rv$all_zone_combined_light_dark_boxplots <- stats::setNames(
            lapply(EXPECTED_VARS(), function(v) build_periods_df(az, v, current_cfg, period_indices_keep = isolate(input$period_indices_keep))),
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
        az <- az_cached()
        current_cfg <- cfg()

        rv$all_zone_combined_light_dark_boxplots <- stats::setNames(
          lapply(EXPECTED_VARS(), function(v) build_periods_df(az, v, current_cfg, period_indices_keep = isolate(input$period_indices_keep))),
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

          az <- az_cached()
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

          az <- az_cached()
          delta_sec <- as.numeric(input$delta_time)
          incProgress(0.7)

          split_list <- build_delta_split(az, EXPECTED_VARS(), input$transition_select, delta_sec,
                                           boundaries_df = dplyr::bind_rows(get_boundaries_list()) |> dplyr::distinct())
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

          az <- az_cached()
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

          rng <- zebRabox:::resolve_txt_time_range(
            df = txt_base(),
            well_keys = wells,
            mode = input$txt_time_mode,
            time_start = input$txt_time_start,
            time_end = input$txt_time_end,
            period_value = input$txt_period_select
          )
          incProgress(0.6)

          rv$txt_spatial_current <- zebRabox:::build_txt_trajectory_df(
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
    # TXT spatial helpers — add_txt_keys, estimate_txt_dt,
    # resolve_txt_selected_wells, resolve_txt_time_range,
    # build_txt_trajectory_df moved to R/visualization.R
    # ==================================================================

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

    generate_txt_plot <- function(df, theme_choice, show_points = TRUE) {
      th        <- get_theme(theme_choice)
      theme_obj <- th$obj
      edge_col  <- th$edge_col

      cond_levels <- sort(unique(as.character(df$condition_grouped)))
      cols <- zebRabox:::ensure_colors(length(cond_levels))
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

      cols <- zebRabox:::ensure_colors(length(ord), raw_cols)
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
                 zebRabox:::void_theme())
      }
      sub <- sub[sub$condition_grouped %in% vis, , drop = FALSE]
      if (!nrow(sub)) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0, y = 0, label = "No data after filtering") +
                 zebRabox:::void_theme())
      }

      th        <- get_theme(theme_choice)
      theme_obj <- th$obj
      edge_col  <- th$edge_col

      hover_css_pts <- "r:6!important;opacity:1!important;fill-opacity:1!important;stroke-opacity:1!important;"
      hover_css_box <- "fill:transparent!important;stroke:transparent!important;opacity:1!important;"

      gvar <- "condition_grouped"
      present_levels <- unique(sub[[gvar]])
      ord <- intersect(condition_order, present_levels); if (!length(ord)) ord <- present_levels
      ord <- unique(c(ord, setdiff(present_levels, ord)))
      sub[[gvar]] <- factor(sub[[gvar]], levels = ord)

      cols_named <- condition_colors
      if (is.null(names(cols_named)) || length(cols_named) != length(ord) || any(!(ord %in% names(cols_named)))) {
        cols_named <- zebRabox:::ensure_colors(length(ord), cols_named); names(cols_named) <- ord
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
                            caption = "Each point represents one animal averaged over the corresponding period.") +
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
        per_cols    <- zebRabox:::ensure_colors(length(per_levels), per_cols_in)
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
                          caption = "Each point represents one animal averaged over the corresponding period.") +
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
                          caption = "Each point represents the cumulative response of one animal over the analysed duration.") +
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
                            caption = "Each point represents one animal averaged within the Before, Switch or After time window.") +
              theme_obj +
              ggplot2::theme(legend.position = "none",
                             axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                             axis.title.x = ggplot2::element_blank(),
                             plot.caption.position = "plot",
                             plot.caption = ggplot2::element_text(hjust = 1))
          )
        }

        phase_cols_in <- trimws(strsplit(input$boxplot_delta_phase_colors, ",")[[1]])
        phase_cols <- zebRabox:::ensure_colors(length(present_phase), phase_cols_in); names(phase_cols) <- present_phase

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
                          caption = "Each point represents one animal averaged within the Before, Switch or After time window.") +
            theme_obj +
            ggplot2::theme(legend.position = "right",
                           axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                           axis.title.x = ggplot2::element_blank(),
                           plot.caption.position = "plot",
                           plot.caption = ggplot2::element_text(hjust = 1))
        )
      }

      if (plot_type == "lineplot") {

        # val_per_well is the condition-level mean — identical for all animals at
        # the same (condition, time_bin). Deduplicate to avoid rendering 96× more
        # points than needed by ggiraph, which dominates rendering time.
        sub <- dplyr::distinct(sub, .data[[gvar]], start_rounded, .keep_all = TRUE)

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
              caption = "Each line represents the mean per-animal response within each time window."
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
              zebRabox:::void_theme()
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
              zebRabox:::void_theme()
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
                zebRabox:::void_theme()
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
              zebRabox:::void_theme()
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

        p0 <- ggplot2::ggplot() + zebRabox:::void_theme()
        to_girafe(p0, width_svg = 12, height_svg = 9)

      }, error = function(e) {
        log(paste("❌ Figure generation failed:", e$message))

        p_err <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0, y = 0, label = "Figure generation failed") +
          zebRabox:::void_theme()

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

        rng <- zebRabox:::resolve_txt_time_range(
          df = txt_base(),
          well_keys = wells,
          mode = input$txt_time_mode,
          time_start = input$txt_time_start,
          time_end = input$txt_time_end,
          period_value = input$txt_period_select
        )

        df_txt <- zebRabox:::build_txt_trajectory_df(
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
    # Percentage Tables
    # ==================================================================
    compute_percentage_table <- function(df, plot_type, transition = NULL) {
      val_col <- switch(
        plot_type,
        "boxplot_periods"  = "mean_val",
        "boxplot_cumulate" = "cum",
        "boxplot_delta"    = "mean_val",
        NULL
      )
      if (is.null(val_col) || !val_col %in% names(df)) return(NULL)

      if (plot_type == "boxplot_delta") {
        if (is.null(transition) || !nzchar(transition)) return(NULL)
        df <- dplyr::filter(df, grepl(paste0("^", transition, "_"), transition_phase))
        df$phase <- gsub(paste0("^", transition, "_"), "", df$transition_phase)
        df$phase <- dplyr::recode(df$phase,
                                   before = "Before", switch = "Switch", after = "After",
                                   .default = NA_character_)
        df <- dplyr::filter(df, !is.na(phase))
        group_col   <- "phase"
        group_order <- c("Before", "Switch", "After")
      } else if (plot_type == "boxplot_periods") {
        group_col   <- "period_without_numbers"
        group_order <- NULL
      } else {
        group_col   <- NULL
        group_order <- NULL
      }

      conds <- sort(unique(as.character(df$condition_grouped)))
      conds <- conds[!is.na(conds) & nzchar(conds)]
      if (length(conds) < 2) return(NULL)

      pairs <- utils::combn(conds, 2, simplify = FALSE)
      zones <- sort(unique(df$zone))

      if (!is.null(group_col) && group_col %in% names(df)) {
        groups <- unique(as.character(df[[group_col]]))
        groups <- groups[!is.na(groups)]
        groups <- if (!is.null(group_order)) intersect(group_order, groups) else sort(groups)
      } else {
        groups <- NULL
      }

      rows <- list()

      for (g in (if (!is.null(groups)) groups else "all")) {
        for (z in zones) {
          sub <- if (!is.null(group_col) && !is.null(groups)) {
            df[df[[group_col]] == g & df$zone == z, , drop = FALSE]
          } else {
            df[df$zone == z, , drop = FALSE]
          }

          for (pair in pairs) {
            cA <- pair[1]; cB <- pair[2]

            vA <- as.numeric(sub[sub$condition_grouped == cA, val_col, drop = TRUE])
            vB <- as.numeric(sub[sub$condition_grouped == cB, val_col, drop = TRUE])
            vA <- vA[is.finite(vA)]; vB <- vB[is.finite(vB)]
            if (!length(vA) || !length(vB)) next

            mA <- mean(vA); mB <- mean(vB)
            medA <- stats::median(vA); medB <- stats::median(vB)

            pct_mean   <- if (abs(mA)   > 1e-9) round((mB   - mA)   / abs(mA)   * 100, 2) else NA_real_
            pct_median <- if (abs(medA) > 1e-9) round((medB - medA) / abs(medA) * 100, 2) else NA_real_

            row <- data.frame(
              zone            = z,
              comparison      = paste0(cA, " vs ", cB),
              n_A             = length(vA),
              n_B             = length(vB),
              mean_A          = round(mA,   4),
              mean_B          = round(mB,   4),
              pct_diff_mean   = pct_mean,
              median_A        = round(medA, 4),
              median_B        = round(medB, 4),
              pct_diff_median = pct_median,
              stringsAsFactors = FALSE
            )

            if (!is.null(group_col) && !is.null(groups)) {
              grp_label <- if (plot_type == "boxplot_delta") "phase" else "period"
              row[[grp_label]] <- g
              col_order <- c(grp_label, "zone", "comparison", "n_A", "n_B",
                             "mean_A", "mean_B", "pct_diff_mean",
                             "median_A", "median_B", "pct_diff_median")
              row <- row[, col_order, drop = FALSE]
            }

            rows[[length(rows) + 1]] <- row
          }
        }
      }

      if (!length(rows)) return(NULL)
      dplyr::bind_rows(rows)
    }

    shiny::observeEvent(input$generate_percentage, {
      tryCatch({
        pt <- isolate(input$plot_type)

        df_list <- switch(
          pt,
          "boxplot_periods"  = rv$all_zone_combined_light_dark_boxplots,
          "boxplot_cumulate" = rv$all_zone_combined_cum_boxplots,
          "boxplot_delta"    = rv$all_zone_combined_delta_boxplots,
          NULL
        )

        if (is.null(df_list)) {
          log("❌ Generate the datasets first before computing percentages.")
          return()
        }

        transition <- if (pt == "boxplot_delta") isolate(input$transition_select) else NULL

        log("⏳ Computing percentage tables...")

        tbl_list <- stats::setNames(
          lapply(names(df_list), function(v) {
            tryCatch(
              compute_percentage_table(df_list[[v]], pt, transition),
              error = function(e) NULL
            )
          }),
          names(df_list)
        )

        rv$percentage_tables_list     <- tbl_list
        rv$percentage_tables_plot_type <- pt

        shiny::updateTabsetPanel(session, "output_tabs", selected = "percentage_tables")
        log("✅ Percentage tables computed.")
      }, error = function(e) {
        log(paste("❌ Percentage table computation failed:", e$message))
      })
    }, ignoreInit = TRUE)

    output$percentage_table <- DT::renderDataTable({
      shiny::req(input$pct_table_var)
      tbl <- rv$percentage_tables_list[[input$pct_table_var]]
      if (is.null(tbl)) {
        return(DT::datatable(
          data.frame(Message = "Click 'Generate Percentage' to compute the table."),
          options = list(dom = "t")
        ))
      }
      pct_cols <- intersect(c("pct_diff_mean", "pct_diff_median"), names(tbl))
      dt <- DT::datatable(tbl, options = list(pageLength = 25, scrollX = TRUE))
      if (length(pct_cols)) {
        dt <- dt |>
          DT::formatStyle(
            pct_cols,
            backgroundColor = DT::styleInterval(0, c("#ffb3b3", "#b3ffb3"))
          )
      }
      dt
    })

    output$download_percentage_table <- shiny::downloadHandler(
      filename = function() {
        pt  <- rv$percentage_tables_plot_type %||% "unknown"
        var <- input$pct_table_var
        sprintf("percentage_%s_%s.xlsx", pt, var)
      },
      content = function(file) {
        shiny::req(input$pct_table_var)
        tbl <- rv$percentage_tables_list[[input$pct_table_var]]
        if (!is.null(tbl)) openxlsx::write.xlsx(tbl, file)
      }
    )

    output$download_all_variables <- shiny::downloadHandler(
      filename = function() {
        dtype <- switch(input$dataset_type,
                        "Boxplot Periods"    = "periods",
                        "Boxplot Cumulative" = "cumulative",
                        "Boxplot Delta"      = "delta",
                        "Lineplot"           = "lineplot",
                        "dataset"
        )
        sprintf("%s_all_variables.xlsx", dtype)
      },
      content = function(file) {
        df_list <- switch(
          input$dataset_type,
          "Boxplot Periods"    = rv$all_zone_combined_light_dark_boxplots,
          "Boxplot Cumulative" = rv$all_zone_combined_cum_boxplots,
          "Boxplot Delta"      = rv$all_zone_combined_delta_boxplots,
          "Lineplot"           = rv$all_zone_combined_lineplots
        )
        shiny::req(df_list)
        sheets <- Filter(Negate(is.null), df_list)
        if (length(sheets) == 0) stop("No datasets available to export.")
        openxlsx::write.xlsx(sheets, file)
      }
    )

    shiny::observeEvent(rv$all_zone_combined_light_dark_boxplots, {
      if (is.null(input$selected_zone) || input$selected_zone == "") {
        zones <- sort(unique(rv$all_zone_combined_light_dark_boxplots[[1]]$zone))
        if (length(zones)) shiny::updateSelectInput(session, "selected_zone", selected = as.character(zones[1]))
      }
    }, ignoreNULL = TRUE)

    # ==================================================================
    # Figure info modal
    # ==================================================================
    shiny::observeEvent(input$figure_info, {
      pt <- isolate(input$plot_type)

      content <- switch(
        pt,

        "boxplot_periods" = shiny::tagList(
          shiny::h4("Period Boxplot — how points are computed"),
          shiny::p(
            shiny::strong("Each point = one animal"), " averaged across all time codes belonging to that period."
          ),
          shiny::tags$ol(
            shiny::tags$li("All raw measurements for an animal within a period are extracted from the XLSX file."),
            shiny::tags$li("These measurements are averaged to produce one value per animal per period."),
            shiny::tags$li("This mean is the plotted point.")
          ),
          shiny::p(
            shiny::strong("Period definition:"), " periods are defined by the transition file (columns ",
            shiny::code("start"), " and ", shiny::code("transition"), "), e.g. light1, dark1, light2 …"
          ),
          shiny::p(
            shiny::strong("'Keep only period indices' filter:"), " if you enter ", shiny::code("1"),
            " or ", shiny::code("1,2"), ", only the 1st (or 1st and 2nd) repetition of each period label is retained."
          ),
          shiny::p(
            shiny::strong("Boxplot:"), " the box shows the interquartile range (Q1–Q3), the horizontal bar is the median,",
            " and whiskers extend to 1.5 × IQR. Points beyond whiskers are plotted individually."
          )
        ),

        "boxplot_cumulate" = shiny::tagList(
          shiny::h4("Cumulative Boxplot — how points are computed"),
          shiny::p(
            shiny::strong("Each point = one animal's total activity"), " across the entire analysed recording."
          ),
          shiny::tags$ol(
            shiny::tags$li("All retained measurements for an animal are extracted (after any removal of wells, conditions, periods, or time codes)."),
            shiny::tags$li("These measurements are summed to produce one cumulative total per animal."),
            shiny::tags$li("This total is the plotted point.")
          ),
          shiny::p(
            shiny::strong("Use:"), " useful for comparing the overall activity level between conditions over the whole recording,",
            " independently of temporal dynamics."
          ),
          shiny::p(
            shiny::strong("Boxplot:"), " same representation as the period boxplot (IQR, median, 1.5 × IQR whiskers)."
          )
        ),

        "boxplot_delta" = shiny::tagList(
          shiny::h4("Delta Boxplot — how points are computed"),
          shiny::p(
            shiny::strong("Each point = one animal averaged within a phase window"), " around a stimulus transition."
          ),
          shiny::p(shiny::strong("Three consecutive windows of equal length Δ are defined:")),
          shiny::tags$ul(
            shiny::tags$li(shiny::strong("Before [t − Δ, t):"), " the window immediately before the transition — baseline response."),
            shiny::tags$li(shiny::strong("Switch [t, t + Δ):"), " the window starting at the transition — immediate response to the stimulus."),
            shiny::tags$li(shiny::strong("After [t + Δ, t + 2Δ):"), " the window following the switch — sustained or rebound response.")
          ),
          shiny::p(
            shiny::strong("t"), " is the transition time taken from the period boundary file, determined per plate independently.",
            " Δ is the value set with the Delta Time Window slider."
          ),
          shiny::p(
            shiny::strong("Per-animal value:"), " within each phase window, all measurements for a given animal are averaged.",
            " This mean is the plotted point."
          ),
          shiny::p(
            shiny::strong("'Separated' mode:"), " one panel per phase.",
            shiny::strong(" 'Pooled' mode:"), " all three phases side-by-side within each condition, coloured by phase."
          ),
          shiny::p(
            shiny::strong("The live preview (Delta Time Explorer)"), " below the main figure shows the raw temporal trajectory",
            " to help you choose an appropriate Δ."
          )
        ),

        "lineplot" = shiny::tagList(
          shiny::h4("Lineplot — how lines and error bands are computed"),
          shiny::p(
            shiny::strong("Each line = the mean per-animal response within each time bin"), " for a given condition."
          ),
          shiny::tags$ol(
            shiny::tags$li("Data are divided into non-overlapping time bins of the chosen aggregation width (e.g. 60 s)."),
            shiny::tags$li("Within each bin, the selected metric is summed per animal."),
            shiny::tags$li("The displayed y-value is the mean of these per-animal sums across all animals in the condition.")
          ),
          shiny::p(shiny::strong("Error representation:")),
          shiny::tags$ul(
            shiny::tags$li(shiny::strong("Error bars:"), " ± 1 standard error of the mean (SE = SD / √n)."),
            shiny::tags$li(shiny::strong("95% CI ribbon:"), " ± 1.96 × SE, shown as a shaded band.")
          ),
          shiny::p(
            shiny::strong("Time axis:"), " displayed in the selected time unit.",
            " If 'Convert Time Unit?' is set to Yes, the raw ", shiny::code("start"), " column (in seconds)",
            " is converted before binning — the aggregation period is then interpreted in the target unit."
          )
        ),

        shiny::p("Select a plot type to see its description.")
      )

      shiny::showModal(shiny::modalDialog(
        title = shiny::tagList(shiny::icon("circle-info"), " How this figure is computed"),
        content,
        easyClose = TRUE,
        size = "l",
        footer = shiny::modalButton("Close")
      ))
    }, ignoreInit = TRUE)

    # ==================================================================
    # Percentage info modal
    # ==================================================================
    shiny::observeEvent(input$percentage_info, {
      pt <- isolate(input$plot_type)

      content <- switch(
        pt,

        "boxplot_periods" = shiny::tagList(
          shiny::h4("Percentage differences — Period Boxplot"),
          shiny::p(
            "For each period, zone, and pair of conditions, two centrality measures are compared."
          ),
          shiny::tags$ul(
            shiny::tags$li(
              shiny::strong("Mean-based:"), " the mean of all per-animal values is computed per condition.",
              " Percentage difference = (mean_B − mean_A) / |mean_A| × 100."
            ),
            shiny::tags$li(
              shiny::strong("Median-based:"), " the median of all per-animal values is computed per condition.",
              " Percentage difference = (median_B − median_A) / |median_A| × 100."
            )
          ),
          shiny::p(
            "A = first condition in the pair (reference); B = second condition (comparator).",
            " A positive value means B > A."
          ),
          shiny::p(
            shiny::strong("Cell colours:"),
            " green = positive difference, red = negative difference."
          ),
          shiny::p(
            shiny::strong("Note:"), " clicking 'Generate Percentage' again after changing the",
            " 'Keep only period indices' filter will update the table with the new period selection."
          )
        ),

        "boxplot_cumulate" = shiny::tagList(
          shiny::h4("Percentage differences — Cumulative Boxplot"),
          shiny::p(
            "For each zone and pair of conditions, two centrality measures of the cumulative activity are compared."
          ),
          shiny::tags$ul(
            shiny::tags$li(
              shiny::strong("Mean-based:"), " mean cumulative value across all animals per condition.",
              " Percentage difference = (mean_B − mean_A) / |mean_A| × 100."
            ),
            shiny::tags$li(
              shiny::strong("Median-based:"), " median cumulative value across all animals per condition.",
              " Percentage difference = (median_B − median_A) / |median_A| × 100."
            )
          ),
          shiny::p(
            "A = first condition in the pair (reference); B = second condition (comparator).",
            " A positive value means B > A."
          ),
          shiny::p(
            shiny::strong("Cell colours:"),
            " green = positive difference, red = negative difference."
          )
        ),

        "boxplot_delta" = shiny::tagList(
          shiny::h4("Percentage differences — Delta Boxplot"),
          shiny::p(
            "For each Momentum (Before, Switch, After), zone, and pair of conditions,",
            " two centrality measures are compared."
          ),
          shiny::tags$ul(
            shiny::tags$li(
              shiny::strong("Mean-based:"), " mean per-animal value within that phase window per condition.",
              " Percentage difference = (mean_B − mean_A) / |mean_A| × 100."
            ),
            shiny::tags$li(
              shiny::strong("Median-based:"), " median per-animal value within that phase window per condition.",
              " Percentage difference = (median_B − median_A) / |median_A| × 100."
            )
          ),
          shiny::p(
            "A = first condition in the pair (reference); B = second condition (comparator).",
            " A positive value means B > A."
          ),
          shiny::p(
            shiny::strong("Cell colours:"),
            " green = positive difference, red = negative difference."
          ),
          shiny::p(
            "Each Momentum is treated independently, allowing you to detect differential",
            " effects of the stimulus across the Before, Switch and After time windows."
          )
        ),

        shiny::p("Select a compatible plot type (Periods, Cumulate, or Delta) to see the description.")
      )

      shiny::showModal(shiny::modalDialog(
        title = shiny::tagList(shiny::icon("circle-info"), " Percentage calculation method"),
        content,
        easyClose = TRUE,
        size = "l",
        footer = shiny::modalButton("Close")
      ))
    }, ignoreInit = TRUE)

    # ==================================================================
    # Statistical helpers (no Shiny dependency)
    # ==================================================================

    get_statistics_dataset <- function(df_list, response_var) {
      if (is.null(df_list) || is.null(response_var) || !nzchar(response_var)) return(NULL)
      df_list[[response_var]]
    }

    check_normality <- function(df, value_col, group_col) {
      if (is.null(df) || !nrow(df)) return(NULL)
      if (!value_col %in% names(df) || !group_col %in% names(df)) return(NULL)

      formula_obj <- as.formula(paste(value_col, "~", group_col))
      model <- tryCatch(aov(formula_obj, data = df), error = function(e) NULL)
      if (is.null(model)) return(NULL)

      resids <- residuals(model)
      if (length(resids) < 3) return(NULL)
      if (length(resids) > 5000) resids <- sample(resids, 5000)

      sw <- tryCatch(stats::shapiro.test(resids), error = function(e) NULL)
      if (is.null(sw)) return(NULL)

      is_normal <- sw$p.value > 0.05

      tbl <- data.frame(
        Test           = "Shapiro-Wilk",
        W_statistic    = round(as.numeric(sw$statistic), 4),
        p_value        = round(sw$p.value, 4),
        Interpretation = if (is_normal)
          "p > 0.05: residuals follow a normal distribution"
        else
          "p <= 0.05: residuals do NOT follow a normal distribution",
        Decision       = if (is_normal) "Normality assumed" else "Normality not assumed",
        stringsAsFactors = FALSE
      )

      list(table = tbl, residuals = resids, model = model, is_normal = is_normal)
    }

    check_homoscedasticity <- function(df, value_col, group_col, is_normal,
                                        sample_type = "independent") {
      if (sample_type == "paired") return(NULL)
      if (is.null(df) || !nrow(df)) return(NULL)

      n_conditions <- length(unique(df[[group_col]]))
      formula_obj  <- as.formula(paste(value_col, "~", group_col))

      if (n_conditions == 2 && sample_type == "independent") {
        groups <- split(df[[value_col]], df[[group_col]])
        result <- tryCatch(var.test(groups[[1]], groups[[2]]), error = function(e) NULL)
        if (is.null(result)) return(NULL)
        test_name <- "F-test for equality of variances (var.test)"
        stat_val  <- round(as.numeric(result$statistic), 4)
        df_str    <- paste(result$parameter, collapse = " / ")
        p_val     <- round(result$p.value, 4)

      } else if (is_normal) {
        result <- tryCatch(stats::bartlett.test(formula_obj, data = df), error = function(e) NULL)
        if (is.null(result)) return(NULL)
        test_name <- "Bartlett test"
        stat_val  <- round(as.numeric(result$statistic), 4)
        df_str    <- as.character(result$parameter)
        p_val     <- round(result$p.value, 4)

      } else {
        result <- tryCatch(
          car::leveneTest(formula_obj, data = df, center = "median"),
          error = function(e) NULL
        )
        if (is.null(result)) return(NULL)
        test_name <- "Levene test (car, center = median)"
        stat_val  <- round(result[["F value"]][1], 4)
        df_str    <- paste(result[["Df"]][1], "/", result[["Df"]][2])
        p_val     <- round(result[["Pr(>F)"]][1], 4)
      }

      is_homo <- p_val > 0.05

      tbl <- data.frame(
        Test            = test_name,
        Statistic       = stat_val,
        Df              = df_str,
        p_value         = p_val,
        Interpretation  = if (is_homo)
          "p > 0.05: variances are equal between groups"
        else
          "p <= 0.05: at least one group has a different variance",
        Decision        = if (is_homo) "Homoscedasticity assumed" else "Homoscedasticity not assumed",
        stringsAsFactors = FALSE
      )

      list(table = tbl, is_homoscedastic = is_homo, test_used = test_name)
    }

    select_inferential_test <- function(df, value_col, group_col, is_normal,
                                         is_homoscedastic, sample_type = "independent") {
      if (is.null(df) || !nrow(df)) return(NULL)

      formula_obj  <- as.formula(paste(value_col, "~", group_col))
      n_conditions <- length(unique(df[[group_col]]))
      groups       <- split(df[[value_col]], df[[group_col]])

      if (n_conditions == 2) {
        g1    <- groups[[1]]; g2 <- groups[[2]]
        n_min <- min(length(g1), length(g2))

        if (sample_type == "paired") {
          if (!is_normal) {
            result    <- tryCatch(stats::wilcox.test(g1, g2, paired = TRUE), error = function(e) NULL)
            test_name <- "Wilcoxon signed-rank test (paired)"
            reason    <- "Paired samples — normality not assumed"
          } else if (n_min >= 30) {
            result    <- tryCatch(stats::t.test(g1, g2, paired = TRUE), error = function(e) NULL)
            test_name <- "Paired Z-test (t.test approximation, n >= 30)"
            reason    <- "Paired samples — normality assumed, n >= 30"
          } else {
            result    <- tryCatch(stats::t.test(g1, g2, paired = TRUE), error = function(e) NULL)
            test_name <- "Paired t-test"
            reason    <- "Paired samples — normality assumed, n < 30"
          }

        } else {
          if (!is_normal) {
            result    <- tryCatch(stats::wilcox.test(g1, g2, paired = FALSE), error = function(e) NULL)
            test_name <- "Wilcoxon-Mann-Whitney test"
            reason    <- "Independent samples — normality not assumed"
          } else if (!is_homoscedastic) {
            result    <- tryCatch(stats::t.test(g1, g2, var.equal = FALSE), error = function(e) NULL)
            test_name <- "Welch's t-test"
            reason    <- "Independent samples — normality assumed, homoscedasticity not assumed"
          } else if (n_min >= 30) {
            result    <- tryCatch(stats::t.test(g1, g2, var.equal = TRUE), error = function(e) NULL)
            test_name <- "Z-test (t.test approximation, n >= 30)"
            reason    <- "Independent samples — normality assumed, homoscedasticity assumed, n >= 30"
          } else {
            result    <- tryCatch(stats::t.test(g1, g2, var.equal = TRUE), error = function(e) NULL)
            test_name <- "Student's t-test"
            reason    <- "Independent samples — normality assumed, homoscedasticity assumed, n < 30"
          }
        }

        if (is.null(result)) return(NULL)
        stat_val <- round(as.numeric(result$statistic), 4)
        p_val    <- round(result$p.value, 4)

      } else {
        if (!is_normal || !is_homoscedastic) {
          result    <- tryCatch(stats::kruskal.test(formula_obj, data = df), error = function(e) NULL)
          test_name <- "Kruskal-Wallis test"
          reason    <- if (!is_normal)
            "More than 2 conditions — normality not assumed"
          else
            "More than 2 conditions — normality assumed but homoscedasticity not assumed"
        } else {
          fit       <- tryCatch(aov(formula_obj, data = df), error = function(e) NULL)
          if (is.null(fit)) return(NULL)
          result    <- tryCatch(summary(fit), error = function(e) NULL)
          test_name <- "One-way ANOVA"
          reason    <- "More than 2 conditions — normality and homoscedasticity assumed"
          attr(result, "aov_model") <- fit
        }

        if (is.null(result)) return(NULL)
        if (test_name == "One-way ANOVA") {
          stat_val <- round(result[[1]][["F value"]][1], 4)
          p_val    <- round(result[[1]][["Pr(>F)"]][1], 4)
        } else {
          stat_val <- round(as.numeric(result$statistic), 4)
          p_val    <- round(result$p.value, 4)
        }
      }

      is_significant <- p_val < 0.05

      tbl <- data.frame(
        Test           = test_name,
        Reason         = reason,
        Statistic      = stat_val,
        p_value        = p_val,
        Interpretation = if (is_significant)
          "p < 0.05: at least one group differs from the others"
        else
          "p >= 0.05: no significant difference between groups",
        Decision       = if (is_significant) "H0 rejected" else "H0 not rejected",
        stringsAsFactors = FALSE
      )

      list(table = tbl, test_name = test_name, reason = reason,
           p_value = p_val, statistic = stat_val,
           is_significant = is_significant, result_obj = result)
    }

    run_posthoc_tests <- function(df, value_col, group_col, test_name,
                                   comparison_type, control_condition = NULL) {
      if (is.null(df) || !nrow(df)) return(NULL)

      df[[group_col]] <- factor(df[[group_col]])
      formula_obj     <- as.formula(paste(value_col, "~", group_col))
      n_per_group     <- as.integer(table(df[[group_col]]))
      is_balanced     <- length(unique(n_per_group)) == 1

      if (grepl("ANOVA", test_name, ignore.case = TRUE)) {
        if (comparison_type == "vs_control" &&
            !is.null(control_condition) && nzchar(control_condition)) {
          df[[group_col]] <- relevel(df[[group_col]], ref = control_condition)
          model <- tryCatch(aov(formula_obj, data = df), error = function(e) NULL)
          if (is.null(model)) return(NULL)

          mcp_spec      <- setNames(list("Dunnett"), group_col)
          contrasts_obj <- tryCatch(
            multcomp::glht(model, linfct = do.call(multcomp::mcp, mcp_spec)),
            error = function(e) NULL
          )

          if (!is.null(contrasts_obj)) {
            s   <- summary(contrasts_obj)
            tbl <- data.frame(
              Comparison  = names(s$test$coefficients),
              Estimate    = round(s$test$coefficients, 4),
              Std_Error   = round(s$test$sigma, 4),
              z_value     = round(s$test$tstat, 4),
              adj_p_value = round(s$test$pvalues, 4),
              Significant = s$test$pvalues < 0.05,
              stringsAsFactors = FALSE
            )
            return(list(table = tbl, method = "Dunnett test (multcomp)", is_balanced = is_balanced))
          }

          # Fallback: Bonferroni filtered to control comparisons
          model2 <- tryCatch(aov(formula_obj, data = df), error = function(e) NULL)
          if (is.null(model2)) return(NULL)
          pw  <- tryCatch(TukeyHSD(model2), error = function(e) NULL)
          if (is.null(pw)) return(NULL)
          raw <- as.data.frame(pw[[1]])
          raw$comparison <- rownames(raw)
          raw <- raw[grepl(control_condition, raw$comparison, fixed = TRUE), ]
          adj <- p.adjust(raw[["p adj"]], method = "bonferroni")
          tbl <- data.frame(
            Comparison  = raw$comparison,
            Estimate    = round(raw$diff, 4),
            adj_p_value = round(adj, 4),
            Significant = adj < 0.05,
            stringsAsFactors = FALSE
          )
          return(list(table = tbl,
                      method = "Bonferroni pairwise (Dunnett fallback, control comparisons)",
                      is_balanced = is_balanced))

        } else {
          model <- tryCatch(aov(formula_obj, data = df), error = function(e) NULL)
          if (is.null(model)) return(NULL)

          if (is_balanced) {
            tukey <- tryCatch(TukeyHSD(model), error = function(e) NULL)
            if (is.null(tukey)) return(NULL)
            raw <- as.data.frame(tukey[[1]])
            tbl <- data.frame(
              Comparison  = rownames(raw),
              Estimate    = round(raw$diff, 4),
              Lower_CI    = round(raw$lwr, 4),
              Upper_CI    = round(raw$upr, 4),
              adj_p_value = round(raw[["p adj"]], 4),
              Significant = raw[["p adj"]] < 0.05,
              stringsAsFactors = FALSE
            )
            return(list(table = tbl, method = "Tukey HSD (balanced design)", is_balanced = TRUE))
          } else {
            result <- tryCatch(
              stats::pairwise.t.test(df[[value_col]], df[[group_col]],
                                     p.adjust.method = "bonferroni"),
              error = function(e) NULL
            )
            if (is.null(result)) return(NULL)
            p_mat <- result$p.value
            idx   <- which(!is.na(p_mat), arr.ind = TRUE)
            tbl <- data.frame(
              Comparison  = paste(rownames(p_mat)[idx[, 1]], "vs", colnames(p_mat)[idx[, 2]]),
              adj_p_value = round(p_mat[idx], 4),
              Significant = p_mat[idx] < 0.05,
              stringsAsFactors = FALSE
            )
            return(list(table = tbl,
                        method = "Bonferroni pairwise t-test (unbalanced design)",
                        is_balanced = FALSE))
          }
        }

      } else if (grepl("Kruskal", test_name, ignore.case = TRUE)) {
        result <- tryCatch(
          rstatix::dunn_test(df, formula_obj, p.adjust.method = "bonferroni"),
          error = function(e) NULL
        )
        if (is.null(result)) return(NULL)

        tbl <- data.frame(
          Comparison  = paste(result$group1, "vs", result$group2),
          Statistic   = round(result$statistic, 4),
          adj_p_value = round(result$p.adj, 4),
          Significant = result$p.adj < 0.05,
          stringsAsFactors = FALSE
        )

        if (comparison_type == "vs_control" &&
            !is.null(control_condition) && nzchar(control_condition)) {
          tbl <- tbl[grepl(control_condition, tbl$Comparison, fixed = TRUE), , drop = FALSE]
        }

        return(list(table = tbl,
                    method = "Dunn test with Bonferroni correction (rstatix)",
                    is_balanced = is_balanced))
      }

      NULL
    }

    # ==================================================================
    # Statistical diagnostic plots (no Shiny dependency)
    # ==================================================================

    plot_residual_qq <- function(residuals, theme_fn = light_theme, sub_df = NULL) {
      rank_order <- order(residuals)
      df_qq <- data.frame(
        resid       = residuals[rank_order],
        theoretical = stats::qnorm(stats::ppoints(length(residuals)))
      )
      if (!is.null(sub_df) && nrow(sub_df) == length(residuals)) {
        df_qq$condition <- as.character(sub_df$condition_grouped)[rank_order]
        df_qq$animal    <- as.character(sub_df$animal)[rank_order]
        df_qq$plate     <- as.character(sub_df$plate_id)[rank_order]
        df_qq$tooltip   <- paste0(
          "Condition: ", df_qq$condition,
          "<br>Animal: ", df_qq$animal,
          "<br>Plate: ", df_qq$plate,
          "<br>Residual: ", sprintf("%.4f", df_qq$resid)
        )
      } else {
        df_qq$tooltip <- paste0("Residual: ", sprintf("%.4f", df_qq$resid))
      }
      df_qq$.id <- paste0("qq_", seq_len(nrow(df_qq)))

      ggplot2::ggplot(df_qq, ggplot2::aes(x = theoretical, y = resid)) +
        ggplot2::stat_qq_line(ggplot2::aes(sample = resid),
                              colour = "red", linewidth = 0.8, linetype = "dashed") +
        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = tooltip, data_id = .id),
          colour = "#339989", alpha = 0.7, size = 2,
          hover_css = "r:5!important;opacity:1!important;"
        ) +
        ggplot2::labs(
          title = "QQ-Plot of Residuals",
          x     = "Theoretical Quantiles",
          y     = "Sample Quantiles"
        ) +
        theme_fn()
    }

    plot_residuals_vs_fitted <- function(model, theme_fn = light_theme, sub_df = NULL) {
      df_diag <- data.frame(
        fitted = fitted(model),
        resid  = residuals(model)
      )
      if (!is.null(sub_df) && nrow(sub_df) == nrow(df_diag)) {
        df_diag$condition <- as.character(sub_df$condition_grouped)
        df_diag$animal    <- as.character(sub_df$animal)
        df_diag$plate     <- as.character(sub_df$plate_id)
        df_diag$tooltip   <- paste0(
          "Condition: ", df_diag$condition,
          "<br>Animal: ", df_diag$animal,
          "<br>Plate: ", df_diag$plate,
          "<br>Fitted: ", sprintf("%.4f", df_diag$fitted),
          "<br>Residual: ", sprintf("%.4f", df_diag$resid)
        )
      } else {
        df_diag$tooltip <- paste0(
          "Fitted: ", sprintf("%.4f", df_diag$fitted),
          "<br>Residual: ", sprintf("%.4f", df_diag$resid)
        )
      }
      df_diag$.id <- paste0("fit_", seq_len(nrow(df_diag)))

      ggplot2::ggplot(df_diag, ggplot2::aes(x = fitted, y = resid)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            colour = "red", linewidth = 0.8) +
        ggplot2::geom_smooth(method = "loess", se = FALSE, colour = "steelblue",
                             linewidth = 0.7, formula = y ~ x) +
        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = tooltip, data_id = .id),
          colour = "#339989", alpha = 0.7, size = 2,
          hover_css = "r:5!important;opacity:1!important;"
        ) +
        ggplot2::labs(
          title = "Residuals vs Fitted",
          x     = "Fitted Values",
          y     = "Residuals"
        ) +
        theme_fn()
    }

    plot_residuals_vs_leverage <- function(model, theme_fn = light_theme, sub_df = NULL) {
      h     <- hatvalues(model)
      r_std <- tryCatch(rstandard(model), error = function(e) residuals(model))
      cooks <- tryCatch(cooks.distance(model),
                        error = function(e) rep(NA_real_, length(h)))

      df_lev <- data.frame(leverage = h, std_resid = r_std, cooks_dist = cooks)
      if (!is.null(sub_df) && nrow(sub_df) == nrow(df_lev)) {
        df_lev$condition <- as.character(sub_df$condition_grouped)
        df_lev$animal    <- as.character(sub_df$animal)
        df_lev$plate     <- as.character(sub_df$plate_id)
      }
      df_lev <- df_lev[is.finite(df_lev$leverage) & is.finite(df_lev$std_resid), ]
      if (!nrow(df_lev)) return(NULL)

      has_meta <- all(c("condition", "animal", "plate") %in% names(df_lev))
      df_lev$tooltip <- if (has_meta) {
        paste0(
          "Condition: ", df_lev$condition,
          "<br>Animal: ", df_lev$animal,
          "<br>Plate: ", df_lev$plate,
          "<br>Leverage: ", sprintf("%.4f", df_lev$leverage),
          "<br>Std. Residual: ", sprintf("%.4f", df_lev$std_resid),
          "<br>Cook's dist.: ", ifelse(is.finite(df_lev$cooks_dist),
                                       sprintf("%.4f", df_lev$cooks_dist), "NA")
        )
      } else {
        paste0(
          "Leverage: ", sprintf("%.4f", df_lev$leverage),
          "<br>Std. Residual: ", sprintf("%.4f", df_lev$std_resid),
          "<br>Cook's dist.: ", ifelse(is.finite(df_lev$cooks_dist),
                                       sprintf("%.4f", df_lev$cooks_dist), "NA")
        )
      }
      df_lev$.id <- paste0("lev_", seq_len(nrow(df_lev)))

      ggplot2::ggplot(df_lev, ggplot2::aes(x = leverage, y = std_resid)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            colour = "red", linewidth = 0.8) +
        ggiraph::geom_point_interactive(
          ggplot2::aes(size = cooks_dist, tooltip = tooltip, data_id = .id),
          colour = "#339989", alpha = 0.7,
          hover_css = "opacity:1!important;"
        ) +
        ggplot2::scale_size_continuous(name = "Cook's distance", range = c(1, 8)) +
        ggplot2::labs(
          title = "Residuals vs Leverage",
          x     = "Leverage",
          y     = "Standardised Residuals"
        ) +
        theme_fn()
    }

    plot_boxplot_by_condition <- function(df, value_col, group_col,
                                           theme_fn = light_theme) {
      if (is.null(df) || !nrow(df)) return(NULL)
      if (!value_col %in% names(df) || !group_col %in% names(df)) return(NULL)

      has_animal <- "animal"   %in% names(df)
      has_plate  <- "plate_id" %in% names(df)
      df$.id <- paste0("bx_", seq_len(nrow(df)))
      df$tooltip <- paste0(
        "Condition: ", as.character(df[[group_col]]),
        if (has_animal) paste0("<br>Animal: ", as.character(df$animal))   else "",
        if (has_plate)  paste0("<br>Plate: ",  as.character(df$plate_id)) else "",
        "<br>Value: ", sprintf("%.4f", as.numeric(df[[value_col]]))
      )

      ggplot2::ggplot(df,
        ggplot2::aes(x = .data[[group_col]], y = .data[[value_col]],
                     fill = .data[[group_col]])) +
        ggplot2::geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.8) +
        ggiraph::geom_point_interactive(
          ggplot2::aes(tooltip = tooltip, data_id = .id),
          position = ggplot2::position_jitter(width = 0.15, seed = 42),
          alpha = 0.6, size = 1.8, colour = "grey30",
          hover_css = "r:5!important;opacity:1!important;"
        ) +
        ggplot2::labs(
          title = paste("Distribution of", value_col, "by condition"),
          x     = NULL,
          y     = value_col
        ) +
        theme_fn() +
        ggplot2::theme(
          axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1),
          legend.position = "none"
        )
    }

    # ==================================================================
    # Statistics: UI population + main observer + output renderers
    # ==================================================================

    shiny::observe({
      pt   <- input$stat_plot_type
      rvar <- input$stat_response_var
      if (is.null(pt) || is.null(rvar) || !nzchar(rvar)) return()

      df_list <- switch(pt,
        "boxplot_periods"  = rv$all_zone_combined_light_dark_boxplots,
        "boxplot_cumulate" = rv$all_zone_combined_cum_boxplots,
        "boxplot_delta"    = rv$all_zone_combined_delta_boxplots,
        "lineplot"         = rv$all_zone_combined_lineplots,
        NULL
      )
      df <- get_statistics_dataset(df_list, rvar)
      if (is.null(df) || !nrow(df)) return()

      zones <- as.character(sort(unique(df$zone)))
      conds <- sort(unique(as.character(
        df$condition_grouped[!is.na(df$condition_grouped) & nzchar(df$condition_grouped)]
      )))

      cur_zone <- isolate(input$stat_zone)
      shiny::updateSelectInput(session, "stat_zone",
        choices  = zones,
        selected = if (!is.null(cur_zone) && cur_zone %in% zones) cur_zone else zones[1]
      )
      cur_ctrl <- isolate(input$stat_control_condition)
      shiny::updateSelectInput(session, "stat_control_condition",
        choices  = conds,
        selected = if (!is.null(cur_ctrl) && cur_ctrl %in% conds) cur_ctrl else conds[1]
      )
    })

    shiny::observeEvent(input$run_statistics, {
      tryCatch({
        pt        <- isolate(input$stat_plot_type)
        rvar      <- isolate(input$stat_response_var)
        zone_sel  <- isolate(input$stat_zone)
        comp_type <- isolate(input$stat_comparison_type)
        ctrl_cond <- if (identical(comp_type, "vs_control"))
                       isolate(input$stat_control_condition) else NULL
        samp_type <- isolate(input$stat_sample_type)
        theme_fn  <- get_theme(isolate(input$theme_switch))$fn

        if (is.null(rvar) || !nzchar(rvar)) {
          log("❌ Statistics: select a response variable first.")
          return()
        }

        df_list <- switch(pt,
          "boxplot_periods"  = rv$all_zone_combined_light_dark_boxplots,
          "boxplot_cumulate" = rv$all_zone_combined_cum_boxplots,
          "boxplot_delta"    = rv$all_zone_combined_delta_boxplots,
          "lineplot"         = rv$all_zone_combined_lineplots,
          NULL
        )

        df <- get_statistics_dataset(df_list, rvar)
        if (is.null(df) || !nrow(df)) {
          log("❌ Statistics: no dataset found. Generate datasets first.")
          return()
        }

        if (!is.null(zone_sel) && nzchar(zone_sel))
          df <- df[!is.na(df$zone) & as.character(df$zone) == zone_sel, , drop = FALSE]

        value_col <- switch(pt,
          "boxplot_periods"  = "mean_val",
          "boxplot_cumulate" = "cum",
          "boxplot_delta"    = "mean_val",
          "lineplot"         = "var_value_per_well",
          NULL
        )

        if (is.null(value_col) || !value_col %in% names(df)) {
          log("❌ Statistics: expected value column not found in dataset.")
          return()
        }

        group_col <- "condition_grouped"

        if (pt == "lineplot") {
          df <- as.data.frame(
            dplyr::summarise(
              dplyr::group_by(df, animal, condition_grouped, zone, plate_id),
              var_value_per_well = sum(var_value_per_well, na.rm = TRUE),
              .groups = "drop"
            )
          )
        }

        if (pt == "boxplot_periods" && "period_without_numbers" %in% names(df)) {
          sub_groups   <- sort(unique(as.character(df$period_without_numbers)))
          group_within <- "period_without_numbers"
        } else if (pt == "boxplot_delta" && "transition_phase" %in% names(df)) {
          df$stats_phase <- dplyr::recode(
            sub(".*_", "", df$transition_phase),
            before = "Before", switch = "Switch", after = "After",
            .default = NA_character_
          )
          df           <- df[!is.na(df$stats_phase), , drop = FALSE]
          sub_groups   <- intersect(c("Before", "Switch", "After"),
                                     unique(df$stats_phase))
          group_within <- "stats_phase"
        } else {
          sub_groups   <- "all"
          group_within <- NULL
        }

        log("⏳ Running statistics...")

        results_list <- stats::setNames(
          lapply(sub_groups, function(grp) {
            sub_df <- if (grp == "all" || is.null(group_within)) df else
              df[!is.na(df[[group_within]]) & df[[group_within]] == grp, , drop = FALSE]

            sub_df <- sub_df[
              !is.na(sub_df[[value_col]]) &
              !is.na(sub_df[[group_col]]) &
              nzchar(as.character(sub_df[[group_col]])), , drop = FALSE
            ]

            if (!nrow(sub_df) || length(unique(sub_df[[group_col]])) < 2) return(NULL)

            norm  <- check_normality(sub_df, value_col, group_col)
            if (is.null(norm)) return(NULL)

            homo    <- check_homoscedasticity(sub_df, value_col, group_col,
                                               norm$is_normal, samp_type)
            is_homo <- if (is.null(homo)) TRUE else homo$is_homoscedastic

            infer <- select_inferential_test(sub_df, value_col, group_col,
                                              norm$is_normal, is_homo, samp_type)

            posthoc <- NULL
            if (!is.null(infer) && infer$is_significant &&
                length(unique(sub_df[[group_col]])) > 2) {
              posthoc <- tryCatch(
                run_posthoc_tests(sub_df, value_col, group_col,
                                   infer$test_name, comp_type, ctrl_cond),
                error = function(e) NULL
              )
            }

            gsize       <- as.data.frame(table(sub_df[[group_col]]))
            names(gsize) <- c("Condition", "N")
            is_balanced  <- length(unique(gsize$N)) == 1

            list(
              group            = grp,
              group_sizes      = gsize,
              is_balanced      = is_balanced,
              normality        = norm,
              homoscedasticity = homo,
              inferential      = infer,
              posthoc          = posthoc,
              plots = list(
                qq       = tryCatch(plot_residual_qq(norm$residuals, theme_fn, sub_df = sub_df),        error = function(e) NULL),
                fitted   = tryCatch(plot_residuals_vs_fitted(norm$model, theme_fn, sub_df = sub_df),   error = function(e) NULL),
                leverage = tryCatch(plot_residuals_vs_leverage(norm$model, theme_fn, sub_df = sub_df), error = function(e) NULL),
                boxplot  = tryCatch(plot_boxplot_by_condition(sub_df, value_col,
                                                               group_col, theme_fn),                   error = function(e) NULL)
              )
            )
          }),
          sub_groups
        )

        results_list <- Filter(Negate(is.null), results_list)

        if (!length(results_list)) {
          log("❌ Statistics: insufficient data (need >= 2 conditions with >= 3 observations).")
          return()
        }

        rv$stats_results <- list(
          results         = results_list,
          plot_type       = pt,
          response_var    = rvar,
          zone            = zone_sel,
          comparison_type = comp_type,
          control         = ctrl_cond,
          sample_type     = samp_type
        )

        shiny::updateTabsetPanel(session, "output_tabs", selected = "statistics")
        log(sprintf("✅ Statistics complete (%d group(s) analysed).", length(results_list)))

      }, error = function(e) {
        log(paste("❌ Statistics failed:", e$message))
      })
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$stat_plot_type, {
      rv$stats_results <- NULL
    }, ignoreInit = TRUE)

    output$stats_tables_ui <- shiny::renderUI({
      res <- rv$stats_results
      if (is.null(res)) {
        return(shiny::div(
          style = "padding:20px; color:#888; text-align:center;",
          shiny::p("No statistics computed yet."),
          shiny::p("Select a plot type, response variable and zone, then click 'Run Statistics'.")
        ))
      }

      sections <- lapply(seq_along(res$results), function(i) {
        grp <- names(res$results)[i]
        r   <- res$results[[grp]]

        local({
          .i <- i; .r <- r

          output[[paste0("stats_sizes_tbl_",   .i)]] <- shiny::renderTable(
            .r$group_sizes, striped = TRUE, bordered = TRUE, hover = TRUE)

          output[[paste0("stats_norm_tbl_",    .i)]] <- shiny::renderTable(
            .r$normality$table, striped = TRUE, bordered = TRUE, hover = TRUE)

          if (!is.null(.r$homoscedasticity))
            output[[paste0("stats_homo_tbl_",  .i)]] <- shiny::renderTable(
              .r$homoscedasticity$table, striped = TRUE, bordered = TRUE, hover = TRUE)

          if (!is.null(.r$inferential))
            output[[paste0("stats_infer_tbl_", .i)]] <- shiny::renderTable(
              .r$inferential$table, striped = TRUE, bordered = TRUE, hover = TRUE)

          if (!is.null(.r$posthoc))
            output[[paste0("stats_posthoc_tbl_", .i)]] <- DT::renderDataTable({
              dt <- DT::datatable(.r$posthoc$table,
                                   options = list(pageLength = 25, scrollX = TRUE))
              if ("Significant" %in% names(.r$posthoc$table))
                dt <- DT::formatStyle(dt, "Significant",
                       backgroundColor = DT::styleEqual(c(TRUE, FALSE),
                                                         c("#b3ffb3", "#ffb3b3")))
              dt
            })
        })

        is_sig           <- !is.null(r$inferential) && r$inferential$is_significant
        infer_badge_bg   <- if (is_sig) "#fff3cd" else "#d4edda"
        infer_badge_bord <- if (is_sig) "#ffc107"  else "#28a745"
        infer_badge_txt  <- if (is_sig)
          paste0("Significant difference detected (p = ", r$inferential$p_value, ")")
        else if (!is.null(r$inferential))
          paste0("No significant difference detected (p = ", r$inferential$p_value, ")")
        else
          "Analysis could not be completed."

        n_sig_ph         <- if (!is.null(r$posthoc) && "Significant" %in% names(r$posthoc$table))
          sum(r$posthoc$table$Significant, na.rm = TRUE) else 0L
        n_total_ph       <- if (!is.null(r$posthoc)) nrow(r$posthoc$table) else 0L
        posthoc_badge_bg   <- if (n_sig_ph > 0) "#fff3cd" else "#d4edda"
        posthoc_badge_bord <- if (n_sig_ph > 0) "#ffc107"  else "#28a745"
        posthoc_badge_txt  <- paste0(
          n_sig_ph, " / ", n_total_ph,
          " comparison(s) significant after correction (p_adj < 0.05)."
        )

        shiny::tagList(
          if (grp != "all")
            shiny::h4(shiny::strong(paste("—", grp))) else NULL,

          shiny::h5("Group Sizes"),
          shiny::p(if (r$is_balanced) "Design: balanced." else "Design: unbalanced."),
          shiny::tableOutput(ns(paste0("stats_sizes_tbl_", i))),
          shiny::hr(),

          shiny::h5("1. Normality Test"),
          shiny::div(class = "well well-sm",
            shiny::p(shiny::strong("H0:"), " Residuals follow a normal distribution (p > 0.05)."),
            shiny::p(shiny::strong("H1:"), " Residuals do NOT follow a normal distribution (p <= 0.05).")
          ),
          shiny::tableOutput(ns(paste0("stats_norm_tbl_", i))),

          if (!is.null(r$homoscedasticity)) shiny::tagList(
            shiny::hr(),
            shiny::h5("2. Homoscedasticity Test"),
            shiny::div(class = "well well-sm",
              shiny::p(shiny::strong("H0:"), " Variances are equal between groups (p > 0.05)."),
              shiny::p(shiny::strong("H1:"), " At least one group has a different variance (p <= 0.05).")
            ),
            shiny::tableOutput(ns(paste0("stats_homo_tbl_", i)))
          ) else NULL,

          if (!is.null(r$inferential)) shiny::tagList(
            shiny::hr(),
            shiny::h5(paste0(if (!is.null(r$homoscedasticity)) "3." else "2.", " Inferential Test")),
            shiny::div(class = "well well-sm",
              shiny::p(shiny::strong("H0:"), " There is no difference between the compared groups (p > 0.05)."),
              shiny::p(shiny::strong("H1:"), " At least one group differs from the others (p <= 0.05).")
            ),
            shiny::tableOutput(ns(paste0("stats_infer_tbl_", i))),
            shiny::div(
              style = paste0("padding:8px 14px; background:", infer_badge_bg,
                             "; border-left:4px solid ", infer_badge_bord, "; margin:8px 0;"),
              shiny::strong(infer_badge_txt)
            )
          ) else NULL,

          if (!is.null(r$posthoc)) shiny::tagList(
            shiny::hr(),
            shiny::h5(paste0("Post-hoc: ", r$posthoc$method)),
            shiny::p(shiny::em(paste("Design:",
              if (r$posthoc$is_balanced) "balanced" else "unbalanced"))),
            DT::dataTableOutput(ns(paste0("stats_posthoc_tbl_", i))),
            shiny::div(
              style = paste0("padding:8px 14px; background:", posthoc_badge_bg,
                             "; border-left:4px solid ", posthoc_badge_bord, "; margin:8px 0;"),
              shiny::strong(posthoc_badge_txt)
            )
          ) else NULL,

          shiny::div(style = "margin-bottom:28px;")
        )
      })

      do.call(shiny::tagList, sections)
    })

    output$stats_figures_ui <- shiny::renderUI({
      res <- rv$stats_results
      if (is.null(res)) {
        return(shiny::div(
          style = "padding:20px; color:#888; text-align:center;",
          shiny::p("No figures to display yet. Run statistics first.")
        ))
      }

      sections <- lapply(seq_along(res$results), function(i) {
        grp <- names(res$results)[i]
        r   <- res$results[[grp]]

        local({
          .i <- i; .r <- r
          if (!is.null(.r$plots$qq))
            output[[paste0("stats_qq_plot_",  .i)]] <- ggiraph::renderGirafe(
              to_girafe(.r$plots$qq, width_svg = 12, height_svg = 6))
          if (!is.null(.r$plots$fitted))
            output[[paste0("stats_fit_plot_", .i)]] <- ggiraph::renderGirafe(
              to_girafe(.r$plots$fitted, width_svg = 12, height_svg = 6))
          if (!is.null(.r$plots$leverage))
            output[[paste0("stats_lev_plot_", .i)]] <- ggiraph::renderGirafe(
              to_girafe(.r$plots$leverage, width_svg = 12, height_svg = 6))
          if (!is.null(.r$plots$boxplot))
            output[[paste0("stats_box_plot_", .i)]] <- ggiraph::renderGirafe(
              to_girafe(.r$plots$boxplot, width_svg = 12, height_svg = 6))
        })

        shiny::tagList(
          if (grp != "all")
            shiny::h4(shiny::strong(paste("—", grp))) else NULL,
          if (!is.null(r$plots$qq)) shiny::tagList(
            shiny::h5("QQ-Plot of Residuals"),
            ggiraph::girafeOutput(ns(paste0("stats_qq_plot_",  i)), width = "100%", height = "400px")
          ) else NULL,
          if (!is.null(r$plots$fitted)) shiny::tagList(
            shiny::h5("Residuals vs Fitted"),
            ggiraph::girafeOutput(ns(paste0("stats_fit_plot_", i)), width = "100%", height = "400px")
          ) else NULL,
          if (!is.null(r$plots$leverage)) shiny::tagList(
            shiny::h5("Residuals vs Leverage"),
            ggiraph::girafeOutput(ns(paste0("stats_lev_plot_", i)), width = "100%", height = "400px")
          ) else NULL,
          if (!is.null(r$plots$boxplot)) shiny::tagList(
            shiny::h5("Distribution by Condition"),
            ggiraph::girafeOutput(ns(paste0("stats_box_plot_", i)), width = "100%", height = "400px")
          ) else NULL,
          shiny::div(style = "margin-bottom:28px;")
        )
      })

      do.call(shiny::tagList, sections)
    })

    onStop(function() {
      rv$plot_gg <- NULL
      rv$plot_girafe <- NULL
    })
  })
}

# ======================================================================
# End of visualization_module.R
# ======================================================================
