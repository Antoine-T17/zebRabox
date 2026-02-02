# ======================================================================
# ui.R – Zebrabox Treatment Dashboard (v1.0)
# ======================================================================

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(fresh)

# ======================================================================
# 1) Base Theme (Light Mode)
# ======================================================================
base_theme <- fresh::create_theme(
  theme = "default",
  fresh::bs_vars_global(
    body_bg    = "#FFF",
    text_color = "#000"
  ),
  fresh::bs_vars_wells(
    bg     = "#F8F9FA",
    border = "#DEE2E6"
  ),
  fresh::bs_vars_button(
    default_bg     = "#2196F3",
    default_color  = "#FFF",
    default_border = "#2196F3"
  )
)

# ======================================================================
# 2) Custom CSS & JavaScript (Global Styles + Theme Toggle)
# ======================================================================
header_styles <- shiny::tags$head(
  # ------------------- CSS -------------------
  shiny::tags$style(shiny::HTML('
    /* Theme Toggle Switch */
    .switch {
      position: relative;
      display: inline-block;
      width: 50px;
      height: 28px;
    }
    .switch input {
      opacity: 0;
      width: 0;
      height: 0;
    }
    .slider {
      position: absolute;
      cursor: pointer;
      top: 0; left: 0; right: 0; bottom: 0;
      background-color: #ccc;
      transition: .4s;
      border-radius: 24px;
    }
    .slider:before {
      position: absolute;
      content: "";
      height: 20px;
      width: 20px;
      left: 4px;
      bottom: 4px;
      background-color: white;
      transition: .4s;
      border-radius: 50%;
    }
    input:checked + .slider {
      background-color: #2196F3;
    }
    input:checked + .slider:before {
      transform: translateX(22px);
    }

    /* Exit Button (Red Circle) */
    .exit-button-circle {
      background-color: #a31f15;
      color: #fff;
      border: none;
      border-radius: 50%;
      width: 35px;
      height: 35px;
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: 18px;
      cursor: pointer;
      margin: 0 !important;
      padding: 0 !important;
    }
    .exit-button-circle:hover {
      background-color: #861810;
    }

    /* Console Output */
    .console-container pre {
      background-color: #FFF !important;
      color: #000 !important;
      border: 1px solid #ccc !important;
      padding: 12px !important;
      border-radius: 6px !important;
      font-family: monospace !important;
      font-size: 0.95em !important;
      margin: 0 !important;
      white-space: pre-wrap !important;
      word-wrap: break-word !important;
      height: 100% !important;
      overflow: auto !important;
    }
    [data-theme="dark"] .console-container pre {
      background-color: #2E2E2E !important;
      color: #FFF !important;
      border: 1px solid #444 !important;
    }

    /* Light Theme */
    .main-header { background-color: #FFF !important; color: #000 !important; }
    .main-sidebar { background-color: #F8F9FA !important; color: #000 !important; }
    .main-sidebar .sidebar-menu li a { color: #000 !important; transition: transform 0.3s ease, box-shadow 0.3s ease !important; }
    .main-sidebar .sidebar-menu li.active a { background-color: #E0E0E0 !important; }
    .main-sidebar .sidebar-menu li a:hover {
      background-color: #2196F3 !important;
      color: #FFF !important;
      transform: translateY(-3px) !important;
      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15) !important;
    }
    .content-wrapper { background-color: #FFF !important; }

    /* Boxes (Cards) */
    .box {
      border-radius: 15px !important;
      background-color: #FFF !important;
      border: 2px solid #DEE2E6 !important;
      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1) !important;
      transition: transform 0.3s ease, box-shadow 0.3s ease !important;
      padding: 15px !important;
    }
    .box:hover {
      transform: translateY(-5px) !important;
      box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2) !important;
    }
    .box:hover i { transform: scale(1.2) !important; }
    .box i { transition: transform 0.5s ease !important; }
    .box .details { display: none; font-size: 0.95em; color: #555; margin-top: 1em; }
    .box:hover .details { display: block; }

    /* Tabs in Boxes */
    .box .nav-tabs {
      background-color: transparent !important;
      border-bottom: 2px solid #DEE2E6 !important;
    }
    .box .nav-tabs li a {
      color: #000 !important;
      background-color: transparent !important;
      border: none !important;
      padding: 10px 20px !important;
    }
    .box .nav-tabs li a:hover,
    .box .nav-tabs li.active a {
      background-color: #2196F3 !important;
      color: #FFF !important;
    }
    .box .tab-content {
      background-color: #F8F9FA !important;
      border-radius: 0 0 15px 15px !important;
      padding: 15px !important;
    }
    
    /* Delta Time Explorer title (light & dark) */
    .delta-time-title { color: #2c3e50; }
    [data-theme="dark"] .delta-time-title { color: #FFF !important; }
    
    /* Delta Time help text (light & dark) */
    .delta-time-help { color: #555; }
    [data-theme="dark"] .delta-time-help { color: #DDD !important; }
    
    /* Dark Theme */
    [data-theme="dark"] .main-header { background-color: #222d32 !important; color: #FFF !important; }
    [data-theme="dark"] .main-sidebar { background-color: #222d32 !important; color: #FFF !important; }
    [data-theme="dark"] .main-sidebar .sidebar-menu li a { color: #FFF !important; }
    [data-theme="dark"] .main-sidebar .sidebar-menu li.active a { background-color: #1a2226 !important; }
    [data-theme="dark"] .main-sidebar .sidebar-menu li a:hover {
      background-color: #2196F3 !important;
      transform: translateY(-3px) !important;
      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.4) !important;
    }
    [data-theme="dark"] .content-wrapper { background-color: #2E2E2E !important; }

    [data-theme="dark"] .box {
      background-color: #2E2E2E !important;
      border: 2px solid #444 !important;
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.4) !important;
    }
    [data-theme="dark"] .box:hover { box-shadow: 0 8px 20px rgba(0, 0, 0, 0.6) !important; }
    [data-theme="dark"] .box .details { color: #BBB; }
    [data-theme="dark"] .box .nav-tabs { border-bottom-color: #444 !important; }
    [data-theme="dark"] .box .nav-tabs li a { color: #FFF !important; }
    [data-theme="dark"] .box .tab-content { background-color: #2E2E2E !important; }

    [data-theme="dark"] pre,
    [data-theme="dark"] .shiny-text-output,
    [data-theme="dark"] .shiny-output-error {
      background-color: #2E2E2E !important;
      color: #FFF !important;
      border: 1px solid #444 !important;
      padding: 10px;
      border-radius: 5px;
    }

    [data-theme="dark"] .well {
      background-color: #444 !important;
      border-color: #666 !important;
      color: #FFF !important;
    }

    [data-theme="dark"] body,
    [data-theme="dark"] .content-wrapper,
    [data-theme="dark"] .main-sidebar,
    [data-theme="dark"] .box,
    [data-theme="dark"] .well,
    [data-theme="dark"] .nav > li > a,
    [data-theme="dark"] .tab-content,
    [data-theme="dark"] .box-header,
    [data-theme="dark"] .box-body {
      color: #FFF !important;
    }

    /* DataTables Dark Mode */
    [data-theme="dark"] .dataTables_wrapper { background-color: #2E2E2E !important; color: #FFF !important; }
    [data-theme="dark"] .dataTables_wrapper table { background-color: #2E2E2E !important; }
    [data-theme="dark"] .dataTables_wrapper .dataTable th,
    [data-theme="dark"] .dataTables_wrapper .dataTable td { color: #FFF !important; border-color: #444 !important; }
    [data-theme="dark"] .dataTables_wrapper .dataTables_filter input,
    [data-theme="dark"] .dataTables_wrapper .dataTables_length select {
      background-color: #444 !important;
      color: #FFF !important;
      border: 1px solid #666 !important;
    }
    [data-theme="dark"] .dataTables_wrapper .dataTables_paginate .paginate_button {
      background-color: #444 !important;
      color: #FFF !important;
    }
    [data-theme="dark"] .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
      background-color: #2196F3 !important;
    }

    /* Footer Global */
    footer {
      background-color: inherit !important;
      margin-top: 60px;
      padding: 30px 0;
      border-top: 1px solid #ddd;
      text-align: center;
    }
    [data-theme="dark"] footer {
      border-top-color: #555 !important;
    }

    .footer-nav-hint {
      font-size: 1.2em;
      margin-bottom: 1.5em;
      font-style: italic;
      color: #777;
    }
    [data-theme="dark"] .footer-nav-hint {
      color: #ccc;
    }

    .footer-line {
      width: 80%;
      max-width: 700px;
      height: 1px;
      background: #ddd;
      margin: 40px auto;
      border: none;
    }
    [data-theme="dark"] .footer-line {
      background: #555;
    }

    .app-footer {
      font-size: 0.95em;
      color: #777;
      transition: color 0.4s ease;
    }
    [data-theme="dark"] .app-footer {
      color: #aaa;
    }
    .app-footer a {
      color: #2196F3;
      text-decoration: none;
      font-weight: 500;
    }
    .app-footer a:hover {
      text-decoration: underline;
    }

    /* Welcome Animations */
    @keyframes fadeIn {
      from { opacity: 0; transform: translateY(20px); }
      to   { opacity: 1; transform: translateY(0); }
    }
    .fade-in { animation: fadeIn 0.8s ease-out forwards; }
    .logo-fade { opacity: 0; animation-delay: 0.3s; }

    .welcome-subtitle {
      color: #555;
      transition: color 0.4s ease;
    }
    [data-theme="dark"] .welcome-subtitle {
      color: #e0e0e0 !important;
    }

    @media (max-width: 768px) {
      .logo-fade { gap: 30px; }
      .logo-fade img { height: 100px !important; width: auto; }
    }
  ')),

  # ------------------- JavaScript -------------------
  shiny::tags$script(shiny::HTML('
    function toggleTheme() {
      var body = document.body;
      var isDark = body.getAttribute("data-theme") === "dark";
      body.setAttribute("data-theme", isDark ? "light" : "dark");
      if (window.Shiny) {
        Shiny.setInputValue("themeChanged", !isDark);
      }
    }
  '))
)

# Copy logos to www/
file.copy("my_sticker_without_border.png", "www/my_sticker_without_border.png", overwrite = TRUE)
file.copy("viewpoint_logo_remove_background.png", "www/viewpoint_logo_remove_background.png", overwrite = TRUE)

# ======================================================================
# 3) Dashboard Header
# ======================================================================
header_items <- shinydashboard::dashboardHeader(
  title = "Zebrabox Treatment",
  shiny::tags$li(
    shiny::div(
      style = "display: flex; align-items: center; gap: 15px; margin-top: 6px; margin-right: 12px;",
      # Theme Toggle
      shiny::tags$label(
        class = "switch",
        shiny::tags$input(type = "checkbox", id = "themeToggle", onclick = "toggleTheme()"),
        shiny::tags$span(class = "slider"),
        shiny::tags$span(style = "margin-left: 8px;", shiny::icon("sun", class = "fa-lg"))
      ),
      # Help
      shiny::a(
        href = "https://github.com/Antoine-T17/zebRabox/tree/main?tab=readme-ov-file#zebrabox--from-raw-data-to-interactive-visualization",
        target = "_blank",
        shiny::icon("question-circle", class = "fa-2x"),
        style = "color: #2196F3; text-decoration: none;"
      ),
      # Reset
      shiny::actionButton(
        inputId = "reset_app",
        label = NULL,
        icon = shiny::icon("refresh"),
        class = "exit-button-circle",
        style = "background-color: #2196F3;"
      ),
      # Exit
      shiny::actionButton(
        inputId = "exit_app",
        label = NULL,
        icon = shiny::icon("power-off"),
        class = "exit-button-circle"
      )
    ),
    class = "dropdown"
  )
)

# ======================================================================
# 4) Sidebar Menu
# ======================================================================
sidebar_content <- shinydashboard::dashboardSidebar(
  shinyjs::useShinyjs(),
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem("Welcome",       tabName = "welcome",       icon = shiny::icon("home")),
    shinydashboard::menuItem("Plate Plan",    tabName = "plate_plan",    icon = shiny::icon("table")),
    shinydashboard::menuItem("Raw Data",      tabName = "raw_data",      icon = shiny::icon("file-import")),
    shinydashboard::menuItem("Processing",    tabName = "processing",    icon = shiny::icon("gears")),
    shinydashboard::menuItem("Visualization", tabName = "visualization", icon = shiny::icon("chart-line"))
  )
)

# ======================================================================
# 5) Welcome Tab Content
# ======================================================================
welcome_content <- shinydashboard::tabItem(
  tabName = "welcome",
  shiny::fluidRow(
    shiny::column(
      width = 12, align = "center",
      shiny::h1("Welcome to Zebrabox Experiment Pipeline",
                style = "font-size: 2.4em; font-weight: 700; margin: 20px 0 8px 0;"),
      shiny::p("Generate randomized plate layouts, import and process raw data, and visualize results in multiple modes.",
               class = "welcome-subtitle",
               style = "font-size: 1.38em; line-height: 1.6; margin-bottom: 35px; max-width: 920px; margin-left: auto; margin-right: auto; font-style: italic;"),
      
      shiny::div(
        class = "fade-in logo-fade",
        style = "display: flex; justify-content: center; align-items: center; gap: 60px; margin: 30px 0 60px 0; flex-wrap: wrap; opacity: 0;",
        shiny::a(href = "https://github.com/Antoine-T17/zebRabox/tree/main?tab=readme-ov-file#zebrabox--from-raw-data-to-interactive-visualization", target = "_blank",
                 shiny::tags$img(src = "my_sticker_without_border.png",
                                 style = "height: 130px; width: auto; transition: transform 0.4s ease; filter: drop-shadow(0 4px 8px rgba(0,0,0,0.1));",
                                 onmouseover = "this.style.transform='scale(1.12)'",
                                 onmouseout  = "this.style.transform='scale(1)'")),
        shiny::a(href = "https://www.viewpoint.fr/", target = "_blank",
                 shiny::tags$img(src = "viewpoint_logo_remove_background.png",
                                 style = "height: 115px; width: auto; transition: transform 0.4s ease; filter: drop-shadow(0 4px 8px rgba(0,0,0,0.1));",
                                 onmouseover = "this.style.transform='scale(1.12)'",
                                 onmouseout  = "this.style.transform='scale(1)'"))
      )
    )
  ),

  shiny::fluidRow(
    shiny::column(width = 3, shinydashboard::box(width = NULL, status = "primary",
      shiny::div(style = "text-align: center; margin-bottom: 1em;", shiny::icon("table", class = "fa-3x")),
      shiny::h4(shiny::strong("PLATE PLAN"), style = "text-align: center; text-transform: uppercase;"),
      shiny::p("Import or generate randomized plate layouts to minimize edge effects from well positioning."),
      shiny::div(class = "details", shiny::p("Randomization helps avoid camera overestimation due to reflections in border wells. Exclude borders if needed, create multiple plates with equitable condition distribution, and preview as tables or figures."))
    )),
    shiny::column(width = 3, shinydashboard::box(width = NULL, status = "primary",
      shiny::div(style = "text-align: center; margin-bottom: 1em;", shiny::icon("file-import", class = "fa-3x")),
      shiny::h4(shiny::strong("RAW DATA"), style = "text-align: center; text-transform: uppercase;"),
      shiny::p("Import Zebrabox outputs (.xlsx). Choose mode: Tracking or Quantization, Light/Dark or Vibration/Rest."),
      shiny::div(class = "details",
                 shiny::p("Tracking follows exact movements of organisms (e.g., one per well, multi-animal support). Quantization measures pixel changes for activity (e.g., Δ pixels above threshold). Light/Dark tests light responses; Vibration/Rest assesses sound/vibration reactions."),
                 shiny::p("For details, see the ", shiny::tags$a(href = "https://www.viewpoint.fr/upload/productBrochurePdf/catalogueaqua-compressed-6373a62793a3e038651827.pdf", "Zebrabox brochure", target = "_blank"), " or visit the ", tags$a(href = "https://www.viewpoint.fr/product/zebrafish/fish-behavior-monitoring/zebrabox", "official website", target = "_blank"), "."))
    )),
    shiny::column(width = 3, shinydashboard::box(width = NULL, status = "primary",
      shiny::div(style = "text-align: center; margin-bottom: 1em;", shiny::icon("gears", class = "fa-3x")),
      shiny::h4(shiny::strong("PROCESSING"), style = "text-align: center; text-transform: uppercase;"),
      shiny::p("Confirm plate assignments with raw data, then upload Transition and Removal .xlsx templates for event timing and exclusions."),
      shiny::div(class = "details", shiny::p("Transition captures time codes (e.g., light/vibration events); Removal excludes times, wells, periods, or conditions. Output cleaned datasets as .xlsx, with console history."))
    )),
    shiny::column(width = 3, shinydashboard::box(width = NULL, status = "primary",
      shiny::div(style = "text-align: center; margin-bottom: 1em;", shiny::icon("chart-line", class = "fa-3x")),
      shiny::h4(shiny::strong("VISUALIZATION"), style = "text-align: center; text-transform: uppercase;"),
      shiny::p("Generate dataframes from processed data and create customizable charts with themes, labels, and colors."),
      shiny::div(class = "details", shiny::p("Export as .png or interactive .html for detailed hovering and exploration."))
    ))
  )
)

# ======================================================================
# 6) Global Footer (All Tabs)
# ======================================================================
global_footer <- shiny::tags$footer(
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shiny::p("Use the left-hand menu to navigate.", class = "footer-nav-hint"),
      shiny::hr(class = "footer-line"),
      shiny::div(
        class = "app-footer",
        shiny::HTML("
          <div>
            Powered by 
            <a href='https://www.viewpoint.fr/' target='_blank'>ViewPoint Behavior Technology</a>
            &nbsp;&nbsp;|&nbsp;&nbsp;
            Developed by 
            <a href='https://www.linkedin.com/in/antoine-tourret-b70672175/' target='_blank'>Antoine Tourret</a>
          </div>
          <div style='margin-top: 8px; font-size: 0.9em;'>
            © 2025 Zebrabox Pipeline • v1.0
          </div>
        ")
      )
    )
  )
)

# ======================================================================
# 7) Dashboard Body
# ======================================================================
dashboard_body <- shinydashboard::dashboardBody(
  fresh::use_theme(base_theme),
  header_styles,
  shinydashboard::tabItems(
    welcome_content,
    shinydashboard::tabItem(tabName = "plate_plan",    plate_plan_ui("plate_plan")),
    shinydashboard::tabItem(tabName = "raw_data",      raw_data_ui("raw_data")),
    shinydashboard::tabItem(tabName = "processing",    shiny::uiOutput("processing_ui")),
    shinydashboard::tabItem(tabName = "visualization", shiny::uiOutput("visualization_ui"))
  ),
  global_footer
)

# ======================================================================
# 8) Final Dashboard
# ======================================================================
shinydashboard::dashboardPage(
  header  = header_items,
  sidebar = sidebar_content,
  body    = dashboard_body,
  skin    = "black"
)