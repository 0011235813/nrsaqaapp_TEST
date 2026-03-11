# =============================================================================
# app_ui.R
# Top-level UI assembled from module UIs.
# =============================================================================

#' Application UI
#'
#' Assembles the full shinydashboard UI, wiring in each module's UI function.
#'
#' @return A `shinydashboard::dashboardPage` UI definition.
#' @export
app_ui <- function() {
  shinydashboard::dashboardPage(

    # ── Header ────────────────────────────────────────────────────────────────
    shinydashboard::dashboardHeader(
      title = "NRSA QA Application",
      shiny::tags$li(
        class = "dropdown",
        shiny::actionButton("show_help", shiny::icon("question-circle"),
                            class = "btn-header",
                            style = "margin-top:8px;margin-right:5px;",
                            title = "Help"),
        shiny::actionButton("settings_btn", shiny::icon("cog"),
                            class = "btn-header",
                            style = "margin-top:8px;margin-right:10px;",
                            title = "Settings")
      )
    ),

    # ── Sidebar ───────────────────────────────────────────────────────────────
    shinydashboard::dashboardSidebar(
      collapsed = TRUE,
      shinydashboard::sidebarMenu(
        id = "tabs",
        shinydashboard::menuItem("Summary",   tabName = "summary",
                                  icon = shiny::icon("table")),
        shinydashboard::menuItem("Site Map",  tabName = "map",
                                  icon = shiny::icon("map")),
        shinydashboard::menuItem("QA Review", tabName = "qa",
                                  icon = shiny::icon("check-square")),
        shinydashboard::menuItem("Batch QA",  tabName = "batch_qa",
                                  icon = shiny::icon("tasks")),
        shinydashboard::menuItem("Add Files / Switch Data Source",
                                  tabName = "source",
                                  icon = shiny::icon("database"))
      )
    ),

    # ── Body ──────────────────────────────────────────────────────────────────
    shinydashboard::dashboardBody(
      shinyjs::useShinyjs(),

      # Global styles + dark mode + font size
      shiny::tags$head(shiny::tags$style(shiny::HTML("
        .btn-header { background-color:transparent; border:none; color:white; font-size:18px; }
        .btn-header:hover { background-color:rgba(255,255,255,0.1); }
        .settings-section { margin-bottom:25px; padding-bottom:20px; border-bottom:1px solid #e0e0e0; }
        .settings-section:last-child { border-bottom:none; }
        .settings-label { font-weight:600; margin-bottom:10px; color:#333; }
        body.dark-mode { background-color:#1a1a1a; color:#e0e0e0; }
        body.dark-mode .content-wrapper,
        body.dark-mode .main-header .navbar   { background-color:#2d2d2d; }
        body.dark-mode .main-header .logo,
        body.dark-mode .skin-blue .main-header .logo { background-color:#1a1a1a; color:#e0e0e0; }
        body.dark-mode .skin-blue .main-header .navbar { background-color:#2d2d2d; }
        body.dark-mode .box         { background-color:#2d2d2d; border-color:#404040; }
        body.dark-mode .small-box   { background-color:#2d2d2d !important; color:#e0e0e0; }
        body.dark-mode .modal-content { background-color:#2d2d2d; color:#e0e0e0; }
        body.dark-mode .modal-header  { border-bottom-color:#404040; }
        body.dark-mode .form-control,
        body.dark-mode .selectize-input    { background-color:#1a1a1a; color:#e0e0e0; border-color:#404040; }
        body.dark-mode .selectize-dropdown { background-color:#2d2d2d; color:#e0e0e0; border-color:#404040; }
        body.dark-mode .dataTables_wrapper,
        body.dark-mode table.dataTable { color:#e0e0e0; background-color:#2d2d2d; }
        body.dark-mode table.dataTable thead th { background-color:#1a1a1a; color:#e0e0e0; }
        body.dark-mode table.dataTable tbody tr { background-color:#2d2d2d; }
        body.dark-mode table.dataTable tbody tr:hover,
        body.dark-mode table.dataTable.stripe tbody tr.odd { background-color:#404040 !important; }
        body.dark-mode .card        { background-color:#2d2d2d; color:#e0e0e0; }
        body.dark-mode .card-header { background-color:#1a1a1a; border-bottom-color:#404040; }
        body.dark-mode .settings-label   { color:#e0e0e0; }
        body.dark-mode .settings-section { border-bottom-color:#404040; }
        body.font-small  { font-size:12px; }
        body.font-medium { font-size:14px; }
        body.font-large  { font-size:16px; }
        body.font-xlarge { font-size:18px; }
        .content-wrapper { padding-bottom:50px; }
      "))),

      # JS: settings persistence + keyboard shortcuts
      shiny::tags$script(shiny::HTML("
        $(function(){
          $('#start_date input').attr('placeholder','Enter Date');
          $('#end_date input').attr('placeholder','Enter Date');

          Shiny.addCustomMessageHandler('loadSettings', function(msg){
            var dm = localStorage.getItem('nrsa_dark_mode');
            var fs = localStorage.getItem('nrsa_font_size');
            if(dm !== null) Shiny.setInputValue('dark_mode_stored', dm === 'true');
            if(fs !== null) Shiny.setInputValue('font_size_stored', fs);
          });
          Shiny.addCustomMessageHandler('applyDarkMode', function(e){
            if(e){ $('body').addClass('dark-mode');    localStorage.setItem('nrsa_dark_mode','true');  }
            else  { $('body').removeClass('dark-mode'); localStorage.setItem('nrsa_dark_mode','false'); }
          });
          Shiny.addCustomMessageHandler('applyFontSize', function(s){
            $('body').removeClass('font-small font-medium font-large font-xlarge')
                     .addClass('font-'+s);
            localStorage.setItem('nrsa_font_size', s);
          });
          $(document).on('keydown', function(e){
            if((e.ctrlKey||e.metaKey) && e.keyCode===82){ e.preventDefault(); $('#apply_filters').click(); }
            if((e.ctrlKey||e.metaKey) && e.keyCode===72){ e.preventDefault(); $('#show_help').click(); }
            if(e.keyCode===27){ Shiny.setInputValue('clear_selection', Math.random()); }
          });
        });
      ")),

      # Global filter bar
      shiny::div(
        id = "filter_controls",
        shiny::fluidRow(
          shiny::column(2, shiny::uiOutput("ui_start_date")),
          shiny::column(2, shiny::uiOutput("ui_end_date")),
          shiny::column(3, shiny::selectInput(
            "protocol", "Protocol",
            choices  = c("NRSA Protocol", "State Protocol"),
            selected = "NRSA Protocol"
          )),
          shiny::column(3, shinyWidgets::pickerInput(
            "project", "Project",
            choices = NULL, multiple = TRUE,
            options = list(`actions-box` = TRUE, `live-search` = TRUE,
                           `selected-text-format` = "count > 3")
          )),
          shiny::column(2,
            shiny::br(),
            shiny::actionButton("apply_filters", "Apply Filters",
                                width = "100%", class = "btn-primary")
          )
        )
      ),

      shiny::br(),

      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "summary",
                                 mod_summary_ui("summary")),
        shinydashboard::tabItem(tabName = "map",
                                 mod_map_ui("map")),
        shinydashboard::tabItem(tabName = "qa",
                                 mod_qa_review_ui("qa")),
        shinydashboard::tabItem(tabName = "batch_qa",
                                 mod_batch_qa_ui("batch_qa")),
        shinydashboard::tabItem(tabName = "source")  # modal only
      ),

      shiny::uiOutput("status_bar")
    )
  )
}
