# =============================================================================
# app_server.R
# Top-level server function — wires modules together, owns global state
# (filters, settings, tab tracking) that spans multiple modules.
# =============================================================================

#' Application server
#'
#' @param input,output,session Standard Shiny server arguments.
#' @export
app_server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Global reactive state
  # ---------------------------------------------------------------------------
  current_tab       <- shiny::reactiveVal("summary")
  dark_mode_enabled <- shiny::reactiveVal(FALSE)
  current_font_size <- shiny::reactiveVal("medium")

  # ---------------------------------------------------------------------------
  # Date input renderers (blank by default — avoids Shiny defaulting to today)
  # ---------------------------------------------------------------------------
  output$ui_start_date <- shiny::renderUI({
    shiny::dateInput("start_date", "Start Date", value = NULL)
  })
  output$ui_end_date <- shiny::renderUI({
    shiny::dateInput("end_date", "End Date", value = NULL)
  })

  # ---------------------------------------------------------------------------
  # Config — use same file-finding logic as run_app() so both are consistent
  # ---------------------------------------------------------------------------
  config_file <- if (file.exists("config.yml")) {
    "config.yml"
  } else {
    system.file("app/config.yml", package = "nrsaqaapp")
  }

  cfg <- tryCatch(
    config::get(file = config_file),
    error = function(e) list(awqms_source = NULL)
  )

  # Resolve awqms_source — handle both plain paths and bundled "auto" keyword
  awqms_path <- cfg$awqms_source
  if (identical(awqms_path, "auto") || identical(awqms_path, "bundled")) {
    awqms_path <- system.file("awqms/AWQMS_Source_Code.R", package = "nrsaqaapp")
  }
  awqms_enabled <- !is.null(awqms_path) && nzchar(awqms_path) && isTRUE(file.exists(awqms_path))

  # ---------------------------------------------------------------------------
  # Data source module
  # ---------------------------------------------------------------------------
  ds <- mod_data_source_server("data_source",
                                current_tab   = current_tab,
                                awqms_enabled = awqms_enabled)

  # ---------------------------------------------------------------------------
  # Filtered data — date + project filters applied on top of loaded data
  # ---------------------------------------------------------------------------
  filtered_data <- shiny::reactive({
    shiny::req(ds$source_data())
    input$apply_filters  # take dependency on Apply Filters button

    d  <- ds$source_data()
    ld <- ds$loaded_dates()
    s_date <- safe_date(input$start_date) %||% safe_date(ld$start)
    e_date <- safe_date(input$end_date)   %||% safe_date(ld$end)

    if (!is.null(s_date) && COL_DATE %in% names(d))
      d <- d[!is.na(d[[COL_DATE]]) & d[[COL_DATE]] >= s_date, ]
    if (!is.null(e_date) && COL_DATE %in% names(d))
      d <- d[!is.na(d[[COL_DATE]]) & d[[COL_DATE]] <= e_date, ]
    if ("project_id" %in% names(d) &&
        !is.null(input$project) && length(input$project) > 0)
      d <- d[!is.na(d$project_id) & d$project_id %in% input$project, ]
    d
  })

  # ---------------------------------------------------------------------------
  # Project picker — initialise with all known IDs; update after data loads
  # ---------------------------------------------------------------------------
  shiny::observe({
    shinyWidgets::updatePickerInput(session, "project",
                                    choices  = sort(NRSA_PROJECT_IDS),
                                    selected = sort(NRSA_PROJECT_IDS))
  })
  shiny::observeEvent(ds$source_data(), {
    d <- ds$source_data()
    if ("project_id" %in% names(d)) {
      projects <- sort(unique(d$project_id[!is.na(d$project_id)]))
      shinyWidgets::updatePickerInput(session, "project",
                                      choices  = projects,
                                      selected = projects)
    }
  })

  # ---------------------------------------------------------------------------
  # Date validation
  # ---------------------------------------------------------------------------
  shiny::observe({
    s <- safe_date(input$start_date)
    e <- safe_date(input$end_date)
    if (is.null(s) || is.null(e)) return()
    if (isTRUE(s > e)) {
      shiny::showNotification("Start date must be before end date",
                              type = "warning", duration = 5)
      shiny::updateDateInput(session, "end_date", value = s)
    }
  })

  # ---------------------------------------------------------------------------
  # Persistent settings (localStorage <-> server)
  # ---------------------------------------------------------------------------
  shiny::observe({ session$sendCustomMessage("loadSettings", list()) })

  shiny::observeEvent(input$dark_mode_stored, {
    dark_mode_enabled(input$dark_mode_stored)
    session$sendCustomMessage("applyDarkMode", input$dark_mode_stored)
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  shiny::observeEvent(input$font_size_stored, {
    current_font_size(input$font_size_stored)
    session$sendCustomMessage("applyFontSize", input$font_size_stored)
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # ---------------------------------------------------------------------------
  # Settings modal
  # ---------------------------------------------------------------------------
  shiny::observeEvent(input$settings_btn, {
    shiny::showModal(shiny::modalDialog(
      title = shiny::tags$h3("Application Settings", style = "margin-top:0;"),
      size  = "m",
      shiny::div(class = "settings-section",
        shiny::div(class = "settings-label", "Appearance"),
        shiny::checkboxInput("dark_mode", "Dark Mode",
                             value = dark_mode_enabled()),
        shiny::selectInput("font_size", "Font Size",
          choices  = c("Small" = "small", "Medium" = "medium",
                       "Large" = "large", "Extra Large" = "xlarge"),
          selected = current_font_size())
      ),
      footer = shiny::tagList(
        shiny::actionButton("save_settings", "Save Settings",
                            class = "btn-primary"),
        shiny::modalButton("Cancel")
      )
    ))
  })

  shiny::observeEvent(input$save_settings, {
    dark_mode_enabled(input$dark_mode)
    session$sendCustomMessage("applyDarkMode", input$dark_mode)
    current_font_size(input$font_size)
    session$sendCustomMessage("applyFontSize", input$font_size)
    shiny::removeModal()
    shiny::showNotification("Settings saved", type = "message", duration = 3)
  })

  # ---------------------------------------------------------------------------
  # Help modal
  # ---------------------------------------------------------------------------
  shiny::observeEvent(input$show_help, {
    shiny::showModal(shiny::modalDialog(
      title = "NRSA QA Application Help", size = "l",
      shiny::div(
        style = "max-height:500px;overflow-y:auto;",
        shiny::h4("Getting Started"),
        shiny::tags$ul(
          shiny::tags$li("Connect to AWQMS via 'Add Files / Switch Data Source'"),
          shiny::tags$li("Apply filters and click Apply Filters"),
          shiny::tags$li("Click a row in the Summary table to review QA for that site")
        ),
        shiny::hr(),
        shiny::h4("Tabs"),
        shiny::tags$ul(
          shiny::tags$li(shiny::tags$strong("Summary"),
                         " \u2014 site-visit overview with transect completeness"),
          shiny::tags$li(shiny::tags$strong("Site Map"),
                         " \u2014 geographic view of all filtered sites"),
          shiny::tags$li(shiny::tags$strong("QA Review"),
                         " \u2014 detailed QA checks for a selected site-visit"),
          shiny::tags$li(shiny::tags$strong("Batch QA"),
                         " \u2014 QA metrics across all site-visits at once")
        ),
        shiny::hr(),
        shiny::h4("Keyboard Shortcuts"),
        shiny::tags$ul(
          shiny::tags$li("Ctrl/Cmd + R \u2014 Apply Filters"),
          shiny::tags$li("Ctrl/Cmd + H \u2014 Help"),
          shiny::tags$li("Escape \u2014 Clear row selection")
        ),
        shiny::hr(),
        shiny::h4("Data Notes"),
        shiny::tags$ul(
          shiny::tags$li("Data is loaded from results_standard_vw, filtered to NRSA projects"),
          shiny::tags$li("Project IDs are read from project_id1 through project_id6"),
          shiny::tags$li("Site-visits are grouped by parent_activity_id")
        )
      ),
      footer = shiny::modalButton("Close")
    ))
  })

  # ---------------------------------------------------------------------------
  # Tab tracking + Source modal trigger
  # ---------------------------------------------------------------------------
  shiny::observeEvent(input$tabs, {
    shiny::req(input$tabs)
    if (input$tabs != "source") current_tab(input$tabs)
    if (input$tabs == "source") {
      shinydashboard::updateTabItems(session, "tabs", current_tab())
      shinyjs::click("data_source-open_source_modal")
    }
  })

  # ---------------------------------------------------------------------------
  # Summary module — returns the selected group ID reactive
  # ---------------------------------------------------------------------------
  selected_group <- mod_summary_server(
    "summary",
    filtered_data = filtered_data,
    source_data   = ds$source_data,
    source_mode   = ds$source_mode
  )

  # ---------------------------------------------------------------------------
  # Map module
  # ---------------------------------------------------------------------------
  mod_map_server(
    "map",
    filtered_data  = filtered_data,
    selected_group = selected_group,
    summary_proxy  = "summary-summary_table"
  )

  # ---------------------------------------------------------------------------
  # QA Review module
  # ---------------------------------------------------------------------------
  mod_qa_review_server(
    "qa",
    filtered_data  = filtered_data,
    selected_group = selected_group
  )

  # ---------------------------------------------------------------------------
  # Batch QA module
  # ---------------------------------------------------------------------------
  mod_batch_qa_server(
    "batch_qa",
    filtered_data = filtered_data
  )

  # ---------------------------------------------------------------------------
  # Escape key clears row selection
  # ---------------------------------------------------------------------------
  shiny::observeEvent(input$clear_selection, {
    DT::dataTableProxy("summary-summary_table") |> DT::selectRows(NULL)
  })

  # ---------------------------------------------------------------------------
  # Status bar
  # ---------------------------------------------------------------------------
  output$status_bar <- shiny::renderUI({
    if (is.null(ds$source_data())) return(NULL)
    d        <- ds$source_data()
    filtered <- tryCatch(filtered_data(), error = function(e) d)
    n_sites  <- if (!is.null(filtered))
      dplyr::n_distinct(filtered[[COL_SITE]], na.rm = TRUE) else "\u2014"
    shiny::div(
      style = paste0("position:fixed;bottom:0;left:0;right:0;",
                     "background-color:#2c3e50;color:white;",
                     "padding:7px 20px;z-index:1000;font-size:13px;"),
      shiny::fluidRow(
        shiny::column(3, shiny::icon("database"), " Total: ",
                      format(nrow(d), big.mark = ",")),
        shiny::column(3, shiny::icon("filter"), " Filtered: ",
                      if (!is.null(filtered))
                        format(nrow(filtered), big.mark = ",") else "\u2014"),
        shiny::column(3, shiny::icon("map-marker-alt"), " Sites: ", n_sites),
        shiny::column(3,
          tryCatch(
            if (!is.null(selected_group()))
              shiny::span(shiny::icon("check-circle", style = "color:#4caf50;"),
                          " Site selected")
            else
              shiny::span(shiny::icon("info-circle"), " No site selected"),
            error = function(e)
              shiny::span(shiny::icon("info-circle"), " No site selected")
          )
        )
      )
    )
  })
}
