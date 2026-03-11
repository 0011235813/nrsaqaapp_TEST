# =============================================================================
# mod_data_source.R
# Shiny module: data source selection, file upload, and AWQMS connection.
# Manages the startup modal, the sidebar "Switch Data Source" re-entry,
# and the date-parse warning modal.
#
# Returns a list of reactives:
#   source_data()   — the loaded data frame (or NULL)
#   source_mode()   — "upload" | "odbc" | NULL
#   loaded_dates()  — list(start, end) of the range used at load time
# =============================================================================

#' Data source module UI
#'
#' Provides the file-input and upload-status outputs used inside the upload
#' modal.  The modals themselves are rendered server-side.
#'
#' @param id Module namespace ID.
#' @return A `tagList` of Shiny UI elements.
#' @export
mod_data_source_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("upload_status"))
  )
}

#' Data source module server
#'
#' @param id Module namespace ID.
#' @param current_tab A `reactiveVal` holding the active tab name, used to
#'   restore navigation after the modal closes.
#' @param awqms_enabled Logical — whether the AWQMS ODBC option is available
#'   (i.e., the connection script was successfully sourced).
#'
#' @return A named list:
#'   \describe{
#'     \item{source_data}{`reactive` — the loaded/filtered data frame or NULL.}
#'     \item{source_mode}{`reactiveVal` — `"upload"` | `"odbc"` | NULL.}
#'     \item{loaded_dates}{`reactiveVal` — `list(start, end)`.}
#'   }
#' @export
mod_data_source_server <- function(id, current_tab, awqms_enabled = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -------------------------------------------------------------------------
    # State
    # -------------------------------------------------------------------------
    source_mode      <- shiny::reactiveVal(NULL)
    source_data      <- shiny::reactiveVal(NULL)
    loaded_dates     <- shiny::reactiveVal(list(start = NULL, end = NULL))
    pending_combined <- shiny::reactiveVal(NULL)
    con_obj          <- NULL

    # -------------------------------------------------------------------------
    # Startup modal — fires once on session start
    # -------------------------------------------------------------------------
    shiny::observe({
      shiny::showModal(.build_welcome_modal(ns, awqms_enabled))
    })

    # -------------------------------------------------------------------------
    # Sidebar "Add Files / Switch Data Source" re-entry
    # -------------------------------------------------------------------------
    shiny::observeEvent(input$open_source_modal, {
      shiny::showModal(.build_source_choice_modal(ns, awqms_enabled))
    })

    # -------------------------------------------------------------------------
    # Branch: Upload
    # -------------------------------------------------------------------------
    shiny::observeEvent(input$startup_upload, {
      shiny::removeModal()
      shiny::showModal(.build_upload_modal(ns))
    })
    shiny::observeEvent(input$choose_upload, {
      shiny::removeModal()
      shiny::showModal(.build_upload_modal(ns))
    })

    output$upload_status <- shiny::renderUI({
      shiny::req(input$upload_files)
      shiny::div(
        style = "margin-top:15px;padding:10px;background-color:#e3f2fd;border-radius:4px;",
        shiny::tags$strong("Files ready:"),
        shiny::tags$ul(lapply(input$upload_files$name, shiny::tags$li))
      )
    })

    shiny::observeEvent(input$confirm_upload, {
      shiny::req(input$upload_files)
      shiny::withProgress(message = "Loading files...", value = 0, {
        fl            <- input$upload_files
        out           <- list()
        date_failures <- list()

        for (i in seq_len(nrow(fl))) {
          shiny::incProgress(1 / nrow(fl), detail = fl$name[i])
          tryCatch({
            df    <- read_uploaded_file(fl$datapath[i], fl$name[i])
            df    <- harmonize_to_odbc_schema(df)
            df    <- normalize_core_types(df)
            fails <- attr(df, "date_parse_failures")
            if (!is.null(fails))
              date_failures[[length(date_failures) + 1]] <-
                c(list(file = fl$name[i]), fails)
            out[[length(out) + 1]] <- df
          }, error = function(e)
            shiny::showNotification(
              paste("Error reading", fl$name[i], ":", e$message),
              type = "error", duration = 10
            ))
        }

        if (length(out) == 0) {
          shiny::showNotification(
            "No valid files loaded — check file format",
            type = "error", duration = 8
          )
          return()
        }

        combined <- dplyr::bind_rows(out)
        combined <- apply_nrsa_filters(combined)
        combined <- combined |>
          add_project_col() |>
          coerce_dates() |>
          coerce_numeric()

        if (nrow(combined) == 0) {
          shiny::showNotification(
            paste("No Habitat/River/Stream records found.",
                  "Check activity_media and monitoring_location_type columns."),
            type = "warning", duration = 10
          )
          return()
        }

        if (COL_DATE %in% names(combined) &&
            any(!is.na(combined[[COL_DATE]]))) {
          dr <- range(combined[[COL_DATE]], na.rm = TRUE)
          loaded_dates(list(start = dr[1], end = dr[2]))
          shiny::updateDateInput(session, "start_date", value = dr[1])
          shiny::updateDateInput(session, "end_date",   value = dr[2])
        }

        if (length(date_failures) > 0) {
          pending_combined(combined)
          shiny::removeModal()
          shiny::showModal(
            .build_date_warning_modal(ns, date_failures)
          )
          return()
        }

        .finalise_upload(combined, nrow(fl), source_data, source_mode,
                         current_tab, session)
      })
    })

    # Date warning: continue
    shiny::observeEvent(input$dismiss_date_warning, {
      d <- pending_combined()
      shiny::req(d)
      .finalise_upload(d, 1L, source_data, source_mode, current_tab, session,
                       warning = TRUE)
      pending_combined(NULL)
    })

    # Date warning: cancel
    shiny::observeEvent(input$abort_date_warning, {
      pending_combined(NULL)
      shiny::removeModal()
      shiny::showNotification(
        "Load cancelled. Please fix the date format and re-upload.",
        type = "error", duration = 8
      )
    })

    # -------------------------------------------------------------------------
    # Branch: AWQMS / ODBC
    # -------------------------------------------------------------------------
    shiny::observeEvent(input$startup_odbc, {
      shiny::removeModal()
      shiny::showModal(.build_odbc_modal(ns))
    })
    shiny::observeEvent(input$choose_odbc, {
      shiny::removeModal()
      shiny::showModal(.build_odbc_modal(ns))
    })

    output$odbc_date_warning <- shiny::renderUI({
      tryCatch({
        s <- input$odbc_start_date
        e <- input$odbc_end_date
        if (is.null(s) || is.null(e) || length(s) == 0 || length(e) == 0)
          return(NULL)
        s <- suppressWarnings(as.Date(s))
        e <- suppressWarnings(as.Date(e))
        if (is.na(s) || is.na(e)) return(NULL)
        if (s > e)
          shiny::div(
            style = "padding:10px;background-color:#f8d7da;border-radius:4px;",
            shiny::icon("times-circle"), " Start date must be before end date."
          )
      }, error = function(e) NULL)
    })

    shiny::observeEvent(input$confirm_odbc, {
      start <- as.Date(input$odbc_start_date)
      end   <- as.Date(input$odbc_end_date)
      if (isTRUE(start > end)) {
        shiny::showNotification("Start date must be before end date.",
                                type = "warning", duration = 6)
        return()
      }

      shiny::withProgress(message = "Connecting to AWQMS...", value = 0.2, {
        con_obj <<- safe_awqms_connect()
        if (is.null(con_obj)) return()

        shiny::incProgress(0.3, detail = "Fetching NRSA records...")
        tryCatch({
          cfg  <- config::get()
          data <- fetch_awqms_data(con_obj, start, end, cfg$awqms_table)

          if (is.null(data)) {
            shiny::showNotification(
              paste0("No NRSA records found between ", start, " and ", end,
                     ". Try a wider date range."),
              type = "warning", duration = 15
            )
            safe_awqms_disconnect(con_obj)
            con_obj <<- NULL
            return()
          }

          loaded_dates(list(start = start, end = end))
          shiny::updateDateInput(session, "start_date", value = start)
          shiny::updateDateInput(session, "end_date",   value = end)
          source_data(data)
          source_mode("odbc")
          shiny::removeModal()
          shinydashboard::updateTabItems(session, "tabs", current_tab())
          shiny::showNotification(
            paste0("Loaded ", format(nrow(data), big.mark = ","),
                   " NRSA records (", start, " to ", end, ")"),
            type = "message", duration = 6
          )
        }, error = function(e) {
          shiny::showNotification(paste("Error fetching data:", e$message),
                                  type = "error", duration = 10)
          safe_awqms_disconnect(con_obj)
          con_obj <<- NULL
        })
      })
    })

    # -------------------------------------------------------------------------
    # Session cleanup
    # -------------------------------------------------------------------------
    session$onSessionEnded(function() safe_awqms_disconnect(con_obj))

    # -------------------------------------------------------------------------
    # Return
    # -------------------------------------------------------------------------
    list(
      source_data  = source_data,
      source_mode  = source_mode,
      loaded_dates = loaded_dates
    )
  })
}

# =============================================================================
# Private modal builders
# =============================================================================

.build_welcome_modal <- function(ns, awqms_enabled) {
  shiny::modalDialog(
    title     = shiny::tags$h3("Welcome to the NRSA QA Application",
                               style = "margin-top:0;text-align:center;"),
    size      = "m",
    easyClose = FALSE,
    footer    = NULL,
    shiny::div(
      style = "text-align:center;margin-bottom:20px;",
      shiny::p("Please select how you would like to load your NRSA data:",
               style = "font-size:16px;color:#666;")
    ),
    shiny::fluidRow(
      shiny::column(6,
        shiny::actionButton(ns("startup_upload"),
          label = shiny::div(shiny::icon("upload", style = "font-size:48px;"),
                             shiny::br(), shiny::br(),
                             shiny::tags$strong("Upload Files"), shiny::br(),
                             shiny::tags$small("Excel (.xlsx / .xls)",
                                               style = "color:#888;")),
          width = "100%", style = "height:160px;font-size:16px;")
      ),
      shiny::column(6,
        shiny::actionButton(ns("startup_odbc"),
          label = shiny::div(shiny::icon("database", style = "font-size:48px;"),
                             shiny::br(), shiny::br(),
                             shiny::tags$strong("Connect to AWQMS"), shiny::br(),
                             shiny::tags$small("ODBC connection",
                                               style = "color:#888;")),
          width = "100%", style = "height:160px;font-size:16px;",
          disabled = if (!awqms_enabled) "disabled" else NULL)
      )
    )
  )
}

.build_source_choice_modal <- function(ns, awqms_enabled) {
  shiny::modalDialog(
    title = "Choose Data Source", size = "m", easyClose = FALSE,
    shiny::p("Select how you want to load NRSA data:",
             style = "margin-bottom:20px;"),
    shiny::fluidRow(
      shiny::column(6,
        shiny::actionButton(ns("choose_upload"),
          label = shiny::div(shiny::icon("upload", style = "font-size:48px;"),
                             shiny::br(), shiny::br(), "Upload Files"),
          width = "100%", style = "height:150px;font-size:18px;")
      ),
      shiny::column(6,
        shiny::actionButton(ns("choose_odbc"),
          label = shiny::div(shiny::icon("database", style = "font-size:48px;"),
                             shiny::br(), shiny::br(), "Connect to AWQMS"),
          width = "100%", style = "height:150px;font-size:18px;",
          disabled = if (!awqms_enabled) "disabled" else NULL)
      )
    ),
    footer = shiny::tagList(shiny::modalButton("Cancel"))
  )
}

.build_upload_modal <- function(ns) {
  shiny::modalDialog(
    title = "Upload NRSA Data Files", size = "l",
    shiny::fileInput(ns("upload_files"),
                     "Select Excel files (.xlsx or .xls)",
                     multiple = TRUE, accept = c(".xlsx", ".xls")),
    shiny::p("Files will be combined into a single dataset.",
             style = "color:#666;font-style:italic;"),
    shiny::uiOutput(ns("upload_status")),
    footer = shiny::tagList(
      shiny::actionButton(ns("confirm_upload"), "Load Data",
                          class = "btn-primary"),
      shiny::modalButton("Cancel")
    )
  )
}

.build_odbc_modal <- function(ns) {
  shiny::modalDialog(
    title     = "Connect to AWQMS Database",
    size      = "m",
    easyClose = FALSE,
    shiny::p("Select a date range to load data from AWQMS.",
             style = "color:#666;"),
    shiny::p(shiny::icon("info-circle"),
             " Ensure you have network access and valid credentials.",
             style = "color:#1976d2;margin-bottom:20px;"),
    shiny::div(
      style = "background-color:#f5f5f5;padding:15px;border-radius:6px;",
      shiny::tags$strong(shiny::icon("calendar"),
                         " Select a date range to load (required):"),
      shiny::br(), shiny::br(),
      shiny::fluidRow(
        shiny::column(6, shiny::dateInput(ns("odbc_start_date"),
                                          "Start Date", value = NULL)),
        shiny::column(6, shiny::dateInput(ns("odbc_end_date"),
                                          "End Date",   value = NULL))
      ),
      shiny::p(shiny::icon("info-circle"),
               " A narrower range loads faster.",
               style = "color:#666;font-size:12px;margin-top:5px;margin-bottom:0;")
    ),
    shiny::br(),
    shiny::uiOutput(ns("odbc_date_warning")),
    footer = shiny::tagList(
      shiny::actionButton(ns("confirm_odbc"), "Connect & Load",
                          class = "btn-primary"),
      shiny::modalButton("Cancel")
    )
  )
}

.build_date_warning_modal <- function(ns, date_failures) {
  total_failed <- sum(vapply(date_failures, `[[`, integer(1), "n"))
  detail_lines <- lapply(date_failures, function(f) {
    paste0(f$file, ": ", f$n, " record(s) — e.g. ",
           paste(f$samples, collapse = ", "))
  })
  shiny::modalDialog(
    title = shiny::tags$h4(
      shiny::icon("exclamation-triangle", style = "color:#c62828;"),
      " Date Parse Warning", style = "margin-top:0;"
    ),
    size = "m",
    shiny::div(
      style = "background:#fff3cd;padding:12px;border-radius:4px;margin-bottom:12px;",
      shiny::tags$strong(paste0(
        total_failed,
        " record(s) have unrecognised date values. ",
        "Their activity_start_date has been set to NA."
      )),
      shiny::br(), shiny::br(),
      "These records are still loaded but will be EXCLUDED from date-filtered views.",
      shiny::br(), shiny::br(),
      shiny::tags$strong("Affected files:"),
      shiny::tags$ul(lapply(detail_lines, shiny::tags$li))
    ),
    shiny::p(
      "Please verify that ", shiny::tags$code("activity_start_date"),
      " uses a recognised format such as ",
      shiny::tags$code("YYYY-MM-DD"), " or ", shiny::tags$code("M/D/YYYY"),
      " before re-uploading."
    ),
    footer = shiny::tagList(
      shiny::actionButton(ns("dismiss_date_warning"), "Continue Anyway",
                          class = "btn-warning"),
      shiny::actionButton(ns("abort_date_warning"),   "Cancel Load",
                          class = "btn-danger")
    )
  )
}

.finalise_upload <- function(combined, n_files, source_data_rv, source_mode_rv,
                              current_tab, session, warning = FALSE) {
  source_data_rv(combined)
  source_mode_rv("upload")
  shiny::removeModal()
  shinydashboard::updateTabItems(session, "tabs", current_tab())
  msg <- paste0(
    "Loaded ", format(nrow(combined), big.mark = ","),
    " records from ", n_files, " file(s)",
    if (warning) " (some dates set to NA — see warning)" else ""
  )
  shiny::showNotification(msg,
    type     = if (warning) "warning" else "message",
    duration = if (warning) 8 else 5
  )
}
