# =============================================================================
# mod_summary.R
# Shiny module: Summary tab — connection banner, dataset metrics box,
# download buttons, and the site-visit summary table.
# =============================================================================

#' Summary module UI
#' @param id Module namespace ID.
#' @export
mod_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("connection_status")),
    shiny::uiOutput(ns("data_summary_box")),
    shiny::uiOutput(ns("download_buttons")),
    shiny::br(),
    DT::DTOutput(ns("summary_table"), width = "100%")
  )
}

#' Summary module server
#'
#' @param id Module namespace ID.
#' @param filtered_data A `reactive` returning the current filtered data frame.
#' @param source_data   A `reactiveVal` with the full (unfiltered) loaded data.
#' @param source_mode   A `reactiveVal` — `"upload"` | `"odbc"` | NULL.
#'
#' @return A `reactive` expression that returns the currently selected row's
#'   `parent_activity_id`, or `NULL` if nothing is selected.
#' @export
mod_summary_server <- function(id, filtered_data, source_data, source_mode) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -------------------------------------------------------------------------
    # Connection status banner
    # -------------------------------------------------------------------------
    output$connection_status <- shiny::renderUI({
      mode <- source_mode()
      if (is.null(mode))
        shiny::div(
          style = "padding:10px;background-color:#fff3cd;border-radius:4px;margin-bottom:15px;",
          shiny::icon("exclamation-triangle"),
          " No data loaded. Use ",
          shiny::tags$strong("Add Files / Switch Data Source"), " to begin."
        )
      else if (mode == "upload")
        shiny::div(
          style = "padding:10px;background-color:#d4edda;border-radius:4px;margin-bottom:15px;",
          shiny::icon("check-circle"), " Uploaded data — ",
          shiny::tags$strong(format(nrow(source_data()), big.mark = ",")),
          " records."
        )
      else
        shiny::div(
          style = "padding:10px;background-color:#d1ecf1;border-radius:4px;margin-bottom:15px;",
          shiny::icon("database"), " AWQMS (NRSA filter) — ",
          shiny::tags$strong(format(nrow(source_data()), big.mark = ",")),
          " records."
        )
    })

    # -------------------------------------------------------------------------
    # Dataset summary metrics box
    # -------------------------------------------------------------------------
    output$data_summary_box <- shiny::renderUI({
      shiny::req(filtered_data())
      d        <- filtered_data()
      n_visits <- dplyr::n_distinct(d[[COL_GROUP]], na.rm = TRUE)
      n_sites  <- dplyr::n_distinct(d[[COL_SITE]],  na.rm = TRUE)
      n_params <- if (COL_PARAM %in% names(d))
        dplyr::n_distinct(d[[COL_PARAM]], na.rm = TRUE) else NA
      dr <- if (COL_DATE %in% names(d) && any(!is.na(d[[COL_DATE]])))
        range(d[[COL_DATE]], na.rm = TRUE) else c(NA, NA)

      shiny::div(
        style = "background-color:#f5f5f5;padding:15px;border-radius:4px;margin-bottom:20px;",
        shiny::h4("Dataset Summary", style = "margin-top:0;"),
        shiny::fluidRow(
          shiny::column(3, shiny::div(style = "text-align:center;",
            shiny::h2(style = "color:#1976d2;margin:5px;",
                      format(nrow(d), big.mark = ",")),
            shiny::p("Total Records", style = "margin:0;color:#666;"))),
          shiny::column(3, shiny::div(style = "text-align:center;",
            shiny::h2(style = "color:#388e3c;margin:5px;", n_sites),
            shiny::p("Sites", style = "margin:0;color:#666;"))),
          shiny::column(3, shiny::div(style = "text-align:center;",
            shiny::h2(style = "color:#7b1fa2;margin:5px;", n_visits),
            shiny::p("Site Visits", style = "margin:0;color:#666;"))),
          shiny::column(3, shiny::div(style = "text-align:center;",
            shiny::h2(style = "color:#d32f2f;margin:5px;",
                      ifelse(is.na(n_params), "\u2014", n_params)),
            shiny::p("Parameters", style = "margin:0;color:#666;")))
        ),
        if (!any(is.na(dr)))
          shiny::p(
            style = "margin-top:15px;text-align:center;color:#666;",
            shiny::icon("calendar"), " Date Range: ",
            shiny::tags$strong(format(dr[1], "%Y-%m-%d")), " to ",
            shiny::tags$strong(format(dr[2], "%Y-%m-%d"))
          )
      )
    })

    # -------------------------------------------------------------------------
    # Download buttons
    # -------------------------------------------------------------------------
    output$download_buttons <- shiny::renderUI({
      shiny::req(filtered_data())
      shiny::div(
        style = "margin-bottom:20px;",
        shiny::downloadButton(ns("export_csv"),
                              "Export Filtered Data (CSV)",
                              class = "btn-primary",
                              style = "margin-right:10px;"),
        shiny::downloadButton(ns("export_summary"),
                              "Export Summary (CSV)",
                              class = "btn-info")
      )
    })

    output$export_csv <- shiny::downloadHandler(
      filename = function() paste0("NRSA_Filtered_", Sys.Date(), ".csv"),
      content  = function(file) readr::write_csv(filtered_data(), file)
    )

    output$export_summary <- shiny::downloadHandler(
      filename = function() paste0("NRSA_Summary_", Sys.Date(), ".csv"),
      content  = function(file) {
        d <- filtered_data()
        d |>
          dplyr::group_by(.data[[COL_GROUP]], .data[[COL_SITE]]) |>
          dplyr::summarize(
            first_date   = min(.data[[COL_DATE]], na.rm = TRUE),
            last_date    = max(.data[[COL_DATE]], na.rm = TRUE),
            n_records    = dplyr::n(),
            n_parameters = dplyr::n_distinct(.data[[COL_PARAM]], na.rm = TRUE),
            .groups = "drop"
          ) |>
          readr::write_csv(file)
      }
    )

    # -------------------------------------------------------------------------
    # Summary table
    # -------------------------------------------------------------------------
    output$summary_table <- DT::renderDT({
      shiny::req(filtered_data())
      d <- filtered_data()

      summary_df <- d |>
        dplyr::group_by(.data[[COL_GROUP]]) |>
        dplyr::summarize(
          Site    = dplyr::first(.data[[COL_SITE]]),
          Date    = dplyr::first(.data[[COL_DATE]]),
          Project = if ("project_id" %in% names(d))
            dplyr::first(project_id) else NA_character_,
          Records = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::rename(`Visit Group` = 1)

      if (all(c(COL_TRANSECT, COL_GROUP) %in% names(d))) {
        ts <- compute_transect_summary(d)
        summary_df <- summary_df |>
          dplyr::left_join(ts, by = c("Visit Group" = "group_id")) |>
          dplyr::mutate(
            Transects = dplyr::coalesce(as.character(transect_summary), "None"),
            Complete  = dplyr::coalesce(as.character(transect_complete), "")
          ) |>
          dplyr::select(-transect_summary, -transect_complete)
      }

      DT::datatable(
        summary_df,
        selection  = "single",
        escape     = FALSE,
        rownames   = FALSE,
        extensions = "Buttons",
        options    = list(
          pageLength = 25, scrollX = TRUE,
          dom = "Bfrtip", buttons = c("copy", "csv", "excel")
        )
      )
    }, server = FALSE)

    # -------------------------------------------------------------------------
    # Return: selected group ID
    # -------------------------------------------------------------------------
    shiny::reactive({
      shiny::req(filtered_data(), input$summary_table_rows_selected)
      d         <- filtered_data()
      group_ids <- d |>
        dplyr::group_by(.data[[COL_GROUP]]) |>
        dplyr::summarize(.groups = "drop") |>
        dplyr::pull(1)
      group_ids[input$summary_table_rows_selected]
    })
  })
}
