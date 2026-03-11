# =============================================================================
# mod_qa_review.R
# Shiny module: per-site QA review tab.
# Shows quality score, missing values, duplicates, transect completeness,
# value range check, method consistency, blank/duplicate checks,
# flagged records table, full data table, and statistical outliers.
# =============================================================================

#' QA Review module UI
#' @param id Module namespace ID.
#' @export
mod_qa_review_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("qa_no_selection_msg")),
    shiny::conditionalPanel(
      condition = paste0("output['", ns("qa_row_selected"), "']"),
      shiny::uiOutput(ns("qa_notifications")),
      shiny::fluidRow(
        shinydashboard::valueBoxOutput(ns("qa_score"),      width = 3),
        shinydashboard::valueBoxOutput(ns("qa_total"),      width = 3),
        shinydashboard::valueBoxOutput(ns("qa_missing"),    width = 3),
        shinydashboard::valueBoxOutput(ns("qa_duplicates"), width = 3)
      ),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(4, bslib::card(
          bslib::card_header(shiny::tags$span(
            "Transect Completeness",
            style = "font-size:1.1rem;font-weight:700;")),
          shiny::uiOutput(ns("transect_qa_detail"))
        )),
        shiny::column(4, bslib::card(
          bslib::card_header(shiny::tags$span(
            "Value Range Check",
            style = "font-size:1.1rem;font-weight:700;")),
          shiny::uiOutput(ns("range_qa_detail"))
        )),
        shiny::column(4, bslib::card(
          bslib::card_header(shiny::tags$span(
            "Method Consistency",
            style = "font-size:1.1rem;font-weight:700;")),
          shiny::uiOutput(ns("method_qa_detail"))
        ))
      ),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(6, bslib::card(
          bslib::card_header(shiny::tags$span(
            "Blank Sample Check",
            style = "font-size:1.1rem;font-weight:700;")),
          shiny::uiOutput(ns("blank_check"))
        )),
        shiny::column(6, bslib::card(
          bslib::card_header(shiny::tags$span(
            "Duplicate / Replicate Check",
            style = "font-size:1.1rem;font-weight:700;")),
          shiny::uiOutput(ns("duplicate_check"))
        ))
      ),
      shiny::br(),
      shiny::h4("QA Flagged Records"),
      DT::DTOutput(ns("qa_flagged_table")),
      shiny::br(),
      shiny::h4("Full Site Data"),
      DT::DTOutput(ns("qa_table")),
      shiny::br(),
      shiny::h4("Statistical Outliers"),
      DT::DTOutput(ns("outlier_detection")),
      shiny::br(),
      shiny::div(
        style = "text-align:right;",
        shiny::downloadButton(ns("download_qa_csv"),
                              "Download Site Data as CSV",
                              class = "btn-primary")
      )
    )
  )
}

#' QA Review module server
#'
#' @param id             Module namespace ID.
#' @param filtered_data  A `reactive` — the current filtered data frame.
#' @param selected_group A `reactive` — the selected `parent_activity_id` or NULL.
#' @export
mod_qa_review_server <- function(id, filtered_data, selected_group) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -------------------------------------------------------------------------
    # Selected site data
    # -------------------------------------------------------------------------
    selected_site_data <- shiny::reactive({
      shiny::req(filtered_data(), selected_group())
      d      <- filtered_data()
      sel_id <- selected_group()
      d |> dplyr::filter(.data[[COL_GROUP]] == sel_id)
    })

    output$qa_row_selected <- shiny::reactive({
      !is.null(tryCatch(selected_group(), error = function(e) NULL))
    })
    shiny::outputOptions(output, "qa_row_selected", suspendWhenHidden = FALSE)

    # -------------------------------------------------------------------------
    # No-selection placeholder
    # -------------------------------------------------------------------------
    output$qa_no_selection_msg <- shiny::renderUI({
      if (is.null(tryCatch(selected_group(), error = function(e) NULL)))
        shiny::div(
          style = "text-align:center;padding:100px 20px;",
          shiny::icon("info-circle",
                      style = "font-size:64px;color:#1976d2;margin-bottom:20px;"),
          shiny::h3("No Site Selected", style = "color:#666;"),
          shiny::p("Select a row in the Summary table to view QA details.",
                   style = "font-size:16px;color:#999;")
        )
    })

    # -------------------------------------------------------------------------
    # Notification banners
    # -------------------------------------------------------------------------
    output$qa_notifications <- shiny::renderUI({
      shiny::req(selected_site_data())
      d     <- selected_site_data()
      notes <- list()

      if (COL_VALUE %in% names(d) && is.numeric(d[[COL_VALUE]])) {
        mp <- sum(is.na(d[[COL_VALUE]])) / nrow(d) * 100
        if (isTRUE(mp > 20))
          notes <- c(notes, list(list(type = "warning",
            msg = paste0(round(mp, 1), "% of result values are missing"))))
      }
      if (COL_TRANSECT %in% names(d)) {
        nt <- dplyr::n_distinct(d[[COL_TRANSECT]], na.rm = TRUE)
        if (nt < 11)
          notes <- c(notes, list(list(type = "warning",
            msg = paste0("Only ", nt, " of 11 transects present"))))
      }
      if (COL_METHOD %in% names(d)) {
        nm <- dplyr::n_distinct(d[[COL_METHOD]], na.rm = TRUE)
        if (nm > 5)
          notes <- c(notes, list(list(type = "info",
            msg = paste0(nm, " different methods detected \u2014 verify consistency"))))
      }

      if (length(notes) == 0)
        shiny::div(
          style = "padding:10px;background-color:#d4edda;border-radius:4px;margin-bottom:15px;",
          shiny::icon("check-circle"), " No major quality issues detected"
        )
      else
        shiny::tagList(lapply(notes, function(n) {
          bg <- if (n$type == "warning") "#fff3cd" else "#d1ecf1"
          ic <- if (n$type == "warning") "exclamation-triangle" else "info-circle"
          shiny::div(
            style = paste0("padding:10px;background-color:", bg,
                           ";border-radius:4px;margin-bottom:8px;"),
            shiny::icon(ic), " ", n$msg
          )
        }))
    })

    # -------------------------------------------------------------------------
    # Value boxes
    # -------------------------------------------------------------------------
    output$qa_score <- shinydashboard::renderValueBox({
      shiny::req(selected_site_data())
      s   <- calculate_quality_score(selected_site_data())
      col <- if (s >= 90) "green" else if (s >= 75) "yellow" else if (s >= 60) "orange" else "red"
      shinydashboard::valueBox(paste0(s, "%"), "Quality Score",
                               shiny::icon("chart-line"), color = col)
    })
    output$qa_total <- shinydashboard::renderValueBox({
      shiny::req(selected_site_data())
      shinydashboard::valueBox(nrow(selected_site_data()), "Total Records",
                               shiny::icon("database"), color = "blue")
    })
    output$qa_missing <- shinydashboard::renderValueBox({
      shiny::req(selected_site_data())
      d <- selected_site_data()
      m <- if (COL_VALUE %in% names(d)) sum(is.na(d[[COL_VALUE]])) else 0
      shinydashboard::valueBox(m, "Missing Values",
        shiny::icon("exclamation-triangle"),
        color = if (m > 0) "yellow" else "green")
    })
    output$qa_duplicates <- shinydashboard::renderValueBox({
      shiny::req(selected_site_data())
      d        <- selected_site_data()
      key_cols <- intersect(c(COL_ACTIVITY, COL_PARAM, COL_VALUE), names(d))
      dups <- if (length(key_cols) > 0)
        d |> dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) |>
          dplyr::filter(dplyr::n() > 1) |> nrow()
      else 0L
      shinydashboard::valueBox(dups, "Potential Duplicates",
        shiny::icon("copy"),
        color = if (dups > 0) "red" else "green")
    })

    # -------------------------------------------------------------------------
    # QA Cards
    # -------------------------------------------------------------------------
    output$transect_qa_detail <- shiny::renderUI({
      shiny::req(selected_site_data())
      d <- selected_site_data()
      if (!all(c(COL_TRANSECT, COL_GROUP) %in% names(d)))
        return(shiny::p("Transect data not available.",
                        style = "color:#666;font-style:italic;"))
      info <- compute_transect_summary(d)
      if (nrow(info) == 0) return(shiny::p("No transect data found."))
      actual   <- info$transect_summary[1]
      expected <- "A-B-C-D-E-F-G-H-I-J-K"
      is_ok    <- actual == expected
      missing  <- setdiff(LETTERS[1:11], strsplit(actual, "-")[[1]])
      shiny::div(
        shiny::p(shiny::tags$strong("Expected:"), expected),
        shiny::p(shiny::tags$strong("Found:"), actual,
                 style = paste0("color:", if (is_ok) "#2e7d32" else "#c62828", ";")),
        if (is_ok)
          shiny::p(shiny::icon("check-circle"), "All transects present",
                   style = "color:#2e7d32;font-weight:600;")
        else
          shiny::p(shiny::icon("times-circle"),
                   paste("Missing:", paste(missing, collapse = ", ")),
                   style = "color:#c62828;font-weight:600;")
      )
    })

    output$range_qa_detail <- shiny::renderUI({
      shiny::req(selected_site_data())
      d <- selected_site_data()
      if (!all(c(COL_PARAM, COL_VALUE) %in% names(d)))
        return(shiny::p("Range check not available.",
                        style = "color:#666;font-style:italic;"))
      oor <- d |>
        dplyr::filter(.data[[COL_PARAM]] %in% names(PARAMETER_QA_RULES),
                      !is.na(.data[[COL_VALUE]]),
                      is.numeric(.data[[COL_VALUE]])) |>
        dplyr::rowwise() |>
        dplyr::filter({
          p <- .data[[COL_PARAM]]; v <- .data[[COL_VALUE]]
          !is.null(p) && !is.na(p) && p %in% names(PARAMETER_QA_RULES) &&
            is.numeric(v) && length(v) > 0 &&
            (v < PARAMETER_QA_RULES[[p]]$min | v > PARAMETER_QA_RULES[[p]]$max)
        }) |>
        dplyr::ungroup()
      if (nrow(oor) == 0)
        shiny::p(shiny::icon("check-circle"),
                 "All values within expected ranges",
                 style = "color:#2e7d32;font-weight:600;")
      else
        shiny::div(
          shiny::p(shiny::icon("exclamation-triangle"),
                   shiny::tags$strong(paste(nrow(oor), "value(s) outside expected range")),
                   style = "color:#ff6f00;font-weight:600;"),
          shiny::tags$ul(lapply(seq_len(min(5, nrow(oor))), function(i)
            shiny::tags$li(paste0(oor[[COL_PARAM]][i], ": ", oor[[COL_VALUE]][i])))),
          if (nrow(oor) > 5) shiny::p(paste("...and", nrow(oor) - 5, "more"))
        )
    })

    output$method_qa_detail <- shiny::renderUI({
      shiny::req(selected_site_data())
      d <- selected_site_data()
      if (!all(c(COL_PARAM, COL_METHOD) %in% names(d)))
        return(shiny::p("Method data not available.",
                        style = "color:#666;font-style:italic;"))
      mc <- d |>
        dplyr::group_by(.data[[COL_PARAM]]) |>
        dplyr::summarize(
          n_methods = dplyr::n_distinct(.data[[COL_METHOD]], na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::filter(n_methods > 1)
      if (nrow(mc) == 0)
        shiny::p(shiny::icon("check-circle"), "Methods are consistent",
                 style = "color:#2e7d32;font-weight:600;")
      else
        shiny::div(
          shiny::p(shiny::icon("exclamation-triangle"),
                   shiny::tags$strong("Multiple methods for same parameter"),
                   style = "color:#ff6f00;font-weight:600;"),
          shiny::tags$ul(lapply(seq_len(nrow(mc)), function(i)
            shiny::tags$li(paste0(mc[[COL_PARAM]][i], ": ",
                                  mc$n_methods[i], " methods"))))
        )
    })

    output$blank_check <- shiny::renderUI({
      shiny::req(selected_site_data())
      d <- selected_site_data()
      if (!COL_ACT_TYPE %in% names(d))
        return(shiny::p("Activity type data not available.",
                        style = "color:#666;font-style:italic;"))
      blanks <- d |>
        dplyr::filter(grepl("blank", .data[[COL_ACT_TYPE]], ignore.case = TRUE))
      if (nrow(blanks) == 0)
        return(shiny::p("No blank samples found.",
                        style = "color:#666;font-style:italic;"))
      detects <- blanks |>
        dplyr::filter(!is.na(.data[[COL_VALUE]]) & .data[[COL_VALUE]] > 0)
      shiny::div(
        shiny::p(shiny::tags$strong("Blank Samples:"), nrow(blanks)),
        if (nrow(detects) > 0)
          shiny::div(
            shiny::p(shiny::icon("exclamation-triangle"),
                     shiny::tags$strong(paste(nrow(detects), "blank(s) with detections")),
                     style = "color:#c62828;font-weight:600;"),
            shiny::tags$ul(lapply(seq_len(min(3, nrow(detects))), function(i)
              shiny::tags$li(paste0(detects[[COL_PARAM]][i], ": ",
                                    detects[[COL_VALUE]][i]))))
          )
        else
          shiny::p(shiny::icon("check-circle"), "All blanks non-detect",
                   style = "color:#2e7d32;font-weight:600;")
      )
    })

    output$duplicate_check <- shiny::renderUI({
      shiny::req(selected_site_data())
      d <- selected_site_data()
      if (!COL_ACT_TYPE %in% names(d))
        return(shiny::p("Activity type data not available.",
                        style = "color:#666;font-style:italic;"))
      dups <- d |>
        dplyr::filter(grepl("duplicate|replicate", .data[[COL_ACT_TYPE]],
                            ignore.case = TRUE))
      if (nrow(dups) == 0)
        return(shiny::p("No duplicate/replicate samples found.",
                        style = "color:#666;font-style:italic;"))
      shiny::p(shiny::tags$strong("Duplicate/Replicate Samples:"), nrow(dups))
    })

    # -------------------------------------------------------------------------
    # Tables
    # -------------------------------------------------------------------------
    output$qa_flagged_table <- DT::renderDT({
      shiny::req(selected_site_data())
      flagged <- apply_qa_flags(selected_site_data()) |>
        dplyr::filter(qa_flag != "Pass")
      cols <- intersect(
        c(COL_ACTIVITY, COL_DATE, COL_PARAM, COL_VALUE, COL_UNIT, "qa_flag"),
        names(flagged)
      )
      if (nrow(flagged) == 0)
        DT::datatable(data.frame(Message = "No QA flags \u2014 all records pass"),
                      rownames = FALSE, options = list(dom = "t"))
      else
        DT::datatable(flagged |> dplyr::select(dplyr::all_of(cols)),
                      rownames = FALSE,
                      options  = list(pageLength = 25, dom = "tp")) |>
          DT::formatStyle("qa_flag",
            backgroundColor = DT::styleEqual(
              c("Missing", "Out of Range"), c("#fff3cd", "#f8d7da")
            ),
            fontWeight = "bold")
    })

    output$qa_table <- DT::renderDT({
      shiny::req(selected_site_data())
      d    <- selected_site_data()
      cols <- intersect(
        c(COL_ACTIVITY, COL_DATE, COL_PARAM, COL_VALUE, COL_UNIT,
          COL_METHOD, COL_TRANSECT, COL_STATUS),
        names(d)
      )
      DT::datatable(
        d |> dplyr::select(dplyr::all_of(cols)),
        rownames   = FALSE,
        filter     = "top",
        extensions = c("Buttons", "FixedColumns"),
        options    = list(
          pageLength   = 50, scrollX = TRUE, scrollY = "400px",
          dom          = "Bfrtip",
          buttons      = c("copy", "csv", "excel"),
          fixedColumns = list(leftColumns = 2)
        )
      )
    })

    output$outlier_detection <- DT::renderDT({
      shiny::req(selected_site_data())
      d <- selected_site_data()
      if (!all(c(COL_PARAM, COL_VALUE) %in% names(d)))
        return(DT::datatable(
          data.frame(Message = "Outlier detection not available"),
          rownames = FALSE, options = list(dom = "t")
        ))
      params  <- unique(d[[COL_PARAM]][!is.na(d[[COL_PARAM]])])
      all_out <- dplyr::bind_rows(lapply(params, detect_outliers, data = d))
      if (nrow(all_out) == 0)
        DT::datatable(
          data.frame(Message = "No statistical outliers detected"),
          rownames = FALSE, options = list(dom = "t")
        )
      else {
        cols <- intersect(
          c(COL_ACTIVITY, COL_PARAM, COL_VALUE, COL_UNIT, "outlier_type"),
          names(all_out)
        )
        DT::datatable(all_out |> dplyr::select(dplyr::all_of(cols)),
                      rownames = FALSE,
                      options  = list(pageLength = 10, dom = "tp")) |>
          DT::formatStyle("outlier_type",
            backgroundColor = DT::styleEqual(
              c("Low", "High"), c("#fff3cd", "#f8d7da")
            ))
      }
    })

    output$download_qa_csv <- shiny::downloadHandler(
      filename = function() {
        site <- dplyr::first(selected_site_data()[[COL_SITE]])
        paste0("NRSA_QA_", site, "_", Sys.Date(), ".csv")
      },
      content = function(file) readr::write_csv(selected_site_data(), file)
    )
  })
}
