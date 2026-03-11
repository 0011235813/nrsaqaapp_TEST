# =============================================================================
# mod_batch_qa.R
# Shiny module: Batch QA tab — QA metrics across all site-visits at once.
# =============================================================================

#' Batch QA module UI
#' @param id Module namespace ID.
#' @export
mod_batch_qa_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Batch QA Summary \u2014 All Sites"),
    shiny::p("QA metrics for every site-visit in the filtered dataset.",
             style = "color:#666;margin-bottom:20px;"),
    shiny::div(
      style = "margin-bottom:15px;",
      shiny::downloadButton(ns("download_batch_qa"),
                            "Download Batch QA Report",
                            class = "btn-primary")
    ),
    DT::DTOutput(ns("batch_qa_table"))
  )
}

#' Batch QA module server
#'
#' @param id            Module namespace ID.
#' @param filtered_data A `reactive` returning the current filtered data frame.
#' @export
mod_batch_qa_server <- function(id, filtered_data) {
  shiny::moduleServer(id, function(input, output, session) {

    # -------------------------------------------------------------------------
    # Helper: build the batch summary data frame
    # -------------------------------------------------------------------------
    .build_batch <- function(d) {
      batch <- d |>
        dplyr::group_by(.data[[COL_GROUP]], .data[[COL_SITE]]) |>
        dplyr::summarize(
          Total_Records  = dplyr::n(),
          Missing_Values = if (COL_VALUE %in% names(d))
            sum(is.na(.data[[COL_VALUE]])) else NA,
          Missing_Pct    = if (COL_VALUE %in% names(d))
            round(sum(is.na(.data[[COL_VALUE]])) / dplyr::n() * 100, 1) else NA,
          N_Parameters   = dplyr::n_distinct(.data[[COL_PARAM]], na.rm = TRUE),
          Date_Sampled   = safe_min_date(.data[[COL_DATE]]),
          .groups = "drop"
        ) |>
        dplyr::rename(`Visit Group` = 1, Site = 2)

      if (COL_TRANSECT %in% names(d)) {
        ts <- compute_transect_summary(d) |>
          dplyr::mutate(
            Transects_Complete = transect_summary == "A-B-C-D-E-F-G-H-I-J-K"
          ) |>
          dplyr::select(group_id, transect_summary, Transects_Complete)
        batch <- dplyr::left_join(batch, ts,
                                   by = c("Visit Group" = "group_id")) |>
          dplyr::rename(Transects = transect_summary)
      }
      batch
    }

    # -------------------------------------------------------------------------
    # Render table
    # -------------------------------------------------------------------------
    output$batch_qa_table <- DT::renderDT({
      shiny::req(filtered_data())
      batch <- .build_batch(filtered_data())

      dt <- DT::datatable(
        batch,
        rownames   = FALSE,
        filter     = "top",
        extensions = "Buttons",
        options    = list(
          pageLength = 25, scrollX = TRUE,
          dom = "Bfrtip", buttons = c("copy", "csv", "excel")
        )
      )

      if ("Missing_Pct" %in% names(batch))
        dt <- dt |> DT::formatStyle(
          "Missing_Pct",
          backgroundColor = DT::styleInterval(
            c(5, 10, 20),
            c("#d4edda", "#fff3cd", "#f8d7da", "#f5c6cb")
          )
        )
      dt
    })

    # -------------------------------------------------------------------------
    # Download
    # -------------------------------------------------------------------------
    output$download_batch_qa <- shiny::downloadHandler(
      filename = function() paste0("NRSA_Batch_QA_", Sys.Date(), ".csv"),
      content  = function(file) {
        shiny::req(filtered_data())
        readr::write_csv(.build_batch(filtered_data()), file)
      }
    )
  })
}
