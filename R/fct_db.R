# =============================================================================
# fct_db.R
# AWQMS database connection helpers.
# The connection script (defining awqms_get_con / awqms_disconnect) is sourced
# at startup by run_app() — these helpers just use those functions safely.
# =============================================================================

#' Safely open an AWQMS connection
#'
#' Wraps \code{awqms_get_con()} in a tryCatch so connection failures produce
#' a Shiny notification rather than crashing the session.
#'
#' @param session A Shiny session object.
#' @return A DBI connection object, or NULL on failure.
#' @export
safe_awqms_connect <- function(session = shiny::getDefaultReactiveDomain()) {
  if (!exists("awqms_get_con", mode = "function")) {
    shiny::showNotification(
      "AWQMS connection function not found. The connection script may not have loaded correctly.",
      type = "error", duration = 10, session = session
    )
    return(NULL)
  }
  tryCatch(
    awqms_get_con(),
    error = function(e) {
      shiny::showNotification(
        paste("ODBC connection failed:", conditionMessage(e)),
        type = "error", duration = 10, session = session
      )
      NULL
    }
  )
}

#' Safely close an AWQMS connection
#'
#' @param con A DBI connection object (or NULL).
#' @return Invisibly NULL.
#' @export
safe_awqms_disconnect <- function(con) {
  if (!is.null(con) && exists("awqms_disconnect", mode = "function"))
    suppressWarnings(try(awqms_disconnect(), silent = TRUE))
  invisible(NULL)
}

#' Fetch NRSA records from AWQMS
#'
#' @param con A DBI connection from \code{safe_awqms_connect()}.
#' @param start Start date (Date scalar).
#' @param end End date (Date scalar).
#' @param table The view/table name to query. Defaults to "results_standard_vw".
#' @return A tibble of NRSA records, or NULL if none found.
#' @export
fetch_awqms_data <- function(con, start, end,
                              table = "results_standard_vw") {
  raw <- dplyr::tbl(con, table) |>
    dplyr::filter(
      activity_media           == "Habitat",
      monitoring_location_type == "River/Stream",
      (project_id1 %in% NRSA_PROJECT_IDS | project_id2 %in% NRSA_PROJECT_IDS |
         project_id3 %in% NRSA_PROJECT_IDS | project_id4 %in% NRSA_PROJECT_IDS |
         project_id5 %in% NRSA_PROJECT_IDS | project_id6 %in% NRSA_PROJECT_IDS),
      activity_start_date >= as.Date(start),
      activity_start_date <= as.Date(end)
    ) |>
    dplyr::collect()

  if (nrow(raw) == 0) return(NULL)

  raw |>
    dplyr::mutate(parent_activity_id = sub(":.*$", "", activity_id)) |>
    add_project_col() |>
    coerce_dates() |>
    coerce_numeric()
}
