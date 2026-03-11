# =============================================================================
# fct_db.R
# AWQMS database connection helpers.
# The connection is sourced from a user-supplied script whose path is
# configured in inst/app/config.yml (key: awqms_source).
# =============================================================================

#' Load the AWQMS connection script
#'
#' Sources the script at the path defined in `config.yml` under `awqms_source`.
#' If `awqms_source` is `NULL` (the default profile), ODBC mode is disabled and
#' this function is a no-op.
#'
#' Call once at app startup via `load_awqms_source()` before using
#' [safe_awqms_connect()].
#'
#' @param cfg A config object from `config::get()`.  Defaults to the active
#'   profile.
#' @return Invisibly `TRUE` if sourced, `FALSE` if skipped.
#' @export
load_awqms_source <- function(cfg = config::get()) {
  path <- cfg$awqms_source
  if (is.null(path) || !nzchar(path)) {
    message("[nrsaqaapp] awqms_source is NULL — ODBC connection disabled.")
    return(invisible(FALSE))
  }
  if (!file.exists(path))
    stop(sprintf("[nrsaqaapp] AWQMS source script not found: %s", path))
  source(path, local = FALSE)
  invisible(TRUE)
}

#' Safely open an AWQMS connection
#'
#' Wraps `awqms_get_con()` (defined by the sourced connection script) in a
#' `tryCatch` so connection failures produce a Shiny notification rather than
#' crashing the session.
#'
#' @param session A Shiny session object (for `showNotification`).
#' @return A DBI connection object, or `NULL` on failure.
#' @export
safe_awqms_connect <- function(session = shiny::getDefaultReactiveDomain()) {
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
#' Wraps `awqms_disconnect()` in a `try` so a missing or already-closed
#' connection does not throw.
#'
#' @param con A DBI connection object (or `NULL`).
#' @return Invisibly `NULL`.
#' @export
safe_awqms_disconnect <- function(con) {
  if (!is.null(con))
    suppressWarnings(try(awqms_disconnect(), silent = TRUE))
  invisible(NULL)
}

#' Fetch NRSA records from AWQMS
#'
#' Pushes all filters to SQL and collects the result.  Adds
#' `parent_activity_id` and the unified `project_id` column before returning.
#'
#' @param con A DBI connection from [safe_awqms_connect()].
#' @param start Start date (`Date` scalar).
#' @param end End date (`Date` scalar).
#' @param table Character — the view/table name (from config).
#' @return A tibble of NRSA records, or `NULL` if none found.
#' @export
fetch_awqms_data <- function(con, start, end,
                              table = config::get()$awqms_table) {
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
