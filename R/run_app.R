# =============================================================================
# run_app.R
# =============================================================================

#' Run the NRSA QA Application
#'
#' @param awqms_script Optional path to an AWQMS connection script that defines
#'   \code{awqms_get_con()} and \code{awqms_disconnect()}. If \code{NULL}
#'   (default) the bundled OWRB script is used when available. Set to
#'   \code{FALSE} to force file-upload-only mode.
#' @param ... Additional arguments passed to \code{shiny::shinyApp()}.
#' @export
run_app <- function(awqms_script = NULL, ...) {

  # Resolve the AWQMS script path:
  # 1. Caller supplied an explicit path -> use it
  # 2. NULL (default) -> use the bundled script if it exists
  # 3. FALSE -> upload-only mode, no AWQMS
  if (identical(awqms_script, FALSE)) {
    awqms_path <- NULL
  } else if (!is.null(awqms_script)) {
    awqms_path <- awqms_script
  } else {
    awqms_path <- system.file("awqms/AWQMS_Source_Code.R",
                               package = "nrsaqaapp")
    if (!nzchar(awqms_path)) awqms_path <- NULL
  }

  # Source the script if we have one
  if (!is.null(awqms_path) && file.exists(awqms_path)) {
    message("[nrsaqaapp] Loading AWQMS source: ", awqms_path)
    tryCatch(
      source(awqms_path, local = FALSE),
      error = function(e) {
        warning("[nrsaqaapp] Could not load AWQMS source: ", conditionMessage(e),
                "\nRunning in file-upload-only mode.")
        awqms_path <<- NULL
      }
    )
  } else {
    message("[nrsaqaapp] No AWQMS source found — running in file-upload-only mode.")
    awqms_path <- NULL
  }

  awqms_enabled <- !is.null(awqms_path) &&
                   exists("awqms_get_con", mode = "function")

  options(shiny.maxRequestSize = 5 * 1024^3)

  shiny::shinyApp(
    ui     = app_ui(),
    server = function(input, output, session) {
      app_server(input, output, session, awqms_enabled = awqms_enabled)
    },
    ...
  )
}
