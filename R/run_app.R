# =============================================================================
# run_app.R
# Package entry point — the function other agencies call to launch the app.
# =============================================================================

#' Run the NRSA QA Application
#'
#' Launches the Shiny app, loads the AWQMS connection script if configured,
#' and applies upload size limits from config.
#'
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#' @param config_profile Character — the `config.yml` profile to use.
#'   Defaults to the value of the `R_CONFIG_ACTIVE` environment variable,
#'   or `"default"` if unset.
#'
#' @examples
#' \dontrun{
#' # Default (upload-only) mode:
#' nrsaqaapp::run_app()
#'
#' # OWRB AWQMS mode:
#' Sys.setenv(R_CONFIG_ACTIVE = "owrb")
#' nrsaqaapp::run_app()
#'
#' # Another agency's config:
#' Sys.setenv(R_CONFIG_ACTIVE = "myagency")
#' nrsaqaapp::run_app()
#' }
#'
#' @export
run_app <- function(..., config_profile = Sys.getenv("R_CONFIG_ACTIVE", "default")) {
  Sys.setenv(R_CONFIG_ACTIVE = config_profile)

  # Locate config.yml:
  # 1. Check working directory first — allows users to override with their own copy
  # 2. Fall back to the copy bundled inside the installed package
  config_file <- if (file.exists("config.yml")) {
    "config.yml"
  } else {
    system.file("app/config.yml", package = "nrsaqaapp")
  }

  cfg <- tryCatch(
    config::get(file = config_file),
    error = function(e) list(
      awqms_source     = NULL,
      awqms_table      = "results_standard_vw",
      max_upload_bytes = 5 * 1024^3
    )
  )

  # Apply upload size limit from config
  options(shiny.maxRequestSize = cfg$max_upload_bytes %||% (5 * 1024^3))

  # Source AWQMS connection script if configured
  tryCatch(
    load_awqms_source(cfg),
    error = function(e) {
      warning("[nrsaqaapp] Could not load AWQMS source: ", conditionMessage(e),
              "\nRunning in file-upload-only mode.")
    }
  )

  shiny::shinyApp(ui = app_ui, server = app_server, ...)
}
