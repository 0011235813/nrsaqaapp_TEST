# =============================================================================
# constants.R
# Column name constants and NRSA project identifiers.
# All references to raw column names go through these — if the database schema
# ever changes, only this file needs updating.
# =============================================================================

#' @keywords internal
COL_SITE     <- "monitoring_location_id"
COL_DATE     <- "activity_start_date"
COL_LAT      <- "monitoring_location_latitude"
COL_LON      <- "monitoring_location_longitude"
COL_TRANSECT <- "sampling_component_name"
COL_PARAM    <- "characteristic_name"
COL_VALUE    <- "result_measure"
COL_UNIT     <- "result_measure_unit"
COL_METHOD   <- "method_speciation"
COL_STATUS   <- "result_status"
COL_ACTIVITY <- "activity_id"
COL_ACT_TYPE <- "activity_type"
COL_GROUP    <- "parent_activity_id"   # derived: sub(":.*$", "", activity_id)
COL_LOC_NAME <- "monitoring_location_name"

#' @keywords internal
PROJECT_COLS <- paste0("project_id", 1:6)

#' NRSA Project IDs
#'
#' The set of project identifiers used to filter NRSA records from AWQMS.
#' Records must have at least one of these values across project_id1–project_id6.
#'
#' @export
NRSA_PROJECT_IDS <- c(
  "FWProb08-12", "FWProb13-17", "FWProb18-22", "FWProb23-27",
  "FWTrend", "FWFire2019", "FWProbAll", "AncillaryHabitat", "NRSA-Data"
)

#' Expected columns in the AWQMS results view
#' @keywords internal
ODBC_COLUMNS <- c(
  "activity_media", "monitoring_location_type", "activity_start_date",
  "sample_collection_method_id",
  "project_id1", "project_id2", "project_id3",
  "project_id4", "project_id5", "project_id6",
  "activity_id", "sampling_component_name",
  "monitoring_location_id", "monitoring_location_name",
  "monitoring_location_latitude", "monitoring_location_longitude",
  "result_measure", "characteristic_name", "analytical_method_id",
  "result_measure_unit", "method_speciation", "result_status"
)

#' QA range rules per parameter
#'
#' Named list of min/max acceptable values for known parameters.
#' Used by `apply_qa_flags()` to flag out-of-range results.
#'
#' @export
PARAMETER_QA_RULES <- list(
  "Temperature, water"            = list(min = -5,    max = 50),
  "pH"                            = list(min = 0,     max = 14),
  "Dissolved oxygen (DO)"         = list(min = 0,     max = 20),
  "Conductivity"                  = list(min = 0,     max = 5000),
  "Turbidity"                     = list(min = 0,     max = 1000),
  "Total suspended solids"        = list(min = 0,     max = 10000),
  "Chlorophyll a"                 = list(min = 0,     max = 500),
  "Total nitrogen, mixed forms"   = list(min = 0,     max = 50),
  "Total phosphorus, mixed forms" = list(min = 0,     max = 10)
)
