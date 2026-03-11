# =============================================================================
# fct_data.R
# Data loading, type coercion, and schema harmonization.
# These functions are pure (no Shiny reactivity) and are fully unit-testable.
# =============================================================================

#' Add a unified project_id column
#'
#' Collapses project_id1–project_id6 into a single `project_id` column
#' containing the first non-empty value for each row.
#'
#' @param data A data frame containing one or more `project_id*` columns.
#' @return The input data frame with a new `project_id` character column.
#' @export
add_project_col <- function(data) {
  present <- intersect(PROJECT_COLS, names(data))
  if (length(present) == 0) {
    data$project_id <- NA_character_
    return(data)
  }
  data$project_id <- apply(data[, present, drop = FALSE], 1, function(r) {
    v <- r[!is.na(r) & nchar(trimws(r)) > 0]
    if (length(v)) v[[1]] else NA_character_
  })
  data
}

#' Coerce activity_start_date to Date
#'
#' Safe no-op if the column is already a `Date`.
#'
#' @param data A data frame.
#' @return The data frame with `activity_start_date` as `Date`.
#' @export
coerce_dates <- function(data) {
  if (COL_DATE %in% names(data) && !inherits(data[[COL_DATE]], "Date"))
    data[[COL_DATE]] <- as.Date(data[[COL_DATE]])
  data
}

#' Coerce result_measure to numeric
#'
#' Strips detection-limit prefixes (`<0.1`, `>100`, etc.) before coercing.
#' Non-parseable values become `NA` rather than throwing an error.
#' Also handles list-columns that sometimes arise with certain ODBC drivers.
#'
#' @param data A data frame.
#' @return The data frame with `result_measure` as `numeric`.
#' @export
coerce_numeric <- function(data) {
  if (COL_VALUE %in% names(data)) {
    v <- data[[COL_VALUE]]
    if (is.list(v)) v <- unlist(v)
    if (!is.numeric(v))
      v <- suppressWarnings(
        as.numeric(gsub("^[<>]=?\\s*", "", as.character(v)))
      )
    data[[COL_VALUE]] <- v
  }
  data
}

#' Read an uploaded Excel file
#'
#' Forces all columns to character type on read to prevent `readxl` from
#' guessing `logical` for sparsely-populated columns and then warning when
#' it encounters real values further down.  Type conversion is handled
#' downstream by [normalize_core_types()] and [coerce_numeric()].
#'
#' @param path Local file path (the `datapath` from Shiny's `fileInput`).
#' @param name Original file name (used to detect extension).
#' @return A tibble with all columns as character.
#' @export
read_uploaded_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls"))
    readxl::read_excel(path, col_types = "text")
  else
    stop(sprintf(
      "Unsupported file type: .%s — please upload .xlsx or .xls files only", ext
    ))
}

#' Harmonize an uploaded data frame to the AWQMS column schema
#'
#' 1. Snake-cases all column names.
#' 2. Applies synonym mappings for known alternate column names.
#' 3. Falls back latitude/longitude from `activity_*` columns if the
#'    `monitoring_location_*` equivalents are absent.
#' 4. Adds any missing ODBC columns as `NA`.
#'
#' @param df A data frame from [read_uploaded_file()].
#' @return A data frame aligned to [ODBC_COLUMNS].
#' @export
harmonize_to_odbc_schema <- function(df) {
  names(df) <- to_snake_case(names(df))

  synonym_map <- c(
    sampling_component_quadrat = "sampling_component_name",
    result_value               = "result_measure",
    activity_latitude          = "activity_latitude",
    activity_longitude         = "activity_longitude"
  )
  for (src in names(synonym_map)) {
    dest <- synonym_map[[src]]
    if (src %in% names(df) && !(dest %in% names(df)))
      names(df)[names(df) == src] <- dest
  }

  if (!("monitoring_location_latitude" %in% names(df)) &&
      "activity_latitude" %in% names(df))
    df$monitoring_location_latitude <- df$activity_latitude

  if (!("monitoring_location_longitude" %in% names(df)) &&
      "activity_longitude" %in% names(df))
    df$monitoring_location_longitude <- df$activity_longitude

  for (col in setdiff(ODBC_COLUMNS, names(df)))
    df[[col]] <- NA

  df
}

#' Normalize core column types after upload
#'
#' Attempts to parse `activity_start_date` through four strategies in order:
#' 1. ISO `YYYY-MM-DD`
#' 2. `M/D/YYYY` (common Excel export format)
#' 3. `D-Mon-YYYY` (e.g. `15-Jul-2024`)
#' 4. Excel numeric serial number (result of `col_types = "text"`)
#'
#' Attaches a `date_parse_failures` attribute to the returned data frame if
#' any non-empty date values could not be parsed, so the Shiny upload handler
#' can surface a warning to the user rather than silently dropping dates.
#'
#' Also coerces latitude and longitude to numeric.
#'
#' @param df A data frame from [harmonize_to_odbc_schema()].
#' @return The data frame with typed columns; may carry a
#'   `date_parse_failures` attribute (list with `n` and `samples`).
#' @export
normalize_core_types <- function(df) {
  if ("activity_start_date" %in% names(df)) {
    raw <- df$activity_start_date

    parsed <- suppressWarnings({
      # 1. ISO
      out <- as.Date(raw, format = "%Y-%m-%d")

      # 2. M/D/Y
      na_idx <- is.na(out) & !is.na(raw) & nchar(trimws(raw)) > 0
      if (any(na_idx))
        out[na_idx] <- as.Date(raw[na_idx], format = "%m/%d/%Y")

      # 3. D-Mon-Y
      na_idx <- is.na(out) & !is.na(raw) & nchar(trimws(raw)) > 0
      if (any(na_idx))
        out[na_idx] <- as.Date(raw[na_idx], format = "%d-%b-%Y")

      # 4. Excel numeric serial
      na_idx <- is.na(out) & !is.na(raw) & nchar(trimws(raw)) > 0
      if (any(na_idx)) {
        serial <- suppressWarnings(as.numeric(raw[na_idx]))
        valid  <- !is.na(serial)
        if (any(valid))
          out[na_idx][valid] <- as.Date(serial[valid] - 1, origin = "1899-12-30")
      }
      out
    })

    failed_mask <- is.na(parsed) & !is.na(raw) & nchar(trimws(raw)) > 0
    if (any(failed_mask))
      attr(df, "date_parse_failures") <- list(
        n       = sum(failed_mask),
        samples = head(unique(raw[failed_mask]), 10)
      )

    df$activity_start_date <- parsed
  }

  for (col in c("monitoring_location_latitude", "monitoring_location_longitude"))
    if (col %in% names(df))
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))

  df
}

#' Apply base NRSA filters to a data frame
#'
#' Retains only rows where `activity_media == "Habitat"` and
#' `monitoring_location_type == "River/Stream"`, then derives
#' `parent_activity_id` from `activity_id`.
#'
#' @param df A combined data frame.
#' @return Filtered data frame with `parent_activity_id` column added.
#' @export
apply_nrsa_filters <- function(df) {
  if ("activity_id" %in% names(df))
    df$parent_activity_id <- sub(":.*$", "", df$activity_id)
  if ("activity_media" %in% names(df))
    df <- df[df$activity_media == "Habitat", ]
  if ("monitoring_location_type" %in% names(df))
    df <- df[df$monitoring_location_type == "River/Stream", ]
  df
}
