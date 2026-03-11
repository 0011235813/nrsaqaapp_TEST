# =============================================================================
# fct_qa.R
# QA computation functions — outlier detection, quality scoring,
# transect completeness, and QA flagging.
# All functions are pure (no Shiny reactivity) and fully unit-testable.
# =============================================================================

#' Compute transect completeness summary
#'
#' Groups records by visit (`parent_activity_id`) and summarises which
#' transect letters (A–K) were sampled.  Adds an HTML tick/cross indicator
#' for use in `DT::datatable()` with `escape = FALSE`.
#'
#' @param data A filtered data frame containing [COL_TRANSECT] and [COL_GROUP].
#' @return A data frame with columns `group_id`, `transect_summary`, and
#'   `transect_complete` (HTML string).
#' @export
compute_transect_summary <- function(data) {
  expected <- paste(LETTERS[1:11], collapse = "-")
  data |>
    dplyr::filter(!is.na(.data[[COL_TRANSECT]]) & .data[[COL_TRANSECT]] != "") |>
    dplyr::group_by(.data[[COL_GROUP]]) |>
    dplyr::summarize(
      transect_summary = paste(sort(unique(.data[[COL_TRANSECT]])), collapse = "-"),
      .groups = "drop"
    ) |>
    dplyr::rename(group_id = 1) |>
    dplyr::mutate(
      transect_complete = ifelse(
        transect_summary == expected,
        "<span style='color:#2e7d32;font-weight:700;'>&#10004;</span>",
        "<span style='color:#c62828;font-weight:700;'>&#10006;</span>"
      )
    )
}

#' Detect statistical outliers using the IQR method
#'
#' Returns records whose value falls below Q1 - 1.5*IQR or above
#' Q3 + 1.5*IQR.  Requires at least 4 non-missing values to compute
#' quartiles; returns an empty data frame otherwise.
#'
#' @param data A data frame for a single site-visit.
#' @param param Character scalar — the parameter name to check.
#' @return A data frame (subset of `data`) with an added `outlier_type`
#'   column (`"Low"` or `"High"`), or an empty data frame.
#' @export
detect_outliers <- function(data, param) {
  empty <- data.frame(outlier_type = character(0))
  pd    <- data |> dplyr::filter(.data[[COL_PARAM]] == param,
                                  !is.na(.data[[COL_VALUE]]))
  if (nrow(pd) < 4) return(empty)
  vals <- pd[[COL_VALUE]]
  if (!is.numeric(vals)) return(empty)
  Q1      <- stats::quantile(vals, 0.25, na.rm = TRUE)
  Q3      <- stats::quantile(vals, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  result  <- pd |>
    dplyr::filter(.data[[COL_VALUE]] < Q1 - 1.5 * IQR_val |
                    .data[[COL_VALUE]] > Q3 + 1.5 * IQR_val) |>
    dplyr::mutate(
      outlier_type = ifelse(.data[[COL_VALUE]] < Q1 - 1.5 * IQR_val, "Low", "High")
    )
  if (nrow(result) == 0) return(empty)
  result
}

#' Calculate an overall quality score for a site-visit (0–100)
#'
#' Deductions:
#' - 0.5 points per 1% missing result values
#' - 2 points per statistical outlier record
#' - 5 points per missing transect (out of 11 expected)
#'
#' @param d A data frame for a single site-visit.
#' @return A numeric scalar between 0 and 100.
#' @export
calculate_quality_score <- function(d) {
  score <- 100

  if (COL_VALUE %in% names(d) && is.numeric(d[[COL_VALUE]])) {
    mp    <- sum(is.na(d[[COL_VALUE]])) / nrow(d) * 100
    score <- score - mp * 0.5

    params <- unique(d[[COL_PARAM]][!is.na(d[[COL_PARAM]])])
    oc <- sum(vapply(params, function(p) {
      pd   <- d[d[[COL_PARAM]] == p, ]
      vals <- pd[[COL_VALUE]]
      if (length(vals[!is.na(vals)]) < 4) return(0L)
      Q1      <- stats::quantile(vals, 0.25, na.rm = TRUE)
      Q3      <- stats::quantile(vals, 0.75, na.rm = TRUE)
      IQR_val <- Q3 - Q1
      sum(vals < Q1 - 1.5 * IQR_val | vals > Q3 + 1.5 * IQR_val, na.rm = TRUE)
    }, integer(1)))
    score <- score - oc * 2
  }

  if (COL_TRANSECT %in% names(d)) {
    nt    <- dplyr::n_distinct(d[[COL_TRANSECT]], na.rm = TRUE)
    if (nt < 11) score <- score - (11 - nt) * 5
  }

  max(0, min(100, round(score, 1)))
}

#' Apply QA flags to a data frame
#'
#' Adds a `qa_flag` column with values `"Pass"`, `"Missing"`, or
#' `"Out of Range"` based on [PARAMETER_QA_RULES].
#'
#' @param data A data frame containing [COL_PARAM] and [COL_VALUE].
#' @return The input data frame with a `qa_flag` column added.
#' @export
apply_qa_flags <- function(data) {
  if (!all(c(COL_PARAM, COL_VALUE) %in% names(data))) return(data)
  if (!is.numeric(data[[COL_VALUE]]))
    return(dplyr::mutate(data, qa_flag = "Pass"))

  data |>
    dplyr::rowwise() |>
    dplyr::mutate(qa_flag = {
      val   <- .data[[COL_VALUE]]
      param <- .data[[COL_PARAM]]
      if (is.null(val)   || length(val)   == 0) "Pass"
      else if (is.null(param) || length(param) == 0) "Pass"
      else if (is.na(val)) "Missing"
      else if (!is.na(param) && param %in% names(PARAMETER_QA_RULES)) {
        r <- PARAMETER_QA_RULES[[param]]
        if (val < r$min | val > r$max) "Out of Range" else "Pass"
      } else "Pass"
    }) |>
    dplyr::ungroup()
}
