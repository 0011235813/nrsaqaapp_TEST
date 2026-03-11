# =============================================================================
# utils.R
# Small general-purpose helpers used across the app.
# =============================================================================

#' Null-coalescing operator
#'
#' Returns `a` if it is not NULL, otherwise `b`.
#'
#' @param a First value.
#' @param b Fallback value.
#' @return `a` if non-NULL, else `b`.
#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Safely parse a date from any input type
#'
#' Handles Date, POSIXct, character, empty string, and NULL without throwing.
#'
#' @param x Input value.
#' @return A `Date` or `NULL`.
#' @keywords internal
safe_date <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  if (anyNA(x) && length(x) == 1)   return(NULL)
  if (is.character(x) && nchar(trimws(x)) == 0) return(NULL)
  d <- suppressWarnings(as.Date(x))
  if (is.na(d)) NULL else d
}

#' Safe minimum date — returns NA instead of Inf when all values are missing
#'
#' Avoids the `min.default: no non-missing arguments` warning that arises when
#' an entire group has NA dates in a `summarize()` call.
#'
#' @param x A vector of `Date` values (may be all NA).
#' @return A single `Date`, or `as.Date(NA)`.
#' @keywords internal
safe_min_date <- function(x) {
  dv <- x[!is.na(x)]
  if (length(dv) == 0) as.Date(NA) else min(dv)
}

#' Convert a character vector to snake_case
#'
#' Used to normalise column names from uploaded Excel files.
#'
#' @param x Character vector.
#' @return Character vector in snake_case.
#' @keywords internal
to_snake_case <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}
