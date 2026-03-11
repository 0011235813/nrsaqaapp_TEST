test_that("to_snake_case converts correctly", {
  expect_equal(to_snake_case("Activity Start Date"), "activity_start_date")
  expect_equal(to_snake_case("Result_Value"),        "result_value")
  expect_equal(to_snake_case("pH"),                  "p_h")
})

test_that("coerce_numeric strips detection limit prefixes", {
  df <- data.frame(result_measure = c("<0.1", ">100", "5.5", NA, "bad"),
                   stringsAsFactors = FALSE)
  out <- coerce_numeric(df)
  expect_equal(out$result_measure, c(0.1, 100, 5.5, NA, NA))
  expect_true(is.numeric(out$result_measure))
})

test_that("coerce_dates converts dttm to Date", {
  df <- data.frame(
    activity_start_date = as.POSIXct("2023-07-15 10:30:00"),
    stringsAsFactors    = FALSE
  )
  out <- coerce_dates(df)
  expect_s3_class(out$activity_start_date, "Date")
  expect_equal(out$activity_start_date, as.Date("2023-07-15"))
})

test_that("normalize_core_types parses ISO dates", {
  df  <- data.frame(activity_start_date = "2023-07-15", stringsAsFactors = FALSE)
  out <- normalize_core_types(df)
  expect_equal(out$activity_start_date, as.Date("2023-07-15"))
  expect_null(attr(out, "date_parse_failures"))
})

test_that("normalize_core_types parses M/D/Y dates", {
  df  <- data.frame(activity_start_date = "7/15/2023", stringsAsFactors = FALSE)
  out <- normalize_core_types(df)
  expect_equal(out$activity_start_date, as.Date("2023-07-15"))
})

test_that("normalize_core_types parses Excel serial dates", {
  # 45122 = 2023-07-14 in Excel (origin 1899-12-30, subtract 1)
  df  <- data.frame(activity_start_date = "45122", stringsAsFactors = FALSE)
  out <- normalize_core_types(df)
  expect_s3_class(out$activity_start_date, "Date")
  # Confirm it's in plausible range
  expect_true(out$activity_start_date >= as.Date("2020-01-01"))
})

test_that("normalize_core_types attaches failure attribute for bad dates", {
  df  <- data.frame(activity_start_date = "not-a-date", stringsAsFactors = FALSE)
  out <- normalize_core_types(df)
  fail <- attr(out, "date_parse_failures")
  expect_false(is.null(fail))
  expect_equal(fail$n, 1L)
  expect_true("not-a-date" %in% fail$samples)
})

test_that("add_project_col picks first non-NA project", {
  df <- data.frame(
    project_id1 = c(NA,       "FWTrend"),
    project_id2 = c("FWProb", NA),
    stringsAsFactors = FALSE
  )
  out <- add_project_col(df)
  expect_equal(out$project_id, c("FWProb", "FWTrend"))
})

test_that("apply_nrsa_filters retains only Habitat / River-Stream rows", {
  df <- data.frame(
    activity_id              = c("A:1", "B:1", "C:1"),
    activity_media           = c("Habitat", "Water", "Habitat"),
    monitoring_location_type = c("River/Stream", "River/Stream", "Lake"),
    stringsAsFactors         = FALSE
  )
  out <- apply_nrsa_filters(df)
  expect_equal(nrow(out), 1L)
  expect_equal(out$activity_id, "A:1")
  expect_true("parent_activity_id" %in% names(out))
  expect_equal(out$parent_activity_id, "A")
})
