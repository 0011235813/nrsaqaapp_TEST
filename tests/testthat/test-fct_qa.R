test_that("detect_outliers returns empty frame for fewer than 4 values", {
  df <- data.frame(
    characteristic_name = "pH",
    result_measure      = c(7.0, 7.2, 7.1),
    stringsAsFactors    = FALSE
  )
  out <- detect_outliers(df, "pH")
  expect_equal(nrow(out), 0L)
})

test_that("detect_outliers flags obvious outliers", {
  df <- data.frame(
    characteristic_name = rep("pH", 10),
    result_measure      = c(7.0, 7.1, 6.9, 7.0, 7.2, 7.1, 7.0, 6.8, 0.1, 14.0),
    stringsAsFactors    = FALSE
  )
  out <- detect_outliers(df, "pH")
  expect_true(nrow(out) >= 1)
  expect_true(all(out$outlier_type %in% c("Low", "High")))
})

test_that("calculate_quality_score returns 100 for clean data", {
  df <- data.frame(
    result_measure      = rep(7.0, 20),
    characteristic_name = rep("pH", 20),
    sampling_component_name = LETTERS[1:11][c(1:11, 1:9)],
    stringsAsFactors    = FALSE
  )
  # All 11 transects present, no missing, no outliers
  df2 <- data.frame(
    result_measure      = rep(7.0, 11),
    characteristic_name = rep("pH", 11),
    sampling_component_name = LETTERS[1:11],
    stringsAsFactors    = FALSE
  )
  score <- calculate_quality_score(df2)
  expect_equal(score, 100)
})

test_that("calculate_quality_score penalises missing transects", {
  df <- data.frame(
    result_measure      = rep(7.0, 5),
    characteristic_name = rep("pH", 5),
    sampling_component_name = LETTERS[1:5],
    stringsAsFactors    = FALSE
  )
  score <- calculate_quality_score(df)
  expect_lt(score, 100)
})

test_that("apply_qa_flags marks out-of-range pH values", {
  df <- data.frame(
    characteristic_name = c("pH", "pH", "pH"),
    result_measure      = c(7.0, -1.0, 15.0),
    stringsAsFactors    = FALSE
  )
  out <- apply_qa_flags(df)
  expect_equal(out$qa_flag, c("Pass", "Out of Range", "Out of Range"))
})

test_that("apply_qa_flags marks missing values", {
  df <- data.frame(
    characteristic_name = c("pH", "pH"),
    result_measure      = c(7.0, NA),
    stringsAsFactors    = FALSE
  )
  out <- apply_qa_flags(df)
  expect_equal(out$qa_flag, c("Pass", "Missing"))
})

test_that("compute_transect_summary identifies complete transect sets", {
  df <- data.frame(
    parent_activity_id      = rep("VISIT1", 11),
    sampling_component_name = LETTERS[1:11],
    stringsAsFactors        = FALSE
  )
  out <- compute_transect_summary(df)
  expect_equal(out$transect_summary, "A-B-C-D-E-F-G-H-I-J-K")
  expect_true(grepl("10004", out$transect_complete))  # tick mark
})

test_that("compute_transect_summary flags incomplete transects", {
  df <- data.frame(
    parent_activity_id      = rep("VISIT1", 5),
    sampling_component_name = LETTERS[1:5],
    stringsAsFactors        = FALSE
  )
  out <- compute_transect_summary(df)
  expect_true(grepl("10006", out$transect_complete))  # cross mark
})
