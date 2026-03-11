test_that("%||% returns first value when non-NULL", {
  expect_equal("a" %||% "b", "a")
  expect_equal(0L  %||% 99L, 0L)
})

test_that("%||% returns fallback when NULL", {
  expect_equal(NULL %||% "b", "b")
})

test_that("safe_date returns NULL for NULL input", {
  expect_null(safe_date(NULL))
})

test_that("safe_date returns NULL for empty string", {
  expect_null(safe_date(""))
  expect_null(safe_date("   "))
})

test_that("safe_date returns NULL for NA", {
  expect_null(safe_date(NA))
})

test_that("safe_date parses a valid date string", {
  d <- safe_date("2023-07-15")
  expect_s3_class(d, "Date")
  expect_equal(d, as.Date("2023-07-15"))
})

test_that("safe_min_date returns NA for all-NA input", {
  x <- as.Date(c(NA, NA, NA))
  expect_equal(safe_min_date(x), as.Date(NA))
})

test_that("safe_min_date returns minimum of non-NA values", {
  x <- as.Date(c("2023-01-01", NA, "2022-06-15"))
  expect_equal(safe_min_date(x), as.Date("2022-06-15"))
})
