# missing_periods() from utilities.R, internal

test_that("warns and returns a single missing period", {
  period_vec <- c("First Period", "Second Period", "Missing Period")
  possible_periods <- c("First Period", "Second Period", "Third Period")
  expect_warning(
    missing_periods(period_vec = period_vec, possible_periods = possible_periods),
    "Missing Period"
  )
  expect_identical(
    suppressWarnings(
      missing_periods(period_vec = period_vec, possible_periods = possible_periods)
    ),
    "Missing Period"
  )
})

test_that("returns unique values for missing periods", {
  period_vec <- c("First Period", "Missing Period", "Second Period", "Missing Period")
  possible_periods <- c("First Period", "Second Period", "Third Period")
  expect_warning(
    missing_periods(period_vec = period_vec, possible_periods = possible_periods),
    "Missing Period"
  )
  expect_identical(
    suppressWarnings(
      missing_periods(period_vec = period_vec, possible_periods = possible_periods)
    ),
    "Missing Period"
  )
})

test_that("warns and returns for multiple missing periods", {
  period_vec <- c("First Period", "Second Period",
                  "Missing Period", "Another Missing Period")
  possible_periods <- c("First Period", "Second Period", "Third Period")
  expect_warning(
    missing_periods(period_vec = period_vec, possible_periods = possible_periods),
    "Missing Period, Another Missing Period"
  )
  expect_identical(
    suppressWarnings(
      missing_periods(period_vec = period_vec, possible_periods = possible_periods)
    ),
    c("Missing Period", "Another Missing Period")
  )
})

test_that("character(0) if all values exist", {
  period_vec <- c("First Period", "Second Period")
  possible_periods <- c("First Period", "Second Period", "Third Period")
  expect_identical(
    missing_periods(period_vec = period_vec, possible_periods = possible_periods),
    character(0)
  )
})

test_that("works for numeric for some reason", {
  period_vec <- c(2, 3, 4)
  possible_periods <- c(1, 2, 3, 4, 5)
  expect_identical(
    missing_periods(period_vec = period_vec, possible_periods = possible_periods),
    character(0)
  )
})

test_that("works for factor in possible_periods", {
  period_vec <- c("First Period", "Second Period")
  possible_periods <- factor(c("First Period", "Second Period"), levels = c("First Period", "Second Period", "Third Period"))
  expect_identical(
    missing_periods(period_vec = period_vec, possible_periods = possible_periods),
    character(0)
  )

  period_vec <- c("First Period", "Second Period", "Missing Period")
  possible_periods <- factor(c("First Period", "Second Period"), levels = c("First Period", "Second Period", "Third Period"))
  expect_warning(
    missing_periods(period_vec = period_vec, possible_periods = possible_periods),
    "Missing Period"
  )

  expect_identical(
    suppressWarnings(
      missing_periods(period_vec = period_vec, possible_periods = possible_periods)
    ),
    c("Missing Period")
  )
})
