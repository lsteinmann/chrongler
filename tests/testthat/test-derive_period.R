## tests for derive_period, from derive_period.R


test_conc <- make_chrongler_conc(
  data.frame(
    values = c("Period 1", "Period 2", "Period 3"),
    group = c("Group A", "Group A", "Group B"),
    dating.min = c(-100, -49, 100),
    dating.max = c(-50, 99, 150)
  )
)
test_data <- data.frame(
  id = c("Obj_1", "Obj_2", "Obj_3"),
  expected_start = c("Period 1", "Period 2", "Period 3"),
  min = c(-100, -49, 100),
  expected_end = c("Period 2", "Period 3", "Period 3"),
  max = c(99, 150, 150)
)

#### Basics
test_that("fails if conc is not a chrongler.conc", {
  expect_error(
    derive_period(data = test_data, conc = list(1, 2, 3)),
    "chrongler.conc"
  )
})

test_that("fails if column does not exist", {
  data <- test_data
  expect_error(
    derive_period(data = data, conc = test_conc,
                  min = "not_a_column", max = "max"),
    "not_a_column"
  )
  expect_error(
    derive_period(data = data, conc = test_conc,
                  min = "min", max = "not_a_column"),
    "not_a_column"
  )
})

test_that("keeps other columns intact", {
  data <- test_data
  data$unrelated_column <- rep("another thing", nrow(data))
  derived <- derive_period(data = data, conc = test_conc,
                           min = "min", max = "max")
  expect_identical(
    derived$unrelated_column, data$unrelated_column
  )
})


## Periods
test_that("derives both periods as expected", {
  data <- test_data
  derived <- derive_period(data = data, conc = test_conc,
                           min = "min", max = "max")
  expect_equal(
    derived$period.start, data$expected_start
  )
  expect_equal(
    derived$period.end, data$expected_end
  )
})


test_that("NA for dates outside periods", {
  data <- data.frame(
    min = c(-200, -500, 500),
    max = c(600, -150, 700)
  )
  res <- derive_period(data = data, conc = test_conc,
                       min = "min", max = "max")
  expect_all_true(is.na(res$period.start))
  expect_all_true(is.na(res$period.end))
})


test_that("derives the partial period from dates as expected", {
  data <- data.frame(
    id = c("Obj_1", "Obj_2", "Obj_3"),
    prev_start = c(NA, NA, "Period 1"),
    prev_end = c(NA, "Period 2", NA),
    min = c(-90, -40, 90),
    max = c(90, 140, 150)
  )
  expectation.start = c("Period 1", "Period 2", "Period 1")
  expectation.end = c("Period 2", "Period 2", "Period 3")

  expectation.source = c("Derived from absolute dating",
                         "Partially derived from absolute dating",
                         "Partially derived from absolute dating")
  derived <- derive_period(data = data, conc = test_conc,
                           min = "min", max = "max",
                           previous_start = "prev_start", previous_end = "prev_end")
  expect_equal(
    derived$period.start, expectation.start
  )
  expect_equal(
    derived$period.end, expectation.end
  )
  expect_identical(
    derived$period.source, expectation.source
  )
})

test_that("message and override for pre-existing period columns", {
  data <- data.frame(
    id = c("Obj_1", "Obj_2", "Obj_3"),
    period.start = c("Not preserved", "Not preserved", "Not preserved"),
    min = c(-90, -40, -95),
    max = c(90, 140, 150)
  )
  expectation.start = c("Period 1", "Period 2", "Period 1")
  expectation.end = c("Period 2", "Period 3", "Period 3")
  expect_message(
    derived <- derive_period(data = data, conc = test_conc,
                             min = "min", max = "max"),
    "previous_start"
  )
  expect_equal(
    derived$period.start, expectation.start
  )
  expect_equal(
    derived$period.end, expectation.end
  )
})



#### Date fits in multiple periods
test_conc_multiple <- make_chrongler_conc(
  data.frame(
    values = c("Period 1", "Period 1b", "Period 2"),
    group = c("Group A", "Group A", "Group B"),
    dating.min = c(-100, 1, 100),
    dating.max = c(99, 99, 150)
  )
)

test_that("multiple periods fitting start", {
  data <- data.frame(
    id = c("Obj_1", "Obj_2", "Obj_3", "Obj_4"),
    min = c(10, 10, 110, -500),
    max = c(101, 101, 140, 200)
  )
  expect_warning(
    tmp <- derive_period(data = data, conc = test_conc_multiple,
                         min = "min", max = "max"),
    "1, 2"
  )
  expect_all_true(grepl(";", tmp$period.start[c(1,2)]))
})

test_that("multiple periods fitting end", {
  data <- data.frame(
    id = c("Obj_1", "Obj_2", "Obj_3", "Obj_4"),
    min = c(-10, -10, 110, -500),
    max = c(20, 20, 140, 200)
  )
  expect_warning(
    tmp <- derive_period(data = data, conc = test_conc_multiple,
                         min = "min", max = "max"),
    "1, 2"
  )
  expect_all_true(grepl(";", tmp$period.end[c(1,2)]))
})
