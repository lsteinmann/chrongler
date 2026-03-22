## tests for derive_dating, from derive_dating.R

test_that("works with example data: BuildingsMilet", {
  data("PeriodsMilet")
  data("BuildingsMilet")
  conc <- make_chrongler_conc(PeriodsMilet)
  result <- derive_dating(
    data = BuildingsMilet,
    conc = conc,
    start = "period.start",
    end = "period.end")
  expect_all_true(
    c("dating.min", "dating.max", "dating.source") %in% colnames(result)
  )
})


#### Basics
test_that("fails if conc is not a chrongler.conc", {
  expect_error(
    derive_dating(data = BuildingsMilet, conc = list(1, 2, 3)),
    "chrongler.conc"
  )
})

data("RomanPeriods")
roman_conc <- make_chrongler_conc(RomanPeriods)

test_that("fails if column does not exist", {
  data <- data.frame(
    id = c("Obj_1", "Obj_2"),
    start = c("Early Roman Republic", "Early Roman Empire"),
    end = c("Late Roman Republic", "Late Roman Empire")
  )
  expect_error(
    derive_dating(data = data, conc = roman_conc,
                  start = "not_a_column", end = "end"),
    "not_a_column"
  )
  expect_error(
    derive_dating(data = data, conc = roman_conc,
                  start = "start", end = "not_a_column"),
    "not_a_column"
  )
})

test_that("keeps other columns intact", {
  data <- data.frame(
    id = c("Obj_1", "Obj_2"),
    start = c("Early Roman Republic", "Early Roman Empire"),
    end = c("Late Roman Republic", "Late Roman Empire"),
    unrelated_column = c("Information", "Another information"),
    different_column = c("Findspot 1", "Findspot 2")
  )
  derived <- derive_dating(data = data, conc = roman_conc,
                           start = "start", end = "end")
  expect_identical(
    derived$unrelated_column, data$unrelated_column
  )
  expect_identical(
    derived$different_column, data$different_column
  )
})


## Periods

test_that("finds missing periods", {
  data <- data.frame(
    id = c("Obj_1", "Obj_2"),
    start = c("Unknown Period", "Early Roman Empire"),
    end = c("Late Roman Republic", "Late Roman Empire")
  )
  expect_warning(
    tmp <- derive_dating(data = data, conc = roman_conc,
                         start = "start", end = "end"),
    "Unknown Period"
  )
  expect_all_true(is.data.frame(tmp))
  expect_true(is.na(tmp[1,"dating.min"]))
  expect_true(is.na(tmp[1,"dating.max"]))
  # Something I need to fix!
  # expect_true(is.na(tmp[1,"source"]))
})

test_conc <- make_chrongler_conc(
  data.frame(
    values = c("Period 1", "Period 2"),
    group = c("Group A", "Group A"),
    dating.min = c(-100, 100),
    dating.max = c(-50, 150)
  )
)

test_that("derives the dating from periods as expected", {
  data <- data.frame(
    id = c("Obj_1", "Obj_2"),
    start = c("Period 1", "Period 2"),
    end = c("Period 2", "Period 2")
  )
  expectation.min = c(-100, 100)
  expectation.max = c(150, 150)
  derived <- derive_dating(data = data, conc = test_conc,
                           start = "start", end = "end")
  expect_equal(
    derived$dating.min, expectation.min
  )
  expect_equal(
    derived$dating.max, expectation.max
  )
})

