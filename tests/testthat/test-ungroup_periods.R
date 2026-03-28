## tests for ungroup_periods, from ungroup_periods.R

test_conc <- make_chrongler_conc(
  data.frame(
    values = c("Group A", "Period 1", "Period 2", "Group B", "Period 3"),
    group = c("Group A", "Group A", "Group A", "Group B", "Group B"),
    dating.min = c(-100, -100, -49, 100, 100),
    dating.max = c(99, -50, 99, 300, 300)
  )
)

test_data <- data.frame(
  id = c("Obj_1", "Obj_2", "Obj_3", "Obj_4", "Obj_5", "Obj_6"),
  period.start = c("Group A", "Period 2", "Group A", "Group A", "Group B", NA),
  period.end =   c("Period 3", "Group B", "Group A", "Group B", "Group B", "unknown")
)


#### Basics

test_that("fails if conc is not a chrongler.conc", {
  expect_error(
    ungroup_periods(data = test_data, conc = list(1, 2, 3),
                    start = "period.start", end = "period.end"),
    "chrongler.conc"
  )
})

test_that("fails if column does not exist", {
  expect_error(
    ungroup_periods(data = test_data, conc = test_conc,
                    start = "not_a_column", end = "period.end"),
    "not_a_column"
  )
  expect_error(
    ungroup_periods(data = test_data, conc = test_conc,
                    start = "period.start", end = "not_a_column"),
    "not_a_column"
  )
})

test_that("returns a data.frame", {
  result <- ungroup_periods(data = test_data, conc = test_conc,
                            start = "period.start", end = "period.end")
  expect_true(is.data.frame(result))
})

test_that("keeps other columns intact", {
  data <- test_data
  data$unrelated_column <- letters[1:6]
  result <- ungroup_periods(data = data, conc = test_conc,
                            start = "period.start", end = "period.end")
  expect_identical(result$unrelated_column, data$unrelated_column)
})

test_that("original start and end columns are preserved", {
  result <- ungroup_periods(data = test_data, conc = test_conc,
                            start = "period.start", end = "period.end")
  expect_identical(result$period.start, test_data$period.start)
  expect_identical(result$period.end, test_data$period.end)
})

test_that("NA input produces NA in grouped output", {
  result <- ungroup_periods(data = test_data, conc = test_conc,
                            start = "period.start", end = "period.end")
  expect_true(is.na(result$start.ungr[result$id == "Obj_6"]))
  expect_true(is.na(result$end.ungr[result$id == "Obj_6"]))
})

#### Output columns

test_that("appends start.ungr and end.ungr columns", {
  result <- ungroup_periods(data = test_data, conc = test_conc,
                            start = "period.start", end = "period.end")
  expect_true("start.ungr" %in% colnames(result))
  expect_true("end.ungr" %in% colnames(result))
})

test_that("start.ungr and end.ungr are ordered factors", {
  result <- ungroup_periods(data = test_data, conc = test_conc,
                            start = "period.start", end = "period.end")
  expect_true(is.ordered(result$start.ungr))
  expect_true(is.ordered(result$end.ungr))
})

test_that("ordered factor levels match conc$period.order", {
  result <- ungroup_periods(data = test_data, conc = test_conc,
                            start = "period.start", end = "period.end")
  expect_equal(levels(result$start.ungr), levels(test_conc$period.order))
  expect_equal(levels(result$end.ungr), levels(test_conc$period.order))
})


#### Grouping logic

test_that("groups are replaced with individual periods", {
  result <- ungroup_periods(data = test_data, conc = test_conc,
                            start = "period.start", end = "period.end")
  # Period 1 is the first Period of Group A
  expect_equal(as.character(result$start.ungr[result$id == "Obj_1"]), "Period 1")

  # the last period in Groub B is Period 3
  expect_equal(as.character(result$end.ungr[result$id == "Obj_2"]), "Period 3")
  # Group A from Period 1 to Period 2
  expect_equal(as.character(result$start.ungr[result$id == "Obj_3"]), "Period 1")
  expect_equal(as.character(result$end.ungr[result$id == "Obj_3"]), "Period 2")
})

test_that("values already at period level are not changed", {
  result <- ungroup_periods(data = test_data, conc = test_conc,
                            start = "period.start", end = "period.end")
  # Obj_1 in end:
  expect_equal(as.character(result$end.ungr[result$id == "Obj_1"]), "Period 3")
  # Obj_2 in start
  expect_equal(as.character(result$start.ungr[result$id == "Obj_2"]), "Period 2")
})

test_that("unknown values are passed through and produce NA in ordered factor", {
  data <- data.frame(
    id = "Obj_X",
    period.start = "Unknown Period",
    period.end = "Period 1"
  )
  result <- ungroup_periods(data = data, conc = test_conc,
                            start = "period.start", end = "period.end")
  expect_true(is.na(result$start.ungr))
  expect_equal(as.character(result$end.ungr), "Period 1")
})


#### Chronological order

test_that("period order is preserved in factor levels", {
  result <- ungroup_periods(data = test_data, conc = test_conc,
                            start = "period.start", end = "period.end")
  period_1 <- factor("Period 1", levels = levels(result$start.ungr), ordered = TRUE)
  period_3 <- factor("Period 3", levels = levels(result$start.ungr), ordered = TRUE)
  expect_true(period_1 < period_3)
})
