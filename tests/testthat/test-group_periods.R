## tests for group_periods, from group_periods.R

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
  period.start = c("Period 1", "Period 2", "Group A", "Period 3", "Group B", NA),
  period.end =   c("Period 2", "Period 2", "Group A", "Period 3", "Group B", "unknown")
)

start <- "period.start"
end <- "period.end"
data <- test_data
conc <- test_conc

#### Basics

test_that("fails if conc is not a chrongler.conc", {
  expect_error(
    group_periods(data = test_data, conc = list(1, 2, 3),
                  start = "period.start", end = "period.end"),
    "chrongler.conc"
  )
})

test_that("fails if column does not exist", {
  expect_error(
    group_periods(data = test_data, conc = test_conc,
                  start = "not_a_column", end = "period.end"),
    "not_a_column"
  )
  expect_error(
    group_periods(data = test_data, conc = test_conc,
                  start = "period.start", end = "not_a_column"),
    "not_a_column"
  )
})

test_that("returns a data.frame", {
  result <- group_periods(data = test_data, conc = test_conc,
                          start = "period.start", end = "period.end")
  expect_true(is.data.frame(result))
})

test_that("keeps other columns intact", {
  data <- test_data
  data$unrelated_column <- letters[1:6]
  result <- group_periods(data = data, conc = test_conc,
                          start = "period.start", end = "period.end")
  expect_identical(result$unrelated_column, data$unrelated_column)
})

test_that("original start and end columns are preserved", {
  result <- group_periods(data = test_data, conc = test_conc,
                          start = "period.start", end = "period.end")
  expect_identical(result$period.start, test_data$period.start)
  expect_identical(result$period.end, test_data$period.end)
})

test_that("NA input produces NA in grouped output", {
  result <- group_periods(data = test_data, conc = test_conc,
                          start = "period.start", end = "period.end")
  expect_true(is.na(result$start.grpd[result$id == "Obj_6"]))
  expect_true(is.na(result$end.grpd[result$id == "Obj_6"]))
})

#### Output columns

test_that("appends start.grpd and end.grpd columns", {
  result <- group_periods(data = test_data, conc = test_conc,
                          start = "period.start", end = "period.end")
  expect_true("start.grpd" %in% colnames(result))
  expect_true("end.grpd" %in% colnames(result))
})

test_that("start.grpd and end.grpd are ordered factors", {
  result <- group_periods(data = test_data, conc = test_conc,
                          start = "period.start", end = "period.end")
  expect_true(is.ordered(result$start.grpd))
  expect_true(is.ordered(result$end.grpd))
})

test_that("ordered factor levels match conc$group.order", {
  result <- group_periods(data = test_data, conc = test_conc,
                          start = "period.start", end = "period.end")
  expect_equal(levels(result$start.grpd), levels(test_conc$group.order))
  expect_equal(levels(result$end.grpd), levels(test_conc$group.order))
})


#### Grouping logic

test_that("periods are replaced with their group", {
  result <- group_periods(data = test_data, conc = test_conc,
                          start = "period.start", end = "period.end")
  # Period 1 and Period 2 both belong to Group A
  expect_equal(as.character(result$start.grpd[result$id == "Obj_1"]), "Group A")
  expect_equal(as.character(result$start.grpd[result$id == "Obj_2"]), "Group A")
  # Period 3 belongs to Group B
  expect_equal(as.character(result$start.grpd[result$id == "Obj_4"]), "Group B")
})

test_that("values already at group level are not changed", {
  result <- group_periods(data = test_data, conc = test_conc,
                          start = "period.start", end = "period.end")
  # Obj_3 already has Group A, Obj_5 already has Group B
  expect_equal(as.character(result$start.grpd[result$id == "Obj_3"]), "Group A")
  expect_equal(as.character(result$start.grpd[result$id == "Obj_5"]), "Group B")
})

test_that("unknown values are passed through and produce NA in ordered factor", {
  data <- data.frame(
    id = "Obj_X",
    period.start = "Unknown Period",
    period.end = "Period 1"
  )
  result <- group_periods(data = data, conc = test_conc,
                          start = "period.start", end = "period.end")
  expect_true(is.na(result$start.grpd))
  expect_equal(as.character(result$end.grpd), "Group A")
})


#### Chronological order

test_that("group order is preserved in factor levels", {
  result <- group_periods(data = test_data, conc = test_conc,
                          start = "period.start", end = "period.end")
  group_a <- factor("Group A", levels = levels(result$start.grpd), ordered = TRUE)
  group_b <- factor("Group B", levels = levels(result$start.grpd), ordered = TRUE)
  expect_true(group_a < group_b)
})
