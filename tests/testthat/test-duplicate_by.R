## tests for duplicate_by, from duplicate_by.R

test_conc <- make_chrongler_conc(
  data.frame(
    values = c("Period 1", "Period 2", "Period 3", "Period 4"),
    group = c("Group A", "Group A", "Group B", "Group B"),
    dating.min = c(-100, -49, 100, 200),
    dating.max = c(-50, 99, 199, 300)
  )
)

test_data <- data.frame(
  id = c("Obj_1", "Obj_2", "Obj_3"),
  period.start = c("Period 1", "Period 2", "Period 3"),
  period.end = c("Period 2", "Period 4", "Period 3")
)


#### Basics

test_that("fails if conc is not a chrongler.conc", {
  expect_error(
    duplicate_by(data = test_data, conc = list(1, 2, 3),
                 start = "period.start", end = "period.end",
                 by_group = FALSE),
    "chrongler.conc"
  )
})

test_that("fails if column does not exist", {
  expect_error(
    duplicate_by(data = test_data, conc = test_conc,
                 start = "not_a_column", end = "period.end",
                 by_group = FALSE),
    "not_a_column"
  )
  expect_error(
    duplicate_by(data = test_data, conc = test_conc,
                 start = "period.start", end = "not_a_column",
                 by_group = FALSE),
    "not_a_column"
  )
})

test_that("returns a data.frame", {
  result <- duplicate_by(data = test_data, conc = test_conc,
                         start = "period.start", end = "period.end",
                         by_group = FALSE)
  expect_true(is.data.frame(result))
})

test_that("keeps other columns intact", {
  data <- test_data
  data$unrelated_column <- c("info_1", "info_2", "info_3")
  result <- duplicate_by(data = data, conc = test_conc,
                         start = "period.start", end = "period.end",
                         by_group = FALSE)
  expect_true("unrelated_column" %in% colnames(result))
  # Obj_1 spans 2 periods, so its unrelated_column value should appear twice
  expect_equal(sum(result$unrelated_column == "info_1"), 2)
})


#### Row counts

test_that("produces correct number of rows by period", {
  # Obj_1: Period 1 -> Period 2 = 2 rows
  # Obj_2: Period 2 -> Period 4 = 3 rows
  # Obj_3: Period 3 -> Period 3 = 1 row
  result <- duplicate_by(data = test_data, conc = test_conc,
                         start = "period.start", end = "period.end",
                         by_group = FALSE)
  expect_equal(nrow(result), 6)
  expect_equal(sum(result$id == "Obj_1"), 2)
  expect_equal(sum(result$id == "Obj_2"), 3)
  expect_equal(sum(result$id == "Obj_3"), 1)
})

test_that("produces correct number of rows by group", {
  # Obj_1: Group A -> Group A = 1 row
  # Obj_2: Group A -> Group B = 2 rows
  # Obj_3: Group B -> Group B = 1 row
  result <- duplicate_by(data = test_data, conc = test_conc,
                         start = "period.start", end = "period.end",
                         by_group = TRUE)
  expect_equal(nrow(result), 4)
  expect_equal(sum(result$id == "Obj_1"), 1)
  expect_equal(sum(result$id == "Obj_2"), 2)
  expect_equal(sum(result$id == "Obj_3"), 1)
})


#### Period column

test_that("period column contains correct values by period", {
  result <- duplicate_by(data = test_data, conc = test_conc,
                         start = "period.start", end = "period.end",
                         by_group = FALSE)
  expect_true("period" %in% colnames(result))
  obj1_periods <- result$period[result$id == "Obj_1"]
  expect_equal(as.character(obj1_periods), c("Period 1", "Period 2"))
  obj2_periods <- result$period[result$id == "Obj_2"]
  expect_equal(as.character(obj2_periods), c("Period 2", "Period 3", "Period 4"))
})

test_that("period column contains correct values by group", {
  result <- duplicate_by(data = test_data, conc = test_conc,
                         start = "period.start", end = "period.end",
                         by_group = TRUE)
  obj2_periods <- result$period[result$id == "Obj_2"]
  expect_equal(as.character(obj2_periods), c("Group A", "Group B"))
})

test_that("period column is the correct ordered factor", {
  result <- duplicate_by(data = test_data, conc = test_conc,
                         start = "period.start", end = "period.end",
                         by_group = FALSE)
  expect_equal(
    levels(result$period),
    levels(test_conc$period.order)
  )
  expect_true(is.ordered(result$period))
  result <- duplicate_by(data = test_data, conc = test_conc,
                         start = "period.start", end = "period.end",
                         by_group = TRUE)
  expect_equal(
    levels(result$period),
    levels(test_conc$group.order)
  )
  expect_true(is.ordered(result$period))
})


#### Fraction column

test_that("fraction column is present", {
  result <- duplicate_by(data = test_data, conc = test_conc,
                         start = "period.start", end = "period.end",
                         by_group = FALSE)
  expect_true("fraction" %in% colnames(result))
  result <- duplicate_by(data = test_data, conc = test_conc,
                         start = "period.start", end = "period.end",
                         by_group = TRUE)
  expect_true("fraction" %in% colnames(result))
})

test_that("fractions sum to 1 per original row", {
  result <- duplicate_by(data = test_data, conc = test_conc,
                         start = "period.start", end = "period.end",
                         by_group = FALSE)
  obj1_sum <- sum(result$fraction[result$id == "Obj_1"])
  obj2_sum <- sum(result$fraction[result$id == "Obj_2"])
  obj3_sum <- sum(result$fraction[result$id == "Obj_3"])
  expect_equal(obj1_sum, 1)
  expect_equal(obj2_sum, 1)
  expect_equal(obj3_sum, 1)
})

test_that("fraction values are correct", {
  result <- duplicate_by(data = test_data, conc = test_conc,
                         start = "period.start", end = "period.end",
                         by_group = FALSE)
  # Obj_1 spans 2 periods: each gets 1/2
  expect_equal(unique(result$fraction[result$id == "Obj_1"]), 1/2)
  # Obj_2 spans 3 periods: each gets 1/3
  expect_equal(unique(result$fraction[result$id == "Obj_2"]), 1/3)
  # Obj_3 spans 1 period: gets 1
  expect_equal(unique(result$fraction[result$id == "Obj_3"]), 1)
})


#### NA handling
test_that("NA start or end produces single row with NA period and NA fraction", {
  data <- data.frame(
    id = c("Obj_1", "Obj_2"),
    period.start = c(NA, "Period 1"),
    period.end = c("Period 2", NA)
  )
  result <- duplicate_by(data = data, conc = test_conc,
                         start = "period.start", end = "period.end",
                         by_group = FALSE)
  expect_equal(nrow(result), 2)
  expect_true(is.na(result$period[result$id == "Obj_1"]))
  expect_true(is.na(result$period[result$id == "Obj_2"]))
  expect_true(is.na(result$fraction[result$id == "Obj_1"]))
  expect_true(is.na(result$fraction[result$id == "Obj_2"]))
})


#### Row identity

test_that("original row data is preserved in duplicated rows", {
  result <- duplicate_by(data = test_data, conc = test_conc,
                         start = "period.start", end = "period.end",
                         by_group = FALSE)
  obj2_rows <- result[result$id == "Obj_2", ]
  # All three duplicated rows should have the same id
  expect_true(all(obj2_rows$id == "Obj_2"))
})
