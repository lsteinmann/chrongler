# Tests for make_chrongler_conc(), from: make_chrongler_conc.R

test_that("accepts only data.frame if not filepath", {
  data("RomanPeriods")
  expect_true(inherits(make_chrongler_conc(RomanPeriods), "chrongler.conc"))
  tmp <- as.matrix(RomanPeriods)
  expect_error(make_chrongler_conc(tmp), "data.frame")
  tmp <- as.list(RomanPeriods)
  expect_error(make_chrongler_conc(tmp), "data.frame")
  tmp <- c(1, 2, 3)
  expect_error(make_chrongler_conc(tmp), "data.frame")
  list <- list("1" = 1, "2" = "A", "3" = "bla")
  expect_error(make_chrongler_conc(tmp), "data.frame")
  tmp <- c("one", "two", "three")
  expect_error(make_chrongler_conc(tmp), "data.frame")
})

test_that("lets read.csv() fail if it wants to", {
  filename <- "./this/does/not/exist.csv"
  expect_error(suppressWarnings(make_chrongler_conc(filename)))
})

test_that("passes additional arguments to read.csv()", {
  filename <- "./this/does/not/exist.csv"
  response <- try(suppressWarnings(
    make_chrongler_conc(filename, fileEncoding = "test")),
    silent = TRUE)
  expect_true(grepl("fileEncoding", response))
})


test_that("error when columns dont exist", {
  test <- data.frame("grs" = c("A", "A", "B"),
                     "prs" = c("A", "donc", "bla"),
                     "min" = c(1, 2, 3),
                     "max" = c(2, 3, 4),
                     "colr" = c("green", "red", "yellow"))
  expect_error(make_chrongler_conc(test), "not found")
})


test_that("substitutes names", {
  test <- data.frame("grs" = c("A", "A", "A", "B"),
                     "prs" = c("A", "donc", "esel", "bla"),
                     "min" = c(1, 2, 3, 4),
                     "max" = c(2, 3, 4, 5),
                     "colr" = c("green", "red", "yellow", "brown"))
  colnames <- list(groups = "grs", values = "prs",
                   dating.min = "min", dating.max = "max",
                   color = "colr")
  expect_true(inherits(make_chrongler_conc(test, cols = colnames), "chrongler.conc"))
})

test_that("works with indices", {
  test <- data.frame("grs" = c("A", "A", "A", "B"),
                     "prs" = c("A", "donc", "esel", "bla"),
                     "min" = c(1, 2, 3, 4),
                     "max" = c(2, 3, 4, 5),
                     "colr" = c("green", "red", "yellow", "brown"))
  colnames <- list(groups = 1, values = 2,
                   dating.min = 3, dating.max = 4,
                   color = 5)
  expect_true(inherits(make_chrongler_conc(test, cols = colnames), "chrongler.conc"))
})

test_that("values column missing", {
  test <- data.frame("groups" = c("A", "A", "A", "B"),
                     "dating.min" = c(1, 2, 3, 4),
                     "dating.max" = c(2, 3, 4, 5))
  expect_error(make_chrongler_conc(test), "values")
})


test_that("group column missing", {
  test <- data.frame("values" = c("A", "donc", "esel", "bla"),
                     "dating.min" = c(1, 2, 3, 4),
                     "dating.max" = c(2, 3, 4, 5))
  expect_error(make_chrongler_conc(test), "group")
})

test_that("dating not numeric", {
  test <- data.frame("groups" = c("A", "A", "A", "B"),
                     "values" = c("A", "donc", "esel", "bla"),
                     "dating.min" = c("2", "2", "hello", "5"),
                     "dating.max" = c("2", "3", "4", "5"),
                     "color" = c("green", "red", "yellow", "brown"))
  warnings <- capture_warnings(make_chrongler_conc(test))
  expect_true(any(grepl("numeric", warnings)))
  expect_true(any(grepl("coercion", warnings)))
})



