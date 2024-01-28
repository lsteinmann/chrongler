test_that("no failure on file name", {
  filename <- system.file(package = "chrongler",
                          "extdata/2023_periods_grouping_example.csv")
  expect_true(inherits(make_chrongler_conc(filename), "chrongler.conc"))
})

test_that("no failure on df", {
  filename <- system.file(package = "chrongler",
                          "extdata/2023_periods_grouping_example.csv")
  df <- read.csv(filename)
  expect_true(inherits(make_chrongler_conc(df), "chrongler.conc"))
})

test_that("no failure on matrix", {
  filename <- system.file(package = "chrongler",
                          "extdata/2023_periods_grouping_example.csv")
  mat <- as.matrix(read.csv(filename))
  expect_true(inherits(suppressWarnings(make_chrongler_conc(mat)),
                       "chrongler.conc"))
})

test_that("failure on something else", {
  vec <- c(1, 2, 3)
  expect_error(make_chrongler_conc(vec), "file")
  list <- list("1" = 1, "2" = "A", "3" = "bla")
  expect_error(make_chrongler_conc(list), "file")
})

test_that("failure on no file or no csv", {
  file <- "this/is/no/file.csv"
  expect_error(make_chrongler_conc(file), "file")
  file <- tempfile(fileext = ".txt")
  writeLines(c("txt"), file)
  expect_error(make_chrongler_conc(file), "file")
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



