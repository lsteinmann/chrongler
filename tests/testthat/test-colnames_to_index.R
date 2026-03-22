# colnames_to_index() from utilities.R, internal

test_that("returns integer of a single column name", {
  colnames <- c("one", "two", "three")
  expect_identical(
    colnames_to_index(colnames = colnames, columns = "three"),
    3L
  )
})

test_that("returns integer of multiple column names", {
  colnames <- c("one", "two", "three")
  expect_identical(
    colnames_to_index(colnames = colnames, columns = c("one", "three")),
    c(1L, 3L)
  )
})

test_that("returns integer of multiple column names as is, not reordered", {
  colnames <- c("one", "two", "three")
  expect_identical(
    colnames_to_index(colnames = colnames, columns = c("three", "one")),
    c(3L, 1L)
  )
})

test_that("returns integer of a single column when index is given", {
  colnames <- c("one", "two", "three")
  expect_identical(
    colnames_to_index(colnames = colnames, columns = 2),
    2L
  )
})

test_that("returns integer of multiple columns when indices are given", {
  colnames <- c("one", "two", "three")
  expect_identical(
    colnames_to_index(colnames = colnames, columns = c(1, 2)),
    c(1L, 2L)
  )
})

test_that("returns integer as is, not reordered", {
  colnames <- c("one", "two", "three")
  expect_identical(
    colnames_to_index(colnames = colnames, columns = c(2, 1)),
    c(2L, 1L)
  )
})

test_that("fails when indices are out of bounds", {
  colnames <- c("one", "two", "three")
  expect_error(
    colnames_to_index(colnames = colnames, columns = c(1, 4, 6)),
    "4"
  )
})

test_that("fails when names are not present", {
  colnames <- c("one", "two", "three")
  expect_error(
    colnames_to_index(colnames = colnames, columns = c("one", "four")),
    "four"
  )
  expect_error(
    colnames_to_index(colnames = colnames, columns = c("five", "four")),
    "five"
  )
})

test_that("fails when columns is not chr or numeric", {
  colnames <- c("one", "two", "three")
  expect_error(
    colnames_to_index(colnames = colnames, columns = list("one")),
    "list"
  )
})

