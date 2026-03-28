test_that("NA for NULL", {
  expect_identical(na_if_empty(NULL), NA)
})

test_that("NA for empty list", {
  expect_identical(na_if_empty(list()), NA)
})

test_that("NA for empty vectors", {
  expect_identical(na_if_empty(character(0)), NA)
  expect_identical(na_if_empty(numeric(0)), NA)
  expect_identical(na_if_empty(integer(0)), NA)
})

test_that("NA for emptry string", {
  expect_identical(na_if_empty(""), NA)
})

test_that("returns non-empty values untouched", {
  expect_identical(na_if_empty(
    list(a = "a")),
    list(a = "a")
  )
  expect_identical(na_if_empty(
    list(a = "a", b = "b")),
    list(a = "a", b = "b")
  )
  expect_identical(na_if_empty(
    "Character"),
    "Character"
  )
  expect_identical(na_if_empty(
    data.frame(a = c(1, 2, 3))),
    data.frame(a = c(1, 2, 3))
  )
  expect_identical(na_if_empty(
    data.frame(a = c("1", "2", "3"))),
    data.frame(a = c("1", "2", "3"))
  )
  expect_identical(na_if_empty(
    matrix(c(1,2,3), c(1,2,3))),
    matrix(c(1,2,3), c(1,2,3))
  )
  expect_identical(na_if_empty(
    10),
    10
  )
})
