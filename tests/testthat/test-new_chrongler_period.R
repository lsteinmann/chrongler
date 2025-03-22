

test_that("produces correct S3 class", {
  per <- new_chrongler_period(
    name = "Early Classical",
    group = "Classical",
    start_date = -480,
    end_date = -426,
    color = "#441794",
    source = "Test"
  )
  expect_s3_class(per, "chrongler_period")
})

test_that("source and color are optional", {
  expect_s3_class(
    new_chrongler_period(
      name = "Early Classical",
      group = "Classical",
      start_date = -480,
      end_date = -426
    ),
    "chrongler_period"
  )
})


test_that("dating has to be numerical", {
  expect_error(
    new_chrongler_period(
      name = "Early Classical",
      group = "Classical",
      start_date = "nein",
      end_date = "ist es nicht"
    )
  )
  expect_error(
    new_chrongler_period(
      name = "Early Classical",
      group = "Classical",
      start_date = "nein",
      end_date = -426
    )
  )
  expect_error(
    new_chrongler_period(
      name = "Early Classical",
      group = "Classical",
      start_date = -480,
      end_date = "ist es nicht"
    )
  )
})

test_that("warns for missing dating", {
  expect_error(
    new_chrongler_period(
      name = "Early Classical",
      group = "Classical",
      start_date = NA,
      end_date = NA
    )
  )
  expect_error(
    new_chrongler_period(
      name = "Early Classical",
      group = "Classical",
      start_date = NA,
      end_date = -426
    )
  )
  expect_error(
    new_chrongler_period(
      name = "Early Classical",
      group = "Classical",
      start_date = -480,
      end_date = NA
    )
  )
})



test_that("multiple periods are processed with warning if values are missing", {
  expect_warning(
    check <- make_chrongler_periods(
      name = test_data$values,
      start_date = test_data$dating.min,
      end_date = test_data$dating.max
    ),
    "undetermined"
  )
  expect_identical(check[["undetermined"]], NA)
})


test_that("multiple periods are processed with warning", {
  tmp_data <- test_data[-19, ]
  res <- make_chrongler_periods(
    name = tmp_data$values,
    start_date = tmp_data$dating.min,
    end_date = tmp_data$dating.max
  )
  expect_true(is.list(res))
  expect_s3_class(
    res[[1]],
    "chrongler_period"
  )
  expect_equal(length(res), nrow(tmp_data))
})


test_that("a chrongler_period can be coerced to vector", {
  tmp <- new_chrongler_period("period", 100, 200, "group", "#ffffff", "source")
  res <- as.vector(tmp)
  expect_type(res, "character")
  expect_identical(names(res), names(tmp))

  res <- as.vector(new_chrongler_period("period", 100, 200, "group"))
  exp <- c("period", "100", "200", "group", NA, NA)
  names(exp) <- chrongler_period_names
  expect_type(res, "character")
  expect_identical(res, exp)
})

test_that("a list of chrongler_periods can be coerced to matrix", {
  tmp <- list(
    new_chrongler_period("Classical", 100, 200),
    new_chrongler_period("Hellenistic", 200, 300)
  )
  res <- as.matrix(tmp)
  expect_type(res, "character")
  expect_true(is.matrix(res))
  expect_equal(nrow(res), length(tmp))
  expect_equal(ncol(res), length(chrongler_period_names))
})
