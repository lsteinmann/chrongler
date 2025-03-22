# new_chrongler_period basics

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


test_that("error if relation between start and end date is wrong", {
  expect_error(
    new_chrongler_period(
      name = "Early Classical",
      group = "Classical",
      start_date = 480,
      end_date = -426
    ),
    "start_date"
  )
})


test_that("only single character is accepted for name and group", {
  expect_error(
    new_chrongler_period(
      name = c("Early Classical", "why"),
      group = "Classical",
      start_date = -480,
      end_date = -426
    ),
    "make_chrongler_periods"
  )

  expect_error(
    new_chrongler_period(
      name = "Early Classical",
      group = c("Classical", "idk"),
      start_date = -480,
      end_date = -426
    ),
    "make_chrongler_periods"
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


test_that("error for missing dating", {
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




# make_chrongler_periods

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


test_that("error if vectors are not same length", {
  tmp_data <- test_data[-19, ]

  expect_error(
    make_chrongler_periods(
      name = tmp_data$values[-1],
      start_date = tmp_data$dating.min,
      end_date = tmp_data$dating.max,
      group = tmp_data$group,
      color = tmp_data$color,
      source = rep("none", nrow(tmp_data))
    ),
    "length"
  )

  expect_error(
    make_chrongler_periods(
      name = tmp_data$values,
      start_date = tmp_data$dating.min[-1],
      end_date = tmp_data$dating.max,
      group = tmp_data$group,
      color = tmp_data$color,
      source = rep("none", nrow(tmp_data))
    ),
    "length"
  )

  expect_error(
    make_chrongler_periods(
      name = tmp_data$values,
      start_date = tmp_data$dating.min,
      end_date = tmp_data$dating.max[-1],
      group = tmp_data$group,
      color = tmp_data$color,
      source = rep("none", nrow(tmp_data))
    ),
    "length"
  )

  expect_error(
    make_chrongler_periods(
      name = tmp_data$values,
      start_date = tmp_data$dating.min,
      end_date = tmp_data$dating.max,
      group = tmp_data$group[-1],
      color = tmp_data$color,
      source = rep("none", nrow(tmp_data))
    ),
    "length"
  )

  expect_error(
    make_chrongler_periods(
      name = tmp_data$values,
      start_date = tmp_data$dating.min,
      end_date = tmp_data$dating.max,
      group = tmp_data$group,
      color = tmp_data$color[-1],
      source = rep("none", nrow(tmp_data))
    ),
    "length"
  )

  expect_error(
    make_chrongler_periods(
      name = tmp_data$values[-1],
      start_date = tmp_data$dating.min[-c(1, 2, 3, 4, 5)],
      end_date = tmp_data$dating.max[-c(5, 6)],
      group = tmp_data$group[-c(3, 4, 5)],
      color = tmp_data$color[-5],
      source = rep("none", nrow(tmp_data))
    ),
    "length"
  )
})


test_that("multiple periods are processed with warning", {
  tmp_data <- test_data[-19, ]
  tmp_data <- tmp_data[-which(tmp_data$period == ""), ]
  res <- make_chrongler_periods(
    name = tmp_data$period,
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




# chrongler_period Methods

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
    new_chrongler_period("One", 100, 200),
    new_chrongler_period("Two", 200, 300)
  )
  res <- as.matrix(tmp)
  expect_type(res, "character")
  expect_true(is.matrix(res))
  expect_equal(nrow(res), length(tmp))
  expect_equal(ncol(res), length(chrongler_period_names))
})


test_that("two periods can be compared", {
  earlier <- new_chrongler_period("earlier", -100, -50)
  later <- new_chrongler_period("later", 100, 200)
  later_sec <- new_chrongler_period("later two", 100, 200)

  expect_true(later > earlier)
  expect_true(earlier < later)
  expect_true(later != earlier)
  expect_true(later == later_sec)
  expect_false(later < earlier)
  expect_false(later == earlier)
})

test_that("print methods don't throw errors", {
  capt <- capture.output(print(new_chrongler_period("Two", 200, 300)))
  expect_true(any(grepl("chrongler_period", capt)))

  capt <- capture.output(print(
    new_chrongler_period("Two", 200, 300, source = "here")
    ))
  expect_true(any(grepl("Source", capt)))

  capt <- capture.output(print(
    new_chrongler_period("Two", 200, 300, color = "here")
  ))
  expect_true(any(grepl("Color", capt)))
})
