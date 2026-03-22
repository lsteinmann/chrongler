# Tests for make_chrongler_conc(), from: make_chrongler_conc.R

### Basic checks and operations for params

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

### Checks for the `cols` param

test_that("uses column names as fallback, warns for malformed cols-list", {
  df <- RomanPeriods
  cols <- list(no_group = "group", no_values = "values",
               no_dating.min = "dating.min", no_dating.max = "dating.max",
               no_color = "color", no_source = "source")
  expect_warning(make_chrongler_conc(file = df, cols = cols),
                 "Unknown names")
})

test_that("errors for malformed cols-list when true columns are absent", {
  df <- RomanPeriods
  cols <- list(no_group = "group", no_values = "values",
               no_dating.min = "dating.min", no_dating.max = "dating.max",
               no_color = "color", no_source = "source")
  colnames(df) <- c("values", "group", "invalid_dating",
                    "dating.max", "source", "color")
  expect_error(
    suppressWarnings(make_chrongler_conc(file = df, cols = cols)),
    "dating.min"
  )
})

test_that("fails for unexpected format in cols-list", {
  df <- RomanPeriods
  cols <- list(group = "group", values = list("values", "more values"),
               dating.min = "dating.min", dating.max = "dating.max",
               color = "color", source = "source")
  # It fails - which is good, but I am not adding a specific error for this.
  expect_error(make_chrongler_conc(file = df, cols = cols))
})


### Verify the structure of the output

# Test setup - reusable across all structural tests
roman_df <- data.frame(
  group = c(rep("Roman Republic", 4), rep("Roman Empire", 4)),
  values = c("Roman Republic", "Early Roman Republic", "High Roman Republic",
             "Late Roman Republic", "Roman Empire", "Early Roman Empire",
             "High Roman Empire", "Late Roman Empire"),
  dating.min = c(-509, -509, -264, -145, -30, -30, 69, 235),
  dating.max = c(-31, -265, -146, -31, 476, 68, 234, 476),
  color = c("#00ACC1", "#00ACC1", "#0097A7", "#00838F",
            "#1565C0", "#1976D2", "#1565C0", "#0D47A1")
)

test_that("returns a chrongler.conc object", {
  expect_true(inherits(make_chrongler_conc(roman_df), "chrongler.conc"))
  expect_true(is.list(make_chrongler_conc(roman_df)))
})

test_that("return value has all expected top-level elements", {
  expect_named(make_chrongler_conc(roman_df),
               c("all", "grouped", "group.order",
                 "period.order", "dating", "color", "source"))
})

test_that("$all has correct structure", {
  roman_conc <- make_chrongler_conc(roman_df)
  expect_true(is.list(roman_conc$all))
  expect_named(roman_conc$all, roman_df$values)
  for (entry in roman_conc$all) {
    expect_named(entry, c("name", "group", "dating", "color", "source"))
    expect_true(is.character(entry$name))
    expect_true(is.character(entry$group))
    expect_named(entry$dating, c("from", "to"))
    expect_true(is.numeric(entry$dating$from))
    expect_true(is.numeric(entry$dating$to))
  }
})

test_that("$grouped has correct structure", {
  roman_conc <- make_chrongler_conc(roman_df)
  expect_true(is.list(roman_conc$grouped))
  expect_named(roman_conc$grouped, c("Roman Republic", "Roman Empire"))
  for (entry in roman_conc$grouped) {
    expect_true(is.ordered(entry))
  }
  # Groups should not contain themselves as a period
  expect_false("Roman Republic" %in% as.character(roman_conc$grouped$`Roman Republic`))
  expect_false("Roman Empire" %in% as.character(roman_conc$grouped$`Roman Empire`))
})

test_that("$group.order has correct structure", {
  roman_conc <- make_chrongler_conc(roman_df)
  expect_true(is.ordered(roman_conc$group.order))
  expect_equal(levels(roman_conc$group.order), c("Roman Republic", "Roman Empire"))
})

test_that("$period.order has correct structure", {
  roman_conc <- make_chrongler_conc(roman_df)
  expect_true(is.ordered(roman_conc$period.order))
  expected_periods <- c("Early Roman Republic", "High Roman Republic",
                        "Late Roman Republic", "Early Roman Empire",
                        "High Roman Empire", "Late Roman Empire")
  expect_equal(levels(roman_conc$period.order), expected_periods)
  # Groups should not appear in period.order
  expect_false("Roman Republic" %in% levels(roman_conc$period.order))
  expect_false("Roman Empire" %in% levels(roman_conc$period.order))
})

test_that("$dating has correct structure", {
  roman_conc <- make_chrongler_conc(roman_df)
  expect_true(is.list(roman_conc$dating))
  expect_named(roman_conc$dating, roman_df$values)
  for (entry in roman_conc$dating) {
    expect_named(entry, c("from", "to"))
    expect_true(is.numeric(entry$from))
    expect_true(is.numeric(entry$to))
  }
})

test_that("$color has correct structure", {
  roman_conc <- make_chrongler_conc(roman_df)
  expect_true(is.character(roman_conc$color))
  expect_named(roman_conc$color, roman_df$values)
})

test_that("single $color is present in $all", {
  color <- roman_df[1,"color"]
  roman_conc <- make_chrongler_conc(roman_df)
  expect_identical(
    roman_conc$all$`Roman Republic`$color,
    color
  )
})

test_that("$source is present", {
  roman_conc <- make_chrongler_conc(roman_df)
  # source not supplied - should exist but be empty/NULL per period
  expect_true("source" %in% names(roman_conc))
})

test_that("single $source is present in $all", {
  roman_df$source <- rep("A source", nrow(roman_df))
  roman_conc <- make_chrongler_conc(roman_df)
  expect_identical(
    roman_conc$all$`Roman Republic`$source,
    "A source"
  )
})

test_that("chronological order is preserved", {
  roman_conc <- make_chrongler_conc(roman_df)
  expect_true(roman_conc$period.order[1] < roman_conc$period.order[3])
  expect_false(roman_conc$period.order[5] < roman_conc$period.order[3])
  expect_true(roman_conc$period.order[5] > roman_conc$period.order[1])
  expect_true(roman_conc$group.order[1] < roman_conc$group.order[2])

  first <- factor("Early Roman Republic",
                  levels = roman_conc$period.order,
                  ordered = TRUE)
  second <- factor("Late Roman Republic",
                   levels = roman_conc$period.order,
                   ordered = TRUE)
  expect_true(first < second)
})


## Dating - numeric
test_that("warn and coerce if dating columns are not numeric", {
  test <- data.frame("group" = c("Group A", "Group A", "Group A", "Group B"),
                     "values" = c("Period 1", "Period 2", "Period 3", "Period 4"),
                     "dating.min" = c("2", "2", "hello", "5"),
                     "dating.max" = c(3, 4, 4, 6))
  expect_warning(
    expect_warning(make_chrongler_conc(test), "dating.min"),
    "NAs"
  )
  test <- data.frame("group" = c("Group A", "Group A", "Group A", "Group B"),
                     "values" = c("Period 1", "Period 2", "Period 3", "Period 4"),
                     "dating.min" = c(0, 1, 2, 3),
                     "dating.max" = c("1", "three", "4", "5"))
  expect_warning(
    expect_warning(make_chrongler_conc(test), "dating.max"),
    "NAs"
  )
})

test_that("add old column name to warning", {
  test <- data.frame("group" = c("Group A", "Group A", "Group A", "Group B"),
                     "values" = c("Period 1", "Period 2", "Period 3", "Period 4"),
                     "minimum" = c("2", "2", "hello", "5"),
                     "maximum" = c(3, 4, 5, 6))
  expect_warning(
    expect_warning(
      make_chrongler_conc(test,
                          cols = list(dating.min = "minimum",
                                      dating.max = "maximum")),
      "minimum"),
    "NAs"
  )
})


test_that("warns if max is higher than or equal to min", {
  test <- data.frame("group" = c("Group A", "Group A", "Group A", "Group B"),
                     "values" = c("Period 1", "Period 2", "Period 3", "Period 4"),
                     "dating.min" = c(1, 3, 4, 7),
                     "dating.max" = c(2, 3, 5, 6))
  expect_warning(make_chrongler_conc(test), "'Period 2', 'Period 4'")
})



### Other things.

test_that("fails if a mandatory column is missing", {
  test <- data.frame("values" = c("Period 1", "Period 2", "Period 3", "Period 4"),
                     "dating.min" = c(1, 3, 4, 7),
                     "dating.max" = c(2, 3, 5, 6))
  expect_error(make_chrongler_conc(test), "not present")
})


test_that("works with example data: RomanPeriods", {
  data("RomanPeriods")
  expect_true(
    inherits(make_chrongler_conc(RomanPeriods), "chrongler.conc")
  )
})

test_that("works with example data: PeriodsMilet", {
  data("PeriodsMilet")
  expect_true(
    inherits(make_chrongler_conc(PeriodsMilet), "chrongler.conc")
  )
})

