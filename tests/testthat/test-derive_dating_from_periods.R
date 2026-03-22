## tests for derive_dating, from derive_dating.R

test_that("works with example data: BuildingsMilet", {
  data("PeriodsMilet")
  data("BuildingsMilet")
  conc <- make_chrongler_conc(PeriodsMilet)
  expect_warning(
    result <- derive_dating(
      data = BuildingsMilet,
      conc = conc,
      start = "period.start",
      end = "period.end")
  )
})

