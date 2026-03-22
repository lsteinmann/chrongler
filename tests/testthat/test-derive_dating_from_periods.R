## tests for derive_dating, from derive_dating.R

test_that("works with example data: BuildingsMilet", {
  data("PeriodsMilet")
  data("BuildingsMilet")
  conc <- make_chrongler_conc(PeriodsMilet)
  result <- derive_dating(
    data = BuildingsMilet,
    conc = conc,
    start = "period.start",
    end = "period.end")
  expect_all_true(
    c("dating.min", "dating.max", "dating.source") %in% colnames(result)
  )
})

