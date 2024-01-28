exChronglerConc <- read.csv(
  system.file(package = "chrongler",
              "extdata/2023_periods_grouping_example.csv")
)
exChronglerConc <- make_chrongler_conc(exChronglerConc)

test_that("works", {
  data("BuildingsMilet")
  derive_dating(BuildingsMilet, exChronglerConc,
                             start = "period.start",
                             end = "period.end")
  expect_equal(2 * 2, 4)
})
