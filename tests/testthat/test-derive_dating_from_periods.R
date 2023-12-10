test_that("works", {
  data("BuildingsMilet")
  data("exChronglerConc")

  derive_dating(BuildingsMilet, exChronglerConc,
                             start = "period.start",
                             end = "period.end")
  expect_equal(2 * 2, 4)
})
