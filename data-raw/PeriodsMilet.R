## code to prepare `DATASET` dataset goes here
PeriodsMilet <- read.csv(
  system.file("extdata/2023_periods_grouping_example.csv",
              package = "chrongler"),
  fileEncoding = "UTF-8")

PeriodsMilet$X <- NULL
PeriodsMilet$period <- NULL


usethis::use_data(PeriodsMilet, overwrite = TRUE)
