test_data <- read.csv(
  system.file(package = "chrongler",
              "extdata/2023_periods_grouping_example.csv"))


chrongler_period_names <- c("name", "start_date", "end_date", "group", "color", "source")
