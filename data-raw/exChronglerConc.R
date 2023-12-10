
data <- read.csv(system.file(package = "chrongler", "extdata/2023_periods_grouping_example.csv"))


exChronglerConc <- make_chrongler_conc(data)


usethis::use_data(exChronglerConc, overwrite = TRUE)
