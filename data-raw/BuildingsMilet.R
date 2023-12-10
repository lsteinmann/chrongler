## code to prepare `DATASET` dataset goes here
BuildingsMilet <- read.csv(system.file("extdata/2023_chrongler_exdata.csv",
                                     package = "chrongler"),
                         fileEncoding = "UTF-8")

BuildingsMilet$X <- NULL

usethis::use_data(BuildingsMilet, overwrite = TRUE)
