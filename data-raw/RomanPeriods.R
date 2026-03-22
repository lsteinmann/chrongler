## code to prepare `RomanPeriods` dataset goes here

conc_df <- data.frame(
  values = c(
    "Roman Republic",
    "Early Roman Republic",
    "High Roman Republic",
    "Late Roman Republic",
    "Roman Empire",
    "Early Roman Empire",
    "High Roman Empire",
    "Late Roman Empire"
  )
)
conc_df$group <- c(rep("Roman Republic", 4), rep("Roman Empire", 4))
conc_df$dating.min <- c(-509, -509, -264, -145, -30, -30, 69, 235)
conc_df$dating.max <- c(-31, -265, -146, -31, 476, 68, 234, 476)
conc_df$source <- list(
  list(URL = "http://chronontology.dainst.org/period/RX2Rv7kcbLGk",
       comment = "dates slightly changed"),
  list(URL = "https://chronontology.dainst.org/period/Y863Mey70o4t"),
  list(URL = "https://chronontology.dainst.org/period/DFLVz4nASAC7"),
  list(URL = "https://chronontology.dainst.org/period/GzPQDbfWXlR8",
       comment = "dates slightly changed"),
  list(URL = "https://chronontology.dainst.org/period/bCTpXj4LVC2n",
       comment = "dates slightly changed"),
  list(URL = "https://chronontology.dainst.org/period/vve7mgqkCAGE"),
  list(URL = "https://chronontology.dainst.org/period/OTyMiCtkjqCz",
       comment = "dates slightly changed"),
  list(URL = "https://chronontology.dainst.org/period/7fDysWha7NKs")
)
conc_df$color <- c("#00ACC1", "#00ACC1", "#0097A7", "#00838F",
                   "#1565C0", "#1976D2", "#1565C0", "#0D47A1")


RomanPeriods <- conc_df

usethis::use_data(RomanPeriods, overwrite = TRUE)
