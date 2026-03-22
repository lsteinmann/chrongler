#' Periods of Selected Buildings from the Map of Miletus
#'
#' A subset of data from the Map of Miletus for testing and demonstrating
#' working with the R-Package chrongler.
#'
#' @format ## `BuildingsMilet`
#' A data frame with 110 rows and 3 columns:
#' \describe{
#'   \item{identifier}{The name of a building in Miletus.}
#'   \item{period.start}{The earliest period the building can be dated to.}
#'   \item{period.end}{The latest period the building can be dated to.}
#' }
#' @source <https://geoserver.dainst.org/catalogue/uuid/53438148-30d1-11ec-9582-0242ac140004>
"BuildingsMilet"

#' Periods as Listed in the Miletus Documentation Manual
#'
#' This is a slightly adapted set of periods as used by the Miletus Excavation,
#' for details see the "Miletus Documentation Manual":
#' Miletus Documentation Manual: Surveying, Forms, Find Processing and
#' Database Usage, v.2.0, <https://doi.org/10.25592/uhhfdm.10029>
#'
#' @format ## `PeriodsMilet`
#' A data frame with 19 rows and 5 columns:
#' \describe{
#'   \item{group}{The 'group' of periods the periods belongs to.}
#'   \item{values}{The label of the period.}
#'   \item{dating.min}{The "starting" year of the period.}
#'   \item{dating.max}{The last year of the period.}
#'   \item{color}{The color scheme as used by the Map of Miletus (see "BuildingsMilet" dataset).}
#' }
#' @source <https://doi.org/10.25592/uhhfdm.10029>
"PeriodsMilet"


#' Example Periods for the Roman Republic and the Roman Empire
#'
#' This is a set of Periods compiled from [iDAI.chronontology](https://chronontology.dainst.org)
#' used to illustrate `chrongler`s functionality. It is compiled in the
#' vignette "chrongler wrangles categorical chronological data", see:
#' \code{vignette("chrongler_workflow", package = "chrongler")}
#'
#'
#' @format ## `RomanPeriods`
#' A data frame with 8 rows and 6 columns:
#' \describe{
#'   \item{group}{The 'group' of periods the periods belongs to.}
#'   \item{values}{The label of the period.}
#'   \item{dating.min}{The "starting" year of the period.}
#'   \item{dating.max}{The last year of the period.}
#'   \item{color}{A color for the period.}
#'   \item{source}{A list of URL and comment on the source of the period.}
#' }
#' @source <https://chronontology.dainst.org>
"RomanPeriods"

