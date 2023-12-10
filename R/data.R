#' Periods of Selected Buildings from the Map of Miletus
#'
#' A subset of data from the Map of Miletus for testing and demonstrating
#' working with the R-Package chrongler.
#'
#' @seealso [exChronglerConc]
#'
#' @format ## `BuildingsMilet`
#' A data frame with 110 rows and 3 columns:
#' \describe{
#'   \item{identifier}{The name of a building in Miletus.}
#'   \item{period.start, period.end}{The earliest and latest period each building is dated to.}
#' }
#' @source <https://geoserver.dainst.org/catalogue/#/map/5764>
"BuildingsMilet"

#' Example Concordance to be used with BuildingsMilet-Dataset
#'
#' A subset of the periods and their translations used in the Miletus database.
#' Included to demonstrate and test the chrongler package with the [BuildingsMilet] dataset.
#'
#' @seealso [BuildingsMilet]
#'
#' @source *L. Steinmann, S. Huy, F. Sliwka, D. Göçmen, N. Lordoğlu,
#' J. Zurbach, C. Berns (2023). Miletus Documentation Manual:
#' Surveying, Forms, Find Processing and Database-Usage, v2.0 (Version 2.0).
#' <http://doi.org/10.25592/uhhfdm.11389>*
"exChronglerConc"
