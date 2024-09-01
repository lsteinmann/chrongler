#' Basic Query for iDAI.chronontology
#'
#' This function searches all iDAI.chronontology periods for the string
#' supplied as 'value'. It always gets all available results.
#' No further filtering or more specific querying is implemented (yet).
#'
#' @param value (chr) The value you want to search for in all chronontology-periods.
#'
#' @return A list of results as handed out by iDAI.chronontology.
#'
#' @source
#'  * data: [iDAI.chronontology](https://chronontology.dainst.org/)
#'  * api: [chronontology REST API reference](https://github.com/dainst/chronontology-backend/blob/master/docs/rest-api-reference.md)
#'
#' @seealso [from_chronontology()]
#'
#' @export
#'
#' @examples
#' roman_periods <- query_chronontology(value = "roman")
#' names <- unlist(lapply(roman_periods, function(x) x$resource$names$en))
query_chronontology <- function(value = "roman") {
  stopifnot(is.character(value))

  url <- "https://chronontology.dainst.org"
  headers <- list(`Content-Type` = "application/json",
                  Accept = "application/json")
  ChronClient <- crul::HttpClient$new(url = url, headers = headers)

  response <- ChronClient$get(path = paste0('data/period/?q=', value, '&size=1'))
  response <- response$parse("UTF-8")
  list <- jsonlite::fromJSON(response, FALSE)

  if (list$total > length(list$results)) {
    response <- ChronClient$get(path = paste0('data/period/?q=', value, '&size=1'))
    response <- response$parse("UTF-8")
    list <- jsonlite::fromJSON(response, FALSE)
  }

  result <- list$results

  return(result)
}
