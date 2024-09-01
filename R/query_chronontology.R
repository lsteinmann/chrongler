#' Basic Query for iDAI.chronontology
#'
#'
#'
#' @param value (chr) The value you want to search for in all chronontology-periods.
#'
#' @return A list of results as handed out by iDAI.chronontology.
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
