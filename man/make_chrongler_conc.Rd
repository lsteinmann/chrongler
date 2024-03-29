% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_chrongler_conc.R
\name{make_chrongler_conc}
\alias{make_chrongler_conc}
\title{Produce a concordance object that can be used with \code{chrongler} functions}
\usage{
make_chrongler_conc(
  file,
  cols = list(group = NA, values = NA, dating.min = NA, dating.max = NA, color = NA,
    source = NA)
)
}
\arguments{
\item{file}{chr / data.frame / matrix}

\item{cols}{list with specific values and column names or Indices, see description.}
}
\value{
A list.
}
\description{
The csv-file or data.frame you supply should contain containing two columns: the first column
should list all period-"groups", e.g. 'Roman Imperial Period', and the
second one should list all the corresponding periods, e.g.
'Early Imperial', ..., 'Late Imperial' and so forth. The grouping
data.frame should be in chronological order, because the results will
be returned as an ordered factor according to this order.
}
\details{
If the column names of your provided csv file or data.frame already
conform to the following logic, you can omit the \code{cols}-argument altogether.
If not, you need to provide the a list in the following format to \code{cols}:
\itemize{
\item \strong{groups} (\emph{chr/int}) Name or Index of the column that contains
the grouping variable.
\item \strong{values} (\emph{chr/int}) Name or Index of the column that contains
all period names. These are expected to be in chronological order!
\item \strong{dating.min} (\emph{chr/int}) Name or Index of the column that contains
the minimum / 'from' absolute date for each period.
Expects negative values for BCE and positive values for CE.
\item \strong{dating.max} (\emph{chr/int}) Name or Index of the column that contains
the maximum / 'to' absolute date for each period.
Expects negative values for BCE and positive values for CE.
\item \strong{color} (\emph{chr/int}) Name or Index of the column that contains
the color associated with each value for consistent
color scales in plots.
}
}
\examples{
\dontrun{
filename <- system.file(package = "chrongler",
                    "extdata/2023_periods_grouping_example.csv")
conc <- make_chrongler_conc(filename)
str(conc)

filename <- system.file(package = "chrongler", "extdata/2023_periods_grouping_example.csv")
table <- read.csv(filename)
conc <- make_chrongler_conc(table)
str(conc)
}
}
