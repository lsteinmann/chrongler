# Example Periods for the Roman Republic and the Roman Empire

This is a set of Periods compiled from
[iDAI.chronontology](https://chronontology.dainst.org) used to
illustrate `chrongler`s functionality. It is compiled in the vignette
"chrongler wrangles categorical chronological data", see:
[`vignette("chrongler_workflow", package = "chrongler")`](https://lsteinmann.github.io/chrongler/articles/chrongler_workflow.md)

## Usage

``` r
RomanPeriods
```

## Format

### `RomanPeriods`

A data frame with 8 rows and 6 columns:

- group:

  The 'group' of periods the periods belongs to.

- values:

  The label of the period.

- dating.min:

  The "starting" year of the period.

- dating.max:

  The last year of the period.

- color:

  A color for the period.

- source:

  A list of URL and comment on the source of the period.

## Source

<https://chronontology.dainst.org>
