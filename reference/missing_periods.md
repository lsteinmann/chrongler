# Check for and Return Periods not Present in the Concordance

Check for and Return Periods not Present in the Concordance

## Usage

``` r
missing_periods(present_periods, possible_periods)
```

## Arguments

- present_periods:

  A vector of values to check for

- possible_periods:

  The vector of values against which to check

## Value

a character vector with the values from `present_periods` not present in
`possible_periods`, or `character(0)` if all are present.
