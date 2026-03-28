# Return NA if a Value is NULL or of Length 0

Return NA if a Value is NULL or of Length 0

## Usage

``` r
na_if_empty(x)
```

## Arguments

- x:

  A value.

## Value

`x` or `NA`

## Examples

``` r
if (FALSE) { # \dontrun{
na_if_empty(list())
na_if_empty(character(0))
na_if_empty(NULL)
na_if_empty("")
} # }
```
