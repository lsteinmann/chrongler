
<!-- badges: start -->
[![R-CMD-check](https://github.com/lsteinmann/chrongler/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lsteinmann/chrongler/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/lsteinmann/chrongler/branch/main/graph/badge.svg)](https://codecov.io/gh/lsteinmann/chrongler?branch=main)
<!-- badges: end -->

# chrongler wrangles categorical chronological data
This package provides convenience functions that are helpful for working with archaeological or historical data, where chronological information is stored as a 'period' (categorical values that may indicate chronology in one way or another). The functions are purely for formatting and 'wrangling' such data, and do not contain any means of analysis or further processing. 

# Installation
chrongler is not on CRAN. You can install the current version from GitHub using: 
``` r
remotes::install_github("lsteinmann/chrongler", build_vignettes = TRUE)
```

# Scope
Using chrongler (and a concordance of periods and their grouping and absolute dating made with `make_chrongler_conc()`) you can easily: 

* group and un-group the categorical dating of objects (`group_periods()`, `ungroup_periods()`)
* duplicate rows of objects according to their periods or groups (`duplicate_by()`)
* add absolute dating based on the periods an object is dated to (`derive_dating()`)
* add the period an object would be dated to based on absolute dating values (`derive_period()`)

The inaccurately named "example_workflow"-vignette explains the functions and especially setting up the concordance in detail, see:
``` r
browseVignettes("chrongler")
```

# why
I made it mainly for myself to avoid redundancies in various other projects and have the process a bit more organized. You may notice that it is very much tailored to data imported from [Field Desktop](https://github.com/dainst/idai-field) via [idaifieldR](https://github.com/lsteinmann/idaifieldR).


