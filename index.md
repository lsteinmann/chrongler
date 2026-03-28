# *chrongler* wrangles categorical chronological data

This package provides convenience functions that are helpful for working
with archaeological or historical data, where chronological information
is stored as a ‘period’ (categorical values that may indicate chronology
in one way or another). The functions are purely for formatting and
‘wrangling’ such data, and do not contain any means of analysis or
further processing.

# Installation

*chrongler* is not (yet?) on CRAN. You can install the current version
from GitHub using:

``` r
remotes::install_github("lsteinmann/chrongler", build_vignettes = TRUE)
```

# Scope

Using *chrongler* (and a concordance of periods and their grouping and
absolute dating made with
[`make_chrongler_conc()`](https://lsteinmann.github.io/chrongler/reference/make_chrongler_conc.md))
you can easily:

- group and un-group the categorical dating of objects
  ([`group_periods()`](https://lsteinmann.github.io/chrongler/reference/group_periods.md),
  [`ungroup_periods()`](https://lsteinmann.github.io/chrongler/reference/ungroup_periods.md))
- duplicate rows of objects according to their periods or groups
  ([`duplicate_by()`](https://lsteinmann.github.io/chrongler/reference/duplicate_by.md))
- add absolute dating based on the periods an object is dated to
  ([`derive_dating()`](https://lsteinmann.github.io/chrongler/reference/derive_dating.md))
- add the period an object would be dated to based on absolute dating
  values
  ([`derive_period()`](https://lsteinmann.github.io/chrongler/reference/derive_period.md))

A vignette explains the functions and especially setting up the
concordance in detail, see:

``` r
browseVignettes("chrongler")
```

or read it online: [chrongler wrangles categorical chronological
data](https://lsteinmann.github.io/chrongler/articles/chrongler_workflow.html).

# why

I made it mainly for myself to avoid redundancies in various other
projects and have the process a bit more organized. There is significant
overlap with the way that data imported from [Field
Desktop](https://github.com/dainst/idai-field) via
[idaifieldR](https://github.com/lsteinmann/idaifieldR) is formatted,
since this is my main use case. Of course, *chrongler* can just as
easily be used with data gathered elsewhere as well.
