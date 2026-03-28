# Produce a Concordance for Wrangling Categorical Data with `chrongler`

`chrongler` assumes that periods come in groups: A set of larger scale
periods or "era"s, which can span multiple periods. The vignette
demonstrates this with the example of the Roman Republic and the Roman
Empire, both encompassing multiple "sub-periods". Time periods also have
a beginning and an end that can be expressed in years BCE/CE.
`chrongler` currently uses negative and positive numeric values for
these dates. (Be aware or potential year-0-errors when further
processing your data.) With `make_chrongler_conc()` you can produce a
concordance for periods and their groups, start, and end dates, which is
used by all other `chrongler`-functions to reformat your data.

## Usage

``` r
make_chrongler_conc(
  file,
  cols = list(group = NA, values = NA, dating.min = NA, dating.max = NA, color = NA,
    source = NA),
  ...
)
```

## Arguments

- file:

  chr / data.frame: Path to a *csv*-file to be read by
  [`read.csv()`](https://rdrr.io/r/utils/read.table.html), or a
  `data.frame`.

- cols:

  A named list mapping expected column roles to the corresponding column
  names (chr) or indices (int) in `file`. The following names are
  recognised:

  - **group** : column containing the period group names

  - **values** : column containing all period names, in chronological
    order

  - **dating.min** : column containing the earliest absolute date for
    the respective period (negative = BCE, positive = CE)

  - **dating.max** : column containing the latest absolute date for the
    respective period (negative = BCE, positive = CE)

  - **color** *(optional)* : column containing a colour value per period

  - **source** *(optional)* : column containing a source reference per
    period

  If your column names already match the names above, you can omit this
  argument entirely and they will be detected automatically.

- ...:

  Further arguments to be passed to
  [`read.csv()`](https://rdrr.io/r/utils/read.table.html) when `file` is
  a path.

## Value

A named list of class `chrongler.conc` with the following elements:

- **all** — a named list with one entry per period (including
  group-level periods). Each entry contains:

  - `name` — the period name (chr)

  - `group` — the group this period belongs to (chr)

  - `dating` — a list with `from` and `to` absolute dates (num)

  - `color` — the associated colour value (chr), or `NULL` if not
    supplied

  - `source` — the associated source reference, or `NULL` if not
    supplied

- **grouped** — a named list with one entry per group. Each entry is an
  ordered factor of the periods belonging to that group, in
  chronological order.

- **group.order** — an ordered factor of all group names, in
  chronological order.

- **period.order** — an ordered factor of all non-group period names, in
  chronological order.

- **dating** — a named list with one `list(from, to)` entry per period,
  including group-level periods.

- **color** — a named character vector of colour values, one per period.

- **source** — a named list of source references, one per period.

This class is used by all other `chrongler` functions to process data.

## Details

The `data.frame` or *csv*-file you supply to this function should be
ordered, i.e. earlier periods should come in the rows before later
periods. Warnings are supplied if the dating seems to be out of order.
This happens if the value in `dating.max` is lower than or equal to the
corresponding value in `dating.min`.

## See also

The "chrongler wrangles categorical chronological data" vignette for a
detailed explanation of the expected data structure:
[`vignette("chrongler_workflow", package = "chrongler")`](https://lsteinmann.github.io/chrongler/articles/chrongler_workflow.md)

## Examples

``` r
filename <- system.file(package = "chrongler",
                    "extdata/2023_periods_grouping_example.csv")
conc <- make_chrongler_conc(filename)
str(conc)
#> List of 7
#>  $ all         :List of 19
#>   ..$ Classical                       :List of 5
#>   .. ..$ name  : chr "Classical"
#>   .. ..$ group : chr "Classical"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int -480
#>   .. .. ..$ to  : int -324
#>   .. ..$ color : chr "#441794"
#>   .. ..$ source: logi NA
#>   ..$ Early classical                 :List of 5
#>   .. ..$ name  : chr "Early classical"
#>   .. ..$ group : chr "Classical"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int -480
#>   .. .. ..$ to  : int -426
#>   .. ..$ color : chr "#582BA8"
#>   .. ..$ source: logi NA
#>   ..$ Late Classical                  :List of 5
#>   .. ..$ name  : chr "Late Classical"
#>   .. ..$ group : chr "Classical"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int -425
#>   .. .. ..$ to  : int -324
#>   .. ..$ color : chr "#441794"
#>   .. ..$ source: logi NA
#>   ..$ Hellenistic                     :List of 5
#>   .. ..$ name  : chr "Hellenistic"
#>   .. ..$ group : chr "Hellenistic"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int -323
#>   .. .. ..$ to  : int -32
#>   .. ..$ color : chr "#1A237E"
#>   .. ..$ source: logi NA
#>   ..$ Early hellenistic               :List of 5
#>   .. ..$ name  : chr "Early hellenistic"
#>   .. ..$ group : chr "Hellenistic"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int -323
#>   .. .. ..$ to  : int -178
#>   .. ..$ color : chr "#283593"
#>   .. ..$ source: logi NA
#>   ..$ Late Hellenistic                :List of 5
#>   .. ..$ name  : chr "Late Hellenistic"
#>   .. ..$ group : chr "Hellenistic"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int -177
#>   .. .. ..$ to  : int -32
#>   .. ..$ color : chr "#1A237E"
#>   .. ..$ source: logi NA
#>   ..$ Roman imperial                  :List of 5
#>   .. ..$ name  : chr "Roman imperial"
#>   .. ..$ group : chr "Roman imperial"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int -31
#>   .. .. ..$ to  : int 305
#>   .. ..$ color : chr "#1565C0"
#>   .. ..$ source: logi NA
#>   ..$ Early imperial                  :List of 5
#>   .. ..$ name  : chr "Early imperial"
#>   .. ..$ group : chr "Roman imperial"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int -31
#>   .. .. ..$ to  : int 98
#>   .. ..$ color : chr "#1976D2"
#>   .. ..$ source: logi NA
#>   ..$ Middle Imperial                 :List of 5
#>   .. ..$ name  : chr "Middle Imperial"
#>   .. ..$ group : chr "Roman imperial"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int 100
#>   .. .. ..$ to  : int 192
#>   .. ..$ color : chr "#1565C0"
#>   .. ..$ source: logi NA
#>   ..$ Late Imperial                   :List of 5
#>   .. ..$ name  : chr "Late Imperial"
#>   .. ..$ group : chr "Roman imperial"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int 193
#>   .. .. ..$ to  : int 305
#>   .. ..$ color : chr "#0D47A1"
#>   .. ..$ source: logi NA
#>   ..$ Late Antiquity                  :List of 5
#>   .. ..$ name  : chr "Late Antiquity"
#>   .. ..$ group : chr "Late Antiquity"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int 306
#>   .. .. ..$ to  : int 699
#>   .. ..$ color : chr "#00BCD4"
#>   .. ..$ source: logi NA
#>   ..$ Byzantine                       :List of 5
#>   .. ..$ name  : chr "Byzantine"
#>   .. ..$ group : chr "Byzantine"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int 306
#>   .. .. ..$ to  : int 1299
#>   .. ..$ color : chr "#00ACC1"
#>   .. ..$ source: logi NA
#>   ..$ Early Byzantine / Late Antiquity:List of 5
#>   .. ..$ name  : chr "Early Byzantine / Late Antiquity"
#>   .. ..$ group : chr "Byzantine"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int 306
#>   .. .. ..$ to  : int 699
#>   .. ..$ color : chr "#00ACC1"
#>   .. ..$ source: logi NA
#>   ..$ Middle Byzantine                :List of 5
#>   .. ..$ name  : chr "Middle Byzantine"
#>   .. ..$ group : chr "Byzantine"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int 700
#>   .. .. ..$ to  : int 999
#>   .. ..$ color : chr "#0097A7"
#>   .. ..$ source: logi NA
#>   ..$ Late Byzantine                  :List of 5
#>   .. ..$ name  : chr "Late Byzantine"
#>   .. ..$ group : chr "Byzantine"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int 1000
#>   .. .. ..$ to  : int 1299
#>   .. ..$ color : chr "#00838F"
#>   .. ..$ source: logi NA
#>   ..$ Emirates Period                 :List of 5
#>   .. ..$ name  : chr "Emirates Period"
#>   .. ..$ group : chr "Emirates Period"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int 1300
#>   .. .. ..$ to  : int 1425
#>   .. ..$ color : chr "#43A047"
#>   .. ..$ source: logi NA
#>   ..$ Ottoman                         :List of 5
#>   .. ..$ name  : chr "Ottoman"
#>   .. ..$ group : chr "Ottoman"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int 1426
#>   .. .. ..$ to  : int 1899
#>   .. ..$ color : chr "#1B5E20"
#>   .. ..$ source: logi NA
#>   ..$ Recent / Modern                 :List of 5
#>   .. ..$ name  : chr "Recent / Modern"
#>   .. ..$ group : chr "Recent / Modern"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int 1900
#>   .. .. ..$ to  : int 2023
#>   .. ..$ color : chr "#004D40"
#>   .. ..$ source: logi NA
#>   ..$ undetermined                    :List of 5
#>   .. ..$ name  : chr "undetermined"
#>   .. ..$ group : chr "undetermined"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: int NA
#>   .. .. ..$ to  : int NA
#>   .. ..$ color : chr "#000000"
#>   .. ..$ source: logi NA
#>  $ grouped     :List of 9
#>   ..$ Classical      : Ord.factor w/ 15 levels "Early classical"<..: 1 2
#>   ..$ Hellenistic    : Ord.factor w/ 15 levels "Early classical"<..: 3 4
#>   ..$ Roman imperial : Ord.factor w/ 15 levels "Early classical"<..: 5 6 7
#>   ..$ Late Antiquity : Ord.factor w/ 15 levels "Early classical"<..: 8
#>   ..$ Byzantine      : Ord.factor w/ 15 levels "Early classical"<..: 9 10 11
#>   ..$ Emirates Period: Ord.factor w/ 15 levels "Early classical"<..: 12
#>   ..$ Ottoman        : Ord.factor w/ 15 levels "Early classical"<..: 13
#>   ..$ Recent / Modern: Ord.factor w/ 15 levels "Early classical"<..: 14
#>   ..$ undetermined   : Ord.factor w/ 15 levels "Early classical"<..: 15
#>  $ group.order : Ord.factor w/ 9 levels "Classical"<"Hellenistic"<..: 1 2 3 4 5 6 7 8 9
#>  $ period.order: Ord.factor w/ 15 levels "Early classical"<..: 1 2 3 4 5 6 7 8 9 10 ...
#>  $ dating      :List of 19
#>   ..$ Classical                       :List of 2
#>   .. ..$ from: int -480
#>   .. ..$ to  : int -324
#>   ..$ Early classical                 :List of 2
#>   .. ..$ from: int -480
#>   .. ..$ to  : int -426
#>   ..$ Late Classical                  :List of 2
#>   .. ..$ from: int -425
#>   .. ..$ to  : int -324
#>   ..$ Hellenistic                     :List of 2
#>   .. ..$ from: int -323
#>   .. ..$ to  : int -32
#>   ..$ Early hellenistic               :List of 2
#>   .. ..$ from: int -323
#>   .. ..$ to  : int -178
#>   ..$ Late Hellenistic                :List of 2
#>   .. ..$ from: int -177
#>   .. ..$ to  : int -32
#>   ..$ Roman imperial                  :List of 2
#>   .. ..$ from: int -31
#>   .. ..$ to  : int 305
#>   ..$ Early imperial                  :List of 2
#>   .. ..$ from: int -31
#>   .. ..$ to  : int 98
#>   ..$ Middle Imperial                 :List of 2
#>   .. ..$ from: int 100
#>   .. ..$ to  : int 192
#>   ..$ Late Imperial                   :List of 2
#>   .. ..$ from: int 193
#>   .. ..$ to  : int 305
#>   ..$ Late Antiquity                  :List of 2
#>   .. ..$ from: int 306
#>   .. ..$ to  : int 699
#>   ..$ Byzantine                       :List of 2
#>   .. ..$ from: int 306
#>   .. ..$ to  : int 1299
#>   ..$ Early Byzantine / Late Antiquity:List of 2
#>   .. ..$ from: int 306
#>   .. ..$ to  : int 699
#>   ..$ Middle Byzantine                :List of 2
#>   .. ..$ from: int 700
#>   .. ..$ to  : int 999
#>   ..$ Late Byzantine                  :List of 2
#>   .. ..$ from: int 1000
#>   .. ..$ to  : int 1299
#>   ..$ Emirates Period                 :List of 2
#>   .. ..$ from: int 1300
#>   .. ..$ to  : int 1425
#>   ..$ Ottoman                         :List of 2
#>   .. ..$ from: int 1426
#>   .. ..$ to  : int 1899
#>   ..$ Recent / Modern                 :List of 2
#>   .. ..$ from: int 1900
#>   .. ..$ to  : int 2023
#>   ..$ undetermined                    :List of 2
#>   .. ..$ from: int NA
#>   .. ..$ to  : int NA
#>  $ color       : Named chr [1:19] "#441794" "#582BA8" "#441794" "#1A237E" ...
#>   ..- attr(*, "names")= chr [1:19] "Classical" "Early classical" "Late Classical" "Hellenistic" ...
#>  $ source      : Named logi [1:19] NA NA NA NA NA NA ...
#>   ..- attr(*, "names")= chr [1:19] "Classical" "Early classical" "Late Classical" "Hellenistic" ...
#>  - attr(*, "class")= chr [1:2] "chrongler.conc" "list"

data("RomanPeriods")
df <- RomanPeriods
conc <- make_chrongler_conc(df)
str(conc)
#> List of 7
#>  $ all         :List of 8
#>   ..$ Roman Republic      :List of 5
#>   .. ..$ name  : chr "Roman Republic"
#>   .. ..$ group : chr "Roman Republic"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: num -509
#>   .. .. ..$ to  : num -31
#>   .. ..$ color : chr "#00ACC1"
#>   .. ..$ source:List of 1
#>   .. .. ..$ :List of 2
#>   .. .. .. ..$ URL    : chr "http://chronontology.dainst.org/period/RX2Rv7kcbLGk"
#>   .. .. .. ..$ comment: chr "dates slightly changed"
#>   ..$ Early Roman Republic:List of 5
#>   .. ..$ name  : chr "Early Roman Republic"
#>   .. ..$ group : chr "Roman Republic"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: num -509
#>   .. .. ..$ to  : num -265
#>   .. ..$ color : chr "#00ACC1"
#>   .. ..$ source:List of 1
#>   .. .. ..$ :List of 1
#>   .. .. .. ..$ URL: chr "https://chronontology.dainst.org/period/Y863Mey70o4t"
#>   ..$ High Roman Republic :List of 5
#>   .. ..$ name  : chr "High Roman Republic"
#>   .. ..$ group : chr "Roman Republic"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: num -264
#>   .. .. ..$ to  : num -146
#>   .. ..$ color : chr "#0097A7"
#>   .. ..$ source:List of 1
#>   .. .. ..$ :List of 1
#>   .. .. .. ..$ URL: chr "https://chronontology.dainst.org/period/DFLVz4nASAC7"
#>   ..$ Late Roman Republic :List of 5
#>   .. ..$ name  : chr "Late Roman Republic"
#>   .. ..$ group : chr "Roman Republic"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: num -145
#>   .. .. ..$ to  : num -31
#>   .. ..$ color : chr "#00838F"
#>   .. ..$ source:List of 1
#>   .. .. ..$ :List of 2
#>   .. .. .. ..$ URL    : chr "https://chronontology.dainst.org/period/GzPQDbfWXlR8"
#>   .. .. .. ..$ comment: chr "dates slightly changed"
#>   ..$ Roman Empire        :List of 5
#>   .. ..$ name  : chr "Roman Empire"
#>   .. ..$ group : chr "Roman Empire"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: num -30
#>   .. .. ..$ to  : num 476
#>   .. ..$ color : chr "#1565C0"
#>   .. ..$ source:List of 1
#>   .. .. ..$ :List of 2
#>   .. .. .. ..$ URL    : chr "https://chronontology.dainst.org/period/bCTpXj4LVC2n"
#>   .. .. .. ..$ comment: chr "dates slightly changed"
#>   ..$ Early Roman Empire  :List of 5
#>   .. ..$ name  : chr "Early Roman Empire"
#>   .. ..$ group : chr "Roman Empire"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: num -30
#>   .. .. ..$ to  : num 68
#>   .. ..$ color : chr "#1976D2"
#>   .. ..$ source:List of 1
#>   .. .. ..$ :List of 1
#>   .. .. .. ..$ URL: chr "https://chronontology.dainst.org/period/vve7mgqkCAGE"
#>   ..$ High Roman Empire   :List of 5
#>   .. ..$ name  : chr "High Roman Empire"
#>   .. ..$ group : chr "Roman Empire"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: num 69
#>   .. .. ..$ to  : num 234
#>   .. ..$ color : chr "#1565C0"
#>   .. ..$ source:List of 1
#>   .. .. ..$ :List of 2
#>   .. .. .. ..$ URL    : chr "https://chronontology.dainst.org/period/OTyMiCtkjqCz"
#>   .. .. .. ..$ comment: chr "dates slightly changed"
#>   ..$ Late Roman Empire   :List of 5
#>   .. ..$ name  : chr "Late Roman Empire"
#>   .. ..$ group : chr "Roman Empire"
#>   .. ..$ dating:List of 2
#>   .. .. ..$ from: num 235
#>   .. .. ..$ to  : num 476
#>   .. ..$ color : chr "#0D47A1"
#>   .. ..$ source:List of 1
#>   .. .. ..$ :List of 1
#>   .. .. .. ..$ URL: chr "https://chronontology.dainst.org/period/7fDysWha7NKs"
#>  $ grouped     :List of 2
#>   ..$ Roman Republic: Ord.factor w/ 6 levels "Early Roman Republic"<..: 1 2 3
#>   ..$ Roman Empire  : Ord.factor w/ 6 levels "Early Roman Republic"<..: 4 5 6
#>  $ group.order : Ord.factor w/ 2 levels "Roman Republic"<..: 1 2
#>  $ period.order: Ord.factor w/ 6 levels "Early Roman Republic"<..: 1 2 3 4 5 6
#>  $ dating      :List of 8
#>   ..$ Roman Republic      :List of 2
#>   .. ..$ from: num -509
#>   .. ..$ to  : num -31
#>   ..$ Early Roman Republic:List of 2
#>   .. ..$ from: num -509
#>   .. ..$ to  : num -265
#>   ..$ High Roman Republic :List of 2
#>   .. ..$ from: num -264
#>   .. ..$ to  : num -146
#>   ..$ Late Roman Republic :List of 2
#>   .. ..$ from: num -145
#>   .. ..$ to  : num -31
#>   ..$ Roman Empire        :List of 2
#>   .. ..$ from: num -30
#>   .. ..$ to  : num 476
#>   ..$ Early Roman Empire  :List of 2
#>   .. ..$ from: num -30
#>   .. ..$ to  : num 68
#>   ..$ High Roman Empire   :List of 2
#>   .. ..$ from: num 69
#>   .. ..$ to  : num 234
#>   ..$ Late Roman Empire   :List of 2
#>   .. ..$ from: num 235
#>   .. ..$ to  : num 476
#>  $ color       : Named chr [1:8] "#00ACC1" "#00ACC1" "#0097A7" "#00838F" ...
#>   ..- attr(*, "names")= chr [1:8] "Roman Republic" "Early Roman Republic" "High Roman Republic" "Late Roman Republic" ...
#>  $ source      :List of 8
#>   ..$ Roman Republic      :List of 2
#>   .. ..$ URL    : chr "http://chronontology.dainst.org/period/RX2Rv7kcbLGk"
#>   .. ..$ comment: chr "dates slightly changed"
#>   ..$ Early Roman Republic:List of 1
#>   .. ..$ URL: chr "https://chronontology.dainst.org/period/Y863Mey70o4t"
#>   ..$ High Roman Republic :List of 1
#>   .. ..$ URL: chr "https://chronontology.dainst.org/period/DFLVz4nASAC7"
#>   ..$ Late Roman Republic :List of 2
#>   .. ..$ URL    : chr "https://chronontology.dainst.org/period/GzPQDbfWXlR8"
#>   .. ..$ comment: chr "dates slightly changed"
#>   ..$ Roman Empire        :List of 2
#>   .. ..$ URL    : chr "https://chronontology.dainst.org/period/bCTpXj4LVC2n"
#>   .. ..$ comment: chr "dates slightly changed"
#>   ..$ Early Roman Empire  :List of 1
#>   .. ..$ URL: chr "https://chronontology.dainst.org/period/vve7mgqkCAGE"
#>   ..$ High Roman Empire   :List of 2
#>   .. ..$ URL    : chr "https://chronontology.dainst.org/period/OTyMiCtkjqCz"
#>   .. ..$ comment: chr "dates slightly changed"
#>   ..$ Late Roman Empire   :List of 1
#>   .. ..$ URL: chr "https://chronontology.dainst.org/period/7fDysWha7NKs"
#>  - attr(*, "class")= chr [1:2] "chrongler.conc" "list"
```
