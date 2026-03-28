# chrongler wrangles categorical chronological data

**chrongler** is an R-package that contains functions meant to re-format
archaeological or historical *periods* stored as categorical variables.
Using a concordance provided by the user, the periods can be replaced
with numerical start and end values taken from that concordance. This
vignettes comments and illustrate the functions **chrongler** provides.

## Why are we doing this?

As archaeologists, historians, or researchers in related fields, we
rarely have the luxury of precise dating of objects or events.
Traditionally, we use phases, epochs, eras, periods and other
representations of time spans to categorize and date our findings or
finds. We describe objects as being produced or used in “the Hellenistic
period”, or “from late Classical to Early Hellenistic” or similar. While
most of the time, this is the more *honest* way of dating, as we can
rarely (if ever) assert that something was produced e.g. between the
years 478 and 356 BCE, it can greatly hinder our ability to visualize,
sort and compare our findings. That holds true both for working with our
*own* data as well as comparing with what others have published.

**chrongler** provides a convenient way to translate between the
categorical and the numeric world of dating things. **chrongler**s
approach is very pragmatic. Moving from periods to numeric dates may
seem like we are pretending to have accurate and precise data when we do
not. When working with this package, we need to be aware and
acknowledge, that all it can give us is a rough translation so help
visualize and order large amounts of finds. It is very much meant to aid
with the broad quantitative overview, the big picture of larger
collections, and not for the specific chronology of single objects,
events, or entities.

## Installing chrongler

**chrongler** is not (yet) on CRAN. You can install the latest version
from the [GitHub-repository](https://github.com/lsteinmann/chrongler)
using:

``` r
remotes::install_github("lsteinmann/chrongler", build_vignettes = TRUE)
```

And then load the package:

``` r
library(chrongler)
```

## Preparing the Concordance

**chrongler** relies on a concordance object supplied by the user. The
concordance holds the grouping of periods, e.g. the information that
“Early Classical” is part of “Classical”, and the numeric dating
attached to the periods in question. The concordance can be brought into
the format needed by **chrongler** using the
[`make_chrongler_conc()`](https://lsteinmann.github.io/chrongler/reference/make_chrongler_conc.md)-function.
While it is easier to create the data in some spreadsheet software and
import a csv-file, I will demonstrate the data structure here by putting
it together in R.

### Creating the Example Data

We will get the dates and values for our examples manually from
[iDAI.chronontology](https://chronontology.dainst.org/). Let’s use the
*[Roman Republic](https://chronontology.dainst.org/period/RX2Rv7kcbLGk)*
and the *[Roman
Empire](https://chronontology.dainst.org/period/bCTpXj4LVC2n)* as an
easy example. *Roman Republic* contains three basic periods: “Early
Roman Republic”, “High Roman Republic”, “Late Roman Republic”. *Roman
Empire* also contains three periods: “Early Roman Empire”, “High Roman
Empire” and “Late Roman Empire”). Thus, our data.frame (or spreadsheet)
would need at least six rows. Since it is needed for some optional
functionality, I will also treat the groups themselves (*Roman Republic*
and *Roman Empire*) as another period:

``` r
conc_df <- data.frame(values = c("Roman Republic", "Early Roman Republic", "High Roman Republic", "Late Roman Republic", "Roman Empire", "Early Roman Empire", "High Roman Empire", "Late Roman Empire"))
```

As established above, the first four periods belong to the group “Roman
Republic”, the following four to the “Roman Empire”. We add this info as
another column in our data.frame:

``` r
conc_df$group <- c(rep("Roman Republic", 4), rep("Roman Empire", 4))
```

Now we need to a assign absolute dates to all our periods. **chrongler**
expects dates BCE to be negative and dates CE to be positive values.
Obviously, how you categorize and date periods in the place you are
working at depends on many things, and this is just an example. I am
using the values from these chronOntology-pages:

- [Early Roman
  Republic](https://chronontology.dainst.org/period/Y863Mey70o4t)
- [High Roman
  Republic](https://chronontology.dainst.org/period/DFLVz4nASAC7)
- [Late Roman
  Republic](https://chronontology.dainst.org/period/GzPQDbfWXlR8)
- [Early Roman
  Empire](https://chronontology.dainst.org/period/vve7mgqkCAGE)
- [High Roman
  Empire](https://chronontology.dainst.org/period/OTyMiCtkjqCz)
- [Late Roman
  Empire](https://chronontology.dainst.org/period/7fDysWha7NKs)

Avoid using overlapping dates or identical start and end dates for
different periods. (I modified the end of *High Roman Empire* to conform
to that.) Ideally, your group has the maximum extend of all periods it
contains, though this is not the case in the chronOntology-period we are
using, so I modified this again.

``` r
conc_df$dating.min <- c(-509, -509, -264, -145, -30, -30, 69, 235)
conc_df$dating.max <- c(-31, -265, -146, -31, 476, 68, 234, 476)
```

It is optional, but you should consider adding some sources to your
table or data.frame for future transparency. Supply them either as a
list detailing what you are adding, or as a simple character vector:

``` r
conc_df$source <- list(
  list(URL = "http://chronontology.dainst.org/period/RX2Rv7kcbLGk", comment = "dates slightly changed"),
  list(URL = "https://chronontology.dainst.org/period/Y863Mey70o4t"),
  list(URL = "https://chronontology.dainst.org/period/DFLVz4nASAC7"),
  list(URL = "https://chronontology.dainst.org/period/GzPQDbfWXlR8", comment = "dates slightly changed"),
  list(URL = "https://chronontology.dainst.org/period/bCTpXj4LVC2n", comment = "dates slightly changed"),
  list(URL = "https://chronontology.dainst.org/period/vve7mgqkCAGE"),
  list(URL = "https://chronontology.dainst.org/period/OTyMiCtkjqCz", comment = "dates slightly changed"),
  list(URL = "https://chronontology.dainst.org/period/7fDysWha7NKs")
)
```

When importing a csv-file, you would need to reformat this column
manually if you wish to add a commented list. Again, optionally, you can
supply a custom colour scale to each period:

``` r
conc_df$color <- c("#00ACC1", "#00ACC1", "#0097A7", "#00838F", "#1565C0", "#1976D2", "#1565C0", "#0D47A1")
```

The resulting data.frame should look this:

| values               | group          | dating.min | dating.max | source                                                                         | color    |
|:---------------------|:---------------|-----------:|-----------:|:-------------------------------------------------------------------------------|:---------|
| Roman Republic       | Roman Republic |       -509 |        -31 | <http://chronontology.dainst.org/period/RX2Rv7kcbLGk>, dates slightly changed  | \#00ACC1 |
| Early Roman Republic | Roman Republic |       -509 |       -265 | <https://chronontology.dainst.org/period/Y863Mey70o4t>                         | \#00ACC1 |
| High Roman Republic  | Roman Republic |       -264 |       -146 | <https://chronontology.dainst.org/period/DFLVz4nASAC7>                         | \#0097A7 |
| Late Roman Republic  | Roman Republic |       -145 |        -31 | <https://chronontology.dainst.org/period/GzPQDbfWXlR8>, dates slightly changed | \#00838F |
| Roman Empire         | Roman Empire   |        -30 |        476 | <https://chronontology.dainst.org/period/bCTpXj4LVC2n>, dates slightly changed | \#1565C0 |
| Early Roman Empire   | Roman Empire   |        -30 |         68 | <https://chronontology.dainst.org/period/vve7mgqkCAGE>                         | \#1976D2 |
| High Roman Empire    | Roman Empire   |         69 |        234 | <https://chronontology.dainst.org/period/OTyMiCtkjqCz>, dates slightly changed | \#1565C0 |
| Late Roman Empire    | Roman Empire   |        235 |        476 | <https://chronontology.dainst.org/period/7fDysWha7NKs>                         | \#0D47A1 |

This example `data.frame` is included in the package data as .

### The Concordance

Obviously, it is much more convenient for you to put all this
information together in some spreadsheet software, save it as a
*.csv*-file, and then import it here, rather than producing the object
in this way in R. I simply wanted to comment in detail on the values
expected by **chrongler** and the logic behind this. The actual function
is as simple as it gets, though depending on your table you can supply a
list of column names or indices to the ‘cols’ argument - if for some
reason they are different than the ones used in the above example. See
the documentation for info on that
([`?make_chrongler_conc`](https://lsteinmann.github.io/chrongler/reference/make_chrongler_conc.md)).
Order of the columns is not important.

``` r
conc <- make_chrongler_conc(conc_df)
```

The concordance-list will contain all the original information (albeit
in list format) in the **all**-sublist. **group** will contain a list
for each group, in which the respective periods are listed as an ordered
factor. **group.order** is an ordered factor of all groups.
**period.order** is an ordered factor of all periods that are not
groups. **dating**, **color** and **source** contain the respective
information for each period, with **dating** divided into a **from** and
**to** date.

All other **chrongler**-functions expect this concordance along with the
actual data. Periods in the actual data are expected to match the
periods in this concordance.

## Example data

We will use example data with no connection to the real world for this
vignette, which illustrates some of the situations we have with real
world data, i.e. spans of multiple groups, mixes of groups and periods
etc.:

| identifier | start                | end                 |
|:-----------|:---------------------|:--------------------|
| Obj_1      | Late Roman Empire    | Late Roman Empire   |
| Obj_2      | Early Roman Republic | High Roman Republic |
| Obj_3      | Early Roman Republic | High Roman Empire   |
| Obj_4      | Early Roman Empire   | High Roman Empire   |
| Obj_5      | Roman Republic       | Roman Empire        |
| Obj_6      | High Roman Republic  | Late Roman Republic |
| Obj_7      | Roman Republic       | Early Roman Empire  |
| Obj_8      | High Roman Empire    | High Roman Empire   |
| Obj_9      | Late Roman Republic  | Early Roman Empire  |
| Obj_10     | Early Roman Empire   | Late Roman Empire   |

As you can see, every object is dated to a *period*. This can either be
one of the finer chronological periods (early, high and late republican
or imperial) or one of the two groups.

## Grouping and Ungrouping

With **chrongler** and our previously put together concordance, we can
now simply get all of these datings to the same level of resolution, for
example by “ungrouping” them. The
[`ungroup_periods()`](https://lsteinmann.github.io/chrongler/reference/ungroup_periods.md)-function
takes the data, our concordance, and the columns names or indices of the
respective start and end columns as arguments:

``` r
ungrouped_data <- ungroup_periods(data_df, conc, start = "start", end = "end")
```

| identifier | start                | end                 | start.ungr           | end.ungr            |
|:-----------|:---------------------|:--------------------|:---------------------|:--------------------|
| Obj_1      | Late Roman Empire    | Late Roman Empire   | Late Roman Empire    | Late Roman Empire   |
| Obj_2      | Early Roman Republic | High Roman Republic | Early Roman Republic | High Roman Republic |
| Obj_3      | Early Roman Republic | High Roman Empire   | Early Roman Republic | High Roman Empire   |
| Obj_4      | Early Roman Empire   | High Roman Empire   | Early Roman Empire   | High Roman Empire   |
| Obj_5      | Roman Republic       | Roman Empire        | Early Roman Republic | Late Roman Empire   |
| Obj_6      | High Roman Republic  | Late Roman Republic | High Roman Republic  | Late Roman Republic |
| Obj_7      | Roman Republic       | Early Roman Empire  | Early Roman Republic | Early Roman Empire  |
| Obj_8      | High Roman Empire    | High Roman Empire   | High Roman Empire    | High Roman Empire   |
| Obj_9      | Late Roman Republic  | Early Roman Empire  | Late Roman Republic  | Early Roman Empire  |
| Obj_10     | Early Roman Empire   | Late Roman Empire   | Early Roman Empire   | Late Roman Empire   |

As you can see, Objects 5 and 7 are now represented by the earliest or
latest period from the respective group, depending on whether it was set
as the start or end date.

[`group_periods()`](https://lsteinmann.github.io/chrongler/reference/group_periods.md)
does exactly the reverse:

``` r
grouped_data <- group_periods(ungrouped_data, conc, start = "start.ungr", end = "end.ungr")
```

| identifier | start                | end                 | start.ungr           | end.ungr            | start.grpd     | end.grpd       |
|:-----------|:---------------------|:--------------------|:---------------------|:--------------------|:---------------|:---------------|
| Obj_1      | Late Roman Empire    | Late Roman Empire   | Late Roman Empire    | Late Roman Empire   | Roman Empire   | Roman Empire   |
| Obj_2      | Early Roman Republic | High Roman Republic | Early Roman Republic | High Roman Republic | Roman Republic | Roman Republic |
| Obj_3      | Early Roman Republic | High Roman Empire   | Early Roman Republic | High Roman Empire   | Roman Republic | Roman Empire   |
| Obj_4      | Early Roman Empire   | High Roman Empire   | Early Roman Empire   | High Roman Empire   | Roman Empire   | Roman Empire   |
| Obj_5      | Roman Republic       | Roman Empire        | Early Roman Republic | Late Roman Empire   | Roman Republic | Roman Empire   |
| Obj_6      | High Roman Republic  | Late Roman Republic | High Roman Republic  | Late Roman Republic | Roman Republic | Roman Republic |
| Obj_7      | Roman Republic       | Early Roman Empire  | Early Roman Republic | Early Roman Empire  | Roman Republic | Roman Empire   |
| Obj_8      | High Roman Empire    | High Roman Empire   | High Roman Empire    | High Roman Empire   | Roman Empire   | Roman Empire   |
| Obj_9      | Late Roman Republic  | Early Roman Empire  | Late Roman Republic  | Early Roman Empire  | Roman Republic | Roman Empire   |
| Obj_10     | Early Roman Empire   | Late Roman Empire   | Early Roman Empire   | Late Roman Empire   | Roman Empire   | Roman Empire   |

## Deriving the Absolute Dating

Using the concordance, we can assign a numerical dating range to each
object based on the period(s) it is dated to. Be aware of the
implications of doing that: This in only useful to present a broad
chronological overview of large amounts of data, and keep it comparable,
and should not be used for any subsequent qualitative work on the single
objects themselves. We should not fall into the trap of assuming the
numbers are correct or meaningful, just because they exist.

``` r
data_abs <- derive_dating(data_df, conc, start = "start", end = "end")
```

| identifier | start                | end                 | dating.min | dating.max | dating.source       |
|:-----------|:---------------------|:--------------------|-----------:|-----------:|:--------------------|
| Obj_1      | Late Roman Empire    | Late Roman Empire   |        235 |        476 | Derived from period |
| Obj_2      | Early Roman Republic | High Roman Republic |       -509 |       -146 | Derived from period |
| Obj_3      | Early Roman Republic | High Roman Empire   |       -509 |        234 | Derived from period |
| Obj_4      | Early Roman Empire   | High Roman Empire   |        -30 |        234 | Derived from period |
| Obj_5      | Roman Republic       | Roman Empire        |       -509 |        476 | Derived from period |
| Obj_6      | High Roman Republic  | Late Roman Republic |       -264 |        -31 | Derived from period |
| Obj_7      | Roman Republic       | Early Roman Empire  |       -509 |         68 | Derived from period |
| Obj_8      | High Roman Empire    | High Roman Empire   |         69 |        234 | Derived from period |
| Obj_9      | Late Roman Republic  | Early Roman Empire  |       -145 |         68 | Derived from period |
| Obj_10     | Early Roman Empire   | Late Roman Empire   |        -30 |        476 | Derived from period |

Be aware: if your `data.frame` already contains columns called
“dating.min”, “dating.max” and “dating.source” (see also in the
documentation of
[`?derive_dating`](https://lsteinmann.github.io/chrongler/reference/derive_dating.md))
only `NA`-values will be filled with the rest of the data remaining
untouched.

## Deriving the Period from Absolute Dating

Likewise, we can derive the period in cases where we only know the
absolute dating. Let’s use these somewhat arbitrary numbers:

``` r
data_rd <- data.frame(identifier = paste0("Obj_", seq(11, 20, 1)))
data_rd$dating.min <- sample(seq(min(conc_df$dating.min), max(conc_df$dating.min)-100, 1), 10)
data_rd$dating.max <- data_rd$dating.min + sample(1:100, 10)
```

| identifier | dating.min | dating.max |
|:-----------|-----------:|-----------:|
| Obj_11     |       -337 |       -313 |
| Obj_12     |       -103 |        -24 |
| Obj_13     |        -50 |         27 |
| Obj_14     |       -447 |       -445 |
| Obj_15     |        -25 |         37 |
| Obj_16     |         49 |        104 |
| Obj_17     |        -58 |        -15 |
| Obj_18     |       -309 |       -213 |
| Obj_19     |       -313 |       -219 |
| Obj_20     |          7 |         12 |

And automatically add the period range:

``` r
data_rd_pr <- derive_period(data_rd, conc, min = "dating.min", max = "dating.max")
```

| identifier | dating.min | dating.max | period.start         | period.end           | period.source                |
|:-----------|-----------:|-----------:|:---------------------|:---------------------|:-----------------------------|
| Obj_11     |       -337 |       -313 | Early Roman Republic | Early Roman Republic | Derived from absolute dating |
| Obj_12     |       -103 |        -24 | Late Roman Republic  | Early Roman Empire   | Derived from absolute dating |
| Obj_13     |        -50 |         27 | Late Roman Republic  | Early Roman Empire   | Derived from absolute dating |
| Obj_14     |       -447 |       -445 | Early Roman Republic | Early Roman Republic | Derived from absolute dating |
| Obj_15     |        -25 |         37 | Early Roman Empire   | Early Roman Empire   | Derived from absolute dating |
| Obj_16     |         49 |        104 | Early Roman Empire   | High Roman Empire    | Derived from absolute dating |
| Obj_17     |        -58 |        -15 | Late Roman Republic  | Early Roman Empire   | Derived from absolute dating |
| Obj_18     |       -309 |       -213 | Early Roman Republic | High Roman Republic  | Derived from absolute dating |
| Obj_19     |       -313 |       -219 | Early Roman Republic | High Roman Republic  | Derived from absolute dating |
| Obj_20     |          7 |         12 | Early Roman Empire   | Early Roman Empire   | Derived from absolute dating |

## Duplicating Objects by Period

In some cases - especially when working with period ranges, one would
like to have a row for each dating ‘instance’ of an object. The
[`duplicate_by()`](https://lsteinmann.github.io/chrongler/reference/duplicate_by.md)
function does this conveniently for you. Each row (= object) will be
duplicated by the number of periods or groups it can be dated to. A
`fraction` column is added that equals to 1 divided by the number of
periods the object can be dated to.

You can either do this by group or by period:

``` r
gr_dupl <- duplicate_by(data_rd_pr, conc, start = "period.start", end = "period.end", by_group = TRUE)
```

| identifier | dating.min | dating.max | period.start         | period.end           | period.source                | start.grpd     | end.grpd       | period         | fraction |
|:-----------|-----------:|-----------:|:---------------------|:---------------------|:-----------------------------|:---------------|:---------------|:---------------|---------:|
| Obj_11     |       -337 |       -313 | Early Roman Republic | Early Roman Republic | Derived from absolute dating | Roman Republic | Roman Republic | Roman Republic |      1.0 |
| Obj_12     |       -103 |        -24 | Late Roman Republic  | Early Roman Empire   | Derived from absolute dating | Roman Republic | Roman Empire   | Roman Republic |      0.5 |
| Obj_12     |       -103 |        -24 | Late Roman Republic  | Early Roman Empire   | Derived from absolute dating | Roman Republic | Roman Empire   | Roman Empire   |      0.5 |
| Obj_13     |        -50 |         27 | Late Roman Republic  | Early Roman Empire   | Derived from absolute dating | Roman Republic | Roman Empire   | Roman Republic |      0.5 |
| Obj_13     |        -50 |         27 | Late Roman Republic  | Early Roman Empire   | Derived from absolute dating | Roman Republic | Roman Empire   | Roman Empire   |      0.5 |
| Obj_14     |       -447 |       -445 | Early Roman Republic | Early Roman Republic | Derived from absolute dating | Roman Republic | Roman Republic | Roman Republic |      1.0 |
| Obj_15     |        -25 |         37 | Early Roman Empire   | Early Roman Empire   | Derived from absolute dating | Roman Empire   | Roman Empire   | Roman Empire   |      1.0 |
| Obj_16     |         49 |        104 | Early Roman Empire   | High Roman Empire    | Derived from absolute dating | Roman Empire   | Roman Empire   | Roman Empire   |      1.0 |
| Obj_17     |        -58 |        -15 | Late Roman Republic  | Early Roman Empire   | Derived from absolute dating | Roman Republic | Roman Empire   | Roman Republic |      0.5 |
| Obj_17     |        -58 |        -15 | Late Roman Republic  | Early Roman Empire   | Derived from absolute dating | Roman Republic | Roman Empire   | Roman Empire   |      0.5 |
| Obj_18     |       -309 |       -213 | Early Roman Republic | High Roman Republic  | Derived from absolute dating | Roman Republic | Roman Republic | Roman Republic |      1.0 |
| Obj_19     |       -313 |       -219 | Early Roman Republic | High Roman Republic  | Derived from absolute dating | Roman Republic | Roman Republic | Roman Republic |      1.0 |
| Obj_20     |          7 |         12 | Early Roman Empire   | Early Roman Empire   | Derived from absolute dating | Roman Empire   | Roman Empire   | Roman Empire   |      1.0 |

``` r
pr_dupl <- duplicate_by(data_rd_pr, conc, start = "period.start", end = "period.end", by_group = FALSE)
```

| identifier | dating.min | dating.max | period.start         | period.end           | period.source                | start.ungr           | end.ungr             | period               | fraction |
|:-----------|-----------:|-----------:|:---------------------|:---------------------|:-----------------------------|:---------------------|:---------------------|:---------------------|---------:|
| Obj_11     |       -337 |       -313 | Early Roman Republic | Early Roman Republic | Derived from absolute dating | Early Roman Republic | Early Roman Republic | Early Roman Republic |      1.0 |
| Obj_12     |       -103 |        -24 | Late Roman Republic  | Early Roman Empire   | Derived from absolute dating | Late Roman Republic  | Early Roman Empire   | Late Roman Republic  |      0.5 |
| Obj_12     |       -103 |        -24 | Late Roman Republic  | Early Roman Empire   | Derived from absolute dating | Late Roman Republic  | Early Roman Empire   | Early Roman Empire   |      0.5 |
| Obj_13     |        -50 |         27 | Late Roman Republic  | Early Roman Empire   | Derived from absolute dating | Late Roman Republic  | Early Roman Empire   | Late Roman Republic  |      0.5 |
| Obj_13     |        -50 |         27 | Late Roman Republic  | Early Roman Empire   | Derived from absolute dating | Late Roman Republic  | Early Roman Empire   | Early Roman Empire   |      0.5 |
| Obj_14     |       -447 |       -445 | Early Roman Republic | Early Roman Republic | Derived from absolute dating | Early Roman Republic | Early Roman Republic | Early Roman Republic |      1.0 |
| Obj_15     |        -25 |         37 | Early Roman Empire   | Early Roman Empire   | Derived from absolute dating | Early Roman Empire   | Early Roman Empire   | Early Roman Empire   |      1.0 |
| Obj_16     |         49 |        104 | Early Roman Empire   | High Roman Empire    | Derived from absolute dating | Early Roman Empire   | High Roman Empire    | Early Roman Empire   |      0.5 |
| Obj_16     |         49 |        104 | Early Roman Empire   | High Roman Empire    | Derived from absolute dating | Early Roman Empire   | High Roman Empire    | High Roman Empire    |      0.5 |
| Obj_17     |        -58 |        -15 | Late Roman Republic  | Early Roman Empire   | Derived from absolute dating | Late Roman Republic  | Early Roman Empire   | Late Roman Republic  |      0.5 |
| Obj_17     |        -58 |        -15 | Late Roman Republic  | Early Roman Empire   | Derived from absolute dating | Late Roman Republic  | Early Roman Empire   | Early Roman Empire   |      0.5 |
| Obj_18     |       -309 |       -213 | Early Roman Republic | High Roman Republic  | Derived from absolute dating | Early Roman Republic | High Roman Republic  | Early Roman Republic |      0.5 |
| Obj_18     |       -309 |       -213 | Early Roman Republic | High Roman Republic  | Derived from absolute dating | Early Roman Republic | High Roman Republic  | High Roman Republic  |      0.5 |
| Obj_19     |       -313 |       -219 | Early Roman Republic | High Roman Republic  | Derived from absolute dating | Early Roman Republic | High Roman Republic  | Early Roman Republic |      0.5 |
| Obj_19     |       -313 |       -219 | Early Roman Republic | High Roman Republic  | Derived from absolute dating | Early Roman Republic | High Roman Republic  | High Roman Republic  |      0.5 |
| Obj_20     |          7 |         12 | Early Roman Empire   | Early Roman Empire   | Derived from absolute dating | Early Roman Empire   | Early Roman Empire   | Early Roman Empire   |      1.0 |
