# Derive Period from Absolute Dates

For each row in `data`, the value in the `min` and `max` column is used
to identify the period(s) the respective row would belong to according
to the dating ranges registered in the the concordance object supplied
to `conc`
([`make_chrongler_conc()`](https://lsteinmann.github.io/chrongler/reference/make_chrongler_conc.md)).
The newly assigned periods are stored in column names `period.start` and
`period.end`. If `previous_start` and/or `previous_end` are supplied,
the values from those columns are preferred, and only empty (`NA`) cells
are filled with derived periods. A comment informing of the procedure is
written to a column called *period.source*.

## Usage

``` r
derive_period(data, conc, min, max, previous_start, previous_end)
```

## Arguments

- data:

  A data.frame containing at least two columns with the minimum and
  maximum absolute dating for each row as integer. The values need to
  correspond with the possible dating ranges present in the concordance
  used (see
  [`make_chrongler_conc()`](https://lsteinmann.github.io/chrongler/reference/make_chrongler_conc.md)).

- conc:

  *chrongler.conc*, *required*, as built by
  [`make_chrongler_conc()`](https://lsteinmann.github.io/chrongler/reference/make_chrongler_conc.md).

- min:

  *chr/int*. Name or index of the column with the **minimum dating**
  (integers representing years).

- max:

  *chr/int*. Name or index of the column with the **maximum dating**
  (integers representing years).

- previous_start:

  *chr/int*, *optional*. Name or index of a pre-existing column
  containing a **starting period**. If supplied, previously empty values
  (`NA`) will be populated in the new *period.start* column. A comment
  is stored in a column called *period.source*.

- previous_end:

  *chr/int*, *optional*. Name or index of a pre-existing column
  containing a **ending period**. If supplied, previously empty values
  (`NA`) will be populated in the new *period.end* column. A comment is
  stored in a column called *period.source*.

## Value

The input `data` as a `data.frame` with three additional columns:

- `period.start` – character, the period the date in `min` would fall in
  (or copied from `previous_start` where available)

- `period.end` – character, the period the date in `max` would fall in
  (or copied from `previous_end` where available)

- `period.source` – character, a comment indicating how the period was
  derived (`"Derived from absolute dating"`,
  `"Partially derived from absolute dating"`, or `NA` if no derivation
  was necessary)

## See also

- [`make_chrongler_conc()`](https://lsteinmann.github.io/chrongler/reference/make_chrongler_conc.md)

- [`derive_dating()`](https://lsteinmann.github.io/chrongler/reference/derive_dating.md)

## Examples

``` r
conc <- make_chrongler_conc(RomanPeriods)

data <- data.frame(name = 1:110)

data$min <- sample(seq(-509, 476, by = 1), 110, replace = TRUE)
data$max <- data$min + sample(abs(round(rnorm(1000, 200, 100))), 110, replace = TRUE)

derive_period(data, conc, min = "min", max = "max")
#>     name  min  max         period.start           period.end
#> 1      1 -337 -277 Early Roman Republic Early Roman Republic
#> 2      2 -103   27  Late Roman Republic   Early Roman Empire
#> 3      3  -50  196  Late Roman Republic    High Roman Empire
#> 4      4 -447 -327 Early Roman Republic Early Roman Republic
#> 5      5  -25  155   Early Roman Empire    High Roman Empire
#> 6      6  360  412    Late Roman Empire    Late Roman Empire
#> 7      7  369  714    Late Roman Empire                 <NA>
#> 8      8   49  242   Early Roman Empire    Late Roman Empire
#> 9      9  417  481    Late Roman Empire                 <NA>
#> 10    10  -58   60  Late Roman Republic   Early Roman Empire
#> 11    11 -309  -55 Early Roman Republic  Late Roman Republic
#> 12    12 -313 -119 Early Roman Republic  Late Roman Republic
#> 13    13    7  176   Early Roman Empire    High Roman Empire
#> 14    14  257  365    Late Roman Empire    Late Roman Empire
#> 15    15   26  147   Early Roman Empire    High Roman Empire
#> 16    16  337  582    Late Roman Empire                 <NA>
#> 17    17 -433 -166 Early Roman Republic  High Roman Republic
#> 18    18  388  633    Late Roman Empire                 <NA>
#> 19    19  -64   50  Late Roman Republic   Early Roman Empire
#> 20    20   57  215   Early Roman Empire    High Roman Empire
#> 21    21  -83   49  Late Roman Republic   Early Roman Empire
#> 22    22 -448 -268 Early Roman Republic Early Roman Republic
#> 23    23   45  240   Early Roman Empire    Late Roman Empire
#> 24    24  263  424    Late Roman Empire    Late Roman Empire
#> 25    25  -41  107  Late Roman Republic    High Roman Empire
#> 26    26 -210   -2  High Roman Republic   Early Roman Empire
#> 27    27  447  611    Late Roman Empire                 <NA>
#> 28    28   36  263   Early Roman Empire    Late Roman Empire
#> 29    29 -440 -229 Early Roman Republic  High Roman Republic
#> 30    30  -63   25  Late Roman Republic   Early Roman Empire
#> 31    31 -250 -237  High Roman Republic  High Roman Republic
#> 32    32  242  565    Late Roman Empire                 <NA>
#> 33    33  164  257    High Roman Empire    Late Roman Empire
#> 34    34  165  306    High Roman Empire    Late Roman Empire
#> 35    35 -293  -34 Early Roman Republic  Late Roman Republic
#> 36    36  216  392    High Roman Empire    Late Roman Empire
#> 37    37   45  249   Early Roman Empire    Late Roman Empire
#> 38    38  393  527    Late Roman Empire                 <NA>
#> 39    39   34  229   Early Roman Empire    High Roman Empire
#> 40    40  439  588    Late Roman Empire                 <NA>
#> 41    41 -440 -234 Early Roman Republic  High Roman Republic
#> 42    42 -245   61  High Roman Republic   Early Roman Empire
#> 43    43 -360  -59 Early Roman Republic  Late Roman Republic
#> 44    44 -244   11  High Roman Republic   Early Roman Empire
#> 45    45  -94  181  Late Roman Republic    High Roman Empire
#> 46    46 -393  -98 Early Roman Republic  Late Roman Republic
#> 47    47  420  543    Late Roman Empire                 <NA>
#> 48    48   19  282   Early Roman Empire    Late Roman Empire
#> 49    49 -330 -292 Early Roman Republic Early Roman Republic
#> 50    50 -326  -37 Early Roman Republic  Late Roman Republic
#> 51    51 -488 -173 Early Roman Republic  High Roman Republic
#> 52    52   97  323    High Roman Empire    Late Roman Empire
#> 53    53  387  639    Late Roman Empire                 <NA>
#> 54    54 -440 -187 Early Roman Republic  High Roman Republic
#> 55    55  164  417    High Roman Empire    Late Roman Empire
#> 56    56 -331 -222 Early Roman Republic  High Roman Republic
#> 57    57  176  306    High Roman Empire    Late Roman Empire
#> 58    58  323  453    Late Roman Empire    Late Roman Empire
#> 59    59   27  281   Early Roman Empire    Late Roman Empire
#> 60    60  -15  323   Early Roman Empire    Late Roman Empire
#> 61    61  -12  306   Early Roman Empire    Late Roman Empire
#> 62    62 -345 -260 Early Roman Republic  High Roman Republic
#> 63    63 -246   43  High Roman Republic   Early Roman Empire
#> 64    64  163  500    High Roman Empire                 <NA>
#> 65    65  226  456    High Roman Empire    Late Roman Empire
#> 66    66  366  466    Late Roman Empire    Late Roman Empire
#> 67    67  341  601    Late Roman Empire                 <NA>
#> 68    68   94  320    High Roman Empire    Late Roman Empire
#> 69    69  286  618    Late Roman Empire                 <NA>
#> 70    70  306  475    Late Roman Empire    Late Roman Empire
#> 71    71  333  536    Late Roman Empire                 <NA>
#> 72    72  476  488    Late Roman Empire                 <NA>
#> 73    73 -219  -24  High Roman Republic   Early Roman Empire
#> 74    74 -380 -150 Early Roman Republic  High Roman Republic
#> 75    75  345  459    Late Roman Empire    Late Roman Empire
#> 76    76  364  566    Late Roman Empire                 <NA>
#> 77    77  133  286    High Roman Empire    Late Roman Empire
#> 78    78  296  709    Late Roman Empire                 <NA>
#> 79    79  -58  113  Late Roman Republic    High Roman Empire
#> 80    80  236  395    Late Roman Empire    Late Roman Empire
#> 81    81   67  150   Early Roman Empire    High Roman Empire
#> 82    82  233  295    High Roman Empire    Late Roman Empire
#> 83    83  392  580    Late Roman Empire                 <NA>
#> 84    84  152  243    High Roman Empire    Late Roman Empire
#> 85    85  149  318    High Roman Empire    Late Roman Empire
#> 86    86  260  447    Late Roman Empire    Late Roman Empire
#> 87    87  322  430    Late Roman Empire    Late Roman Empire
#> 88    88   42  251   Early Roman Empire    Late Roman Empire
#> 89    89 -317 -111 Early Roman Republic  Late Roman Republic
#> 90    90 -354 -149 Early Roman Republic  High Roman Republic
#> 91    91  -55  217  Late Roman Republic    High Roman Empire
#> 92    92 -498 -288 Early Roman Republic Early Roman Republic
#> 93    93  -72   77  Late Roman Republic    High Roman Empire
#> 94    94  299  457    Late Roman Empire    Late Roman Empire
#> 95    95 -468  -88 Early Roman Republic  Late Roman Republic
#> 96    96   68  315   Early Roman Empire    Late Roman Empire
#> 97    97 -408 -191 Early Roman Republic  High Roman Republic
#> 98    98  361  630    Late Roman Empire                 <NA>
#> 99    99  284  573    Late Roman Empire                 <NA>
#> 100  100  318  377    Late Roman Empire    Late Roman Empire
#> 101  101   -8  164   Early Roman Empire    High Roman Empire
#> 102  102 -240  -42  High Roman Republic  Late Roman Republic
#> 103  103  263  482    Late Roman Empire                 <NA>
#> 104  104  -23   63   Early Roman Empire   Early Roman Empire
#> 105  105 -139  184  Late Roman Republic    High Roman Empire
#> 106  106  462  580    Late Roman Empire                 <NA>
#> 107  107  243  518    Late Roman Empire                 <NA>
#> 108  108  359  697    Late Roman Empire                 <NA>
#> 109  109  275  589    Late Roman Empire                 <NA>
#> 110  110 -497 -123 Early Roman Republic  Late Roman Republic
#>                    period.source
#> 1   Derived from absolute dating
#> 2   Derived from absolute dating
#> 3   Derived from absolute dating
#> 4   Derived from absolute dating
#> 5   Derived from absolute dating
#> 6   Derived from absolute dating
#> 7   Derived from absolute dating
#> 8   Derived from absolute dating
#> 9   Derived from absolute dating
#> 10  Derived from absolute dating
#> 11  Derived from absolute dating
#> 12  Derived from absolute dating
#> 13  Derived from absolute dating
#> 14  Derived from absolute dating
#> 15  Derived from absolute dating
#> 16  Derived from absolute dating
#> 17  Derived from absolute dating
#> 18  Derived from absolute dating
#> 19  Derived from absolute dating
#> 20  Derived from absolute dating
#> 21  Derived from absolute dating
#> 22  Derived from absolute dating
#> 23  Derived from absolute dating
#> 24  Derived from absolute dating
#> 25  Derived from absolute dating
#> 26  Derived from absolute dating
#> 27  Derived from absolute dating
#> 28  Derived from absolute dating
#> 29  Derived from absolute dating
#> 30  Derived from absolute dating
#> 31  Derived from absolute dating
#> 32  Derived from absolute dating
#> 33  Derived from absolute dating
#> 34  Derived from absolute dating
#> 35  Derived from absolute dating
#> 36  Derived from absolute dating
#> 37  Derived from absolute dating
#> 38  Derived from absolute dating
#> 39  Derived from absolute dating
#> 40  Derived from absolute dating
#> 41  Derived from absolute dating
#> 42  Derived from absolute dating
#> 43  Derived from absolute dating
#> 44  Derived from absolute dating
#> 45  Derived from absolute dating
#> 46  Derived from absolute dating
#> 47  Derived from absolute dating
#> 48  Derived from absolute dating
#> 49  Derived from absolute dating
#> 50  Derived from absolute dating
#> 51  Derived from absolute dating
#> 52  Derived from absolute dating
#> 53  Derived from absolute dating
#> 54  Derived from absolute dating
#> 55  Derived from absolute dating
#> 56  Derived from absolute dating
#> 57  Derived from absolute dating
#> 58  Derived from absolute dating
#> 59  Derived from absolute dating
#> 60  Derived from absolute dating
#> 61  Derived from absolute dating
#> 62  Derived from absolute dating
#> 63  Derived from absolute dating
#> 64  Derived from absolute dating
#> 65  Derived from absolute dating
#> 66  Derived from absolute dating
#> 67  Derived from absolute dating
#> 68  Derived from absolute dating
#> 69  Derived from absolute dating
#> 70  Derived from absolute dating
#> 71  Derived from absolute dating
#> 72  Derived from absolute dating
#> 73  Derived from absolute dating
#> 74  Derived from absolute dating
#> 75  Derived from absolute dating
#> 76  Derived from absolute dating
#> 77  Derived from absolute dating
#> 78  Derived from absolute dating
#> 79  Derived from absolute dating
#> 80  Derived from absolute dating
#> 81  Derived from absolute dating
#> 82  Derived from absolute dating
#> 83  Derived from absolute dating
#> 84  Derived from absolute dating
#> 85  Derived from absolute dating
#> 86  Derived from absolute dating
#> 87  Derived from absolute dating
#> 88  Derived from absolute dating
#> 89  Derived from absolute dating
#> 90  Derived from absolute dating
#> 91  Derived from absolute dating
#> 92  Derived from absolute dating
#> 93  Derived from absolute dating
#> 94  Derived from absolute dating
#> 95  Derived from absolute dating
#> 96  Derived from absolute dating
#> 97  Derived from absolute dating
#> 98  Derived from absolute dating
#> 99  Derived from absolute dating
#> 100 Derived from absolute dating
#> 101 Derived from absolute dating
#> 102 Derived from absolute dating
#> 103 Derived from absolute dating
#> 104 Derived from absolute dating
#> 105 Derived from absolute dating
#> 106 Derived from absolute dating
#> 107 Derived from absolute dating
#> 108 Derived from absolute dating
#> 109 Derived from absolute dating
#> 110 Derived from absolute dating
```
