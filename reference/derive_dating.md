# Derive Dating from Periods

For each row in `data`, the value in the `start` and `end` column is
used to gather the absolute dating of its period from the concordance
object supplied to `conc`
([`make_chrongler_conc()`](https://lsteinmann.github.io/chrongler/reference/make_chrongler_conc.md)).
The absolute dating is stored in new `dating.min` and `dating.max`
columns. If `previous_min` and/or `previous_max` are supplied, the
values from those columns are preferred, and only empty (`NA`) cells are
filled with derived dates. A comment informing of the procedure is
written to a column called *dating.source*.

## Usage

``` r
derive_dating(data, conc, start, end, previous_min, previous_max)
```

## Arguments

- data:

  *data.frame*, *required* containing at least two columns with the
  start and end period for each row. Columns can be specified in the
  relevant arguments. The values in the start and end period need to
  correspond with the relevant values in the concordance used (see
  [`make_chrongler_conc()`](https://lsteinmann.github.io/chrongler/reference/make_chrongler_conc.md)).

- conc:

  *chrongler.conc*, *required*, as built by
  [`make_chrongler_conc()`](https://lsteinmann.github.io/chrongler/reference/make_chrongler_conc.md).

- start:

  *chr/int*, *required*. Name or index of the column with the **earliest
  period**.

- end:

  *chr/int*, *required*. Name or index of the column with the **latest
  period**.

- previous_min:

  *chr/int*, *optional*. Name or index of a pre-existing column
  containing absolute **minimum dating**. If supplied, previously empty
  values (`NA`) will be populated in the new *dating.min* column. A
  comment is stored in a column called *dating.source*.

- previous_max:

  *chr/int*, *optional*. Name or index of a pre-existing column
  containing absolute **maximum dating**. If supplied, previously empty
  values (`NA`) will be populated in the new *dating.max* column. A
  comment is stored in a column called *dating.source*.

## Value

The input `data` as a `data.frame` with three additional columns:

- `dating.min` — numeric, the earliest absolute date derived from the
  period in `start` (or copied from `previous_min` where available)

- `dating.max` — numeric, the latest absolute date derived from the
  period in `end` (or copied from `previous_max` where available)

- `dating.source` — character, a comment indicating how the dating was
  derived (`"Derived from period"`, `"Partially derived from period"`,
  or `NA` if no derivation was necessary)

## See also

- [`make_chrongler_conc()`](https://lsteinmann.github.io/chrongler/reference/make_chrongler_conc.md)

- [`derive_period()`](https://lsteinmann.github.io/chrongler/reference/derive_period.md)

## Examples

``` r
data("BuildingsMilet")
data("PeriodsMilet")
conc <- make_chrongler_conc(PeriodsMilet)

derive_dating(BuildingsMilet, conc, start = "period.start", end = "period.end")
#>                                                            identifier
#> 1                                Stadtmauer mit Hafentor am Humeitepe
#> 2                                            Tabernae am Athenatempel
#> 3                                Delphinion - hellenistischer Umbau I
#> 4                                                    von-Gerkan-Mauer
#> 5                                        Insula QR/18-19 Byzantinisch
#> 6                                                     Insula KL/12-13
#> 7                                                          Rundkirche
#> 8                                             Apsidenbau mit Propylon
#> 9                                                   Pi-foermige Halle
#> 10                                               Thermen am Westmarkt
#> 11                                              Grosses Hafenmonument
#> 12  Propylon des Serapistempels - Nutzung als byzantinisches Stadttor
#> 13                                             Stadtmauer am Kaletepe
#> 14                                                       Tempelkirche
#> 15                                                   Doppelapsidenbau
#> 16                                          Magazinhalle am Suedmarkt
#> 17                                  Graeber unter der Friedhofskirche
#> 18                                                          Nordmarkt
#> 19                                                         Heroon III
#> 20                                                Nymphaeumsaquaedukt
#> 21                          Hellenistisches Gebaeude unter Heroon III
#> 22                                                 Temenos mit Tempel
#> 23                     Stadtmauer - Suedliche Quermauer, erste Anlage
#> 24                                        Hallenbau am sog. Nordmarkt
#> 25                                                           Tempel A
#> 26                                                Suedmarkt - Latrine
#> 27                                                Eski Balat Gebaeude
#> 28                                                     Ionische Halle
#> 29      Stadtmauer - Suedliche Quermauer, spaethellenistischer Ausbau
#> 30                                           Hellenistisches Peristyl
#> 31                                                    Faustinathermen
#> 32                                                         Stadiontor
#> 33                                                  Thermen am Museum
#> 34                                               Hafenhalle - Latrine
#> 35                                                    Friedhofskirche
#> 36                                                Kleiner Hallenplatz
#> 37                                        Propylon des Serapistempels
#> 38                                                          Westmarkt
#> 39                                                     Theaterkastell
#> 40                                           Befestigung des Kaletepe
#> 41                                              Basilika am Suedmarkt
#> 42                                     Befestigungsmauer am Humeitepe
#> 43                                                          Heroon II
#> 44                                 Insula QR/18-19 Spaethellenistisch
#> 45                               Toepferwerkstatt beim Bischofspalast
#> 46                                            Byzantinischer Wohnturm
#> 47                                          Peristylhaus am Suedmarkt
#> 48                                 Roemisches Hofhaus am Athenatempel
#> 49                                                     Thermenkastell
#> 50                                                           Synagoge
#> 51                                         Insula QR/18-19 Spaetantik
#> 52                                                           Tempel B
#> 53                                    Delphinion - Fruehhellenistisch
#> 54                                          Insula QR/18-19 Klassisch
#> 55                                           Altar des Demetertempels
#> 56                                           Byzantinische Stadtmauer
#> 57                                                      Grosse Kirche
#> 58                                                  Dionysosheiligtum
#> 59                                              Aelteres heiliges Tor
#> 60                               Kaianlagen am suedlichen Loewenhafen
#> 61                                                          Loewentor
#> 62                                              Stadtmauer - Ostmauer
#> 63                                                      Demetertempel
#> 64                                          Wohnhaus FG/24-25 Phase 2
#> 65                                                    Osthafenthermen
#> 66                                          Magazinbau am Loewenhafen
#> 67                                                 Suedstadt-Basilika
#> 68                                                    Insula DE/25-26
#> 69                                               Thermen am Humeitepe
#> 70                                                          Nymphaeum
#> 71                                                           Heroon I
#> 72                                             Juengeres heiliges Tor
#> 73                                         Insula QR/18-19 Kaiserzeit
#> 74                                              Kleines Hafenmonument
#> 75                                           Brunnen in der Suedstadt
#> 76                                               Vier-Saeulen-Moschee
#> 77                                           Hellenistische Hofanlage
#> 78                                                       Bouleuterion
#> 79                                                        Antentempel
#> 80                                        Delphinion - Kaiserzeitlich
#> 81                                                         Hafenmauer
#> 82                                             Heiligtum am Nordmarkt
#> 83  Stadtmauer - Suedliche Quermauer, Wiederherstellung der Gotenzeit
#> 84                              Delphinion - hellenistischer Umbau II
#> 85                               Peristylhaus unter dem Bischospalast
#> 86                                        Suedmarkt mit Antiochosstoa
#> 87                                 Insula QR/18-19 Fruehhellenistisch
#> 88                                     Altar auf der Heiligen Strasse
#> 89                                          Wohnhaus FG/24-25 Phase 1
#> 90                                                 Markttor von Milet
#> 91                                                         Hafenhalle
#> 92                                                            Stadion
#> 93                                                            Theater
#> 94                                                     Bischofspalast
#> 95                           Wohngebaeude unter dem Dionysosheiligtum
#> 96                               Magazinhalle am Suedmarkt (Rueckbau)
#> 97                                                   Suedstadtthermen
#> 98                                       Delphinion - Fruehklassissch
#> 99                                                      Antiochosstoa
#> 100                                                     Capitothermen
#> 101                                                 Eumenes-Gymnasion
#> 102                                         Wohnhaus FG/24-25 Phase 3
#> 103                                                          Hafentor
#> 104                              Bau im Peristylhof des Bouleuterions
#> 105                            Spaetantike Platzanlage am Kalabaktepe
#> 106                 Gebaeude nordwestlich des Hafentores am Humeitepe
#> 107                                                         Serapeion
#> 108                                                    Michaelskirche
#> 109                                                            Museum
#> 110                                       Stadtmauer um den Humeitepe
#>                         period.start                       period.end
#> 1                   Late Hellenistic Early Byzantine / Late Antiquity
#> 2                     Roman imperial                   Roman imperial
#> 3                        Hellenistic                      Hellenistic
#> 4                    Recent / Modern                  Recent / Modern
#> 5                               <NA>                             <NA>
#> 6                               <NA>                   Roman imperial
#> 7                          Byzantine                        Byzantine
#> 8                     Roman imperial                   Roman imperial
#> 9                        Hellenistic                      Hellenistic
#> 10                   Middle Imperial                  Middle Imperial
#> 11                  Late Hellenistic                   Early imperial
#> 12                         Byzantine                        Byzantine
#> 13                       Hellenistic Early Byzantine / Late Antiquity
#> 14  Early Byzantine / Late Antiquity Early Byzantine / Late Antiquity
#> 15                    Late Antiquity                   Late Antiquity
#> 16                 Early hellenistic                Early hellenistic
#> 17                   Middle Imperial                   Late Antiquity
#> 18                 Early hellenistic                   Roman imperial
#> 19                    Roman imperial                   Roman imperial
#> 20                    Early imperial                   Early imperial
#> 21                       Hellenistic                      Hellenistic
#> 22                    Roman imperial                   Roman imperial
#> 23                 Early hellenistic                 Late Hellenistic
#> 24                    Roman imperial                   Roman imperial
#> 25                       Hellenistic                      Hellenistic
#> 26                     Late Imperial                    Late Imperial
#> 27                   Recent / Modern                  Recent / Modern
#> 28                    Roman imperial                   Roman imperial
#> 29                  Late Hellenistic                   Late Antiquity
#> 30                       Hellenistic                      Hellenistic
#> 31                    Roman imperial                   Late Antiquity
#> 32                     Late Imperial Early Byzantine / Late Antiquity
#> 33                     Late Imperial                    Late Imperial
#> 34                     Late Imperial                        Byzantine
#> 35                         Byzantine                        Byzantine
#> 36                 Early hellenistic                Early hellenistic
#> 37                   Middle Imperial                        Byzantine
#> 38                       Hellenistic                      Hellenistic
#> 39                    Late Byzantine                   Late Byzantine
#> 40                    Late Byzantine                   Late Byzantine
#> 41                    Roman imperial                   Roman imperial
#> 42                     Late Imperial Early Byzantine / Late Antiquity
#> 43                   Middle Imperial                   Late Antiquity
#> 44                              <NA>                             <NA>
#> 45                    Late Byzantine                          Ottoman
#> 46                         Byzantine                        Byzantine
#> 47                    Roman imperial                   Roman imperial
#> 48                    Roman imperial                   Roman imperial
#> 49                     Late Imperial                   Late Antiquity
#> 50                    Late Antiquity                   Late Antiquity
#> 51                              <NA>                             <NA>
#> 52                    Roman imperial                   Roman imperial
#> 53                 Early hellenistic                Early hellenistic
#> 54                              <NA>                             <NA>
#> 55                       Hellenistic                      Hellenistic
#> 56  Early Byzantine / Late Antiquity Early Byzantine / Late Antiquity
#> 57  Early Byzantine / Late Antiquity Early Byzantine / Late Antiquity
#> 58                       Hellenistic                   Roman imperial
#> 59                         Classical                      Hellenistic
#> 60                       Hellenistic                   Roman imperial
#> 61                       Hellenistic                   Late Antiquity
#> 62                 Early hellenistic                    Late Imperial
#> 63                       Hellenistic                      Hellenistic
#> 64                    Late Classical                   Late Classical
#> 65                    Roman imperial                   Roman imperial
#> 66                   Middle Imperial                  Middle Imperial
#> 67                         Byzantine                        Byzantine
#> 68                    Early imperial Early Byzantine / Late Antiquity
#> 69                   Middle Imperial                  Middle Imperial
#> 70                    Roman imperial                   Roman imperial
#> 71                       Hellenistic                  Middle Imperial
#> 72                    Late Classical                   Roman imperial
#> 73                              <NA>                             <NA>
#> 74                    Early imperial                   Early imperial
#> 75                   Recent / Modern                  Recent / Modern
#> 76                    Late Byzantine                          Ottoman
#> 77                       Hellenistic                      Hellenistic
#> 78                       Hellenistic                   Roman imperial
#> 79                       Hellenistic                      Hellenistic
#> 80                    Roman imperial Early Byzantine / Late Antiquity
#> 81                       Hellenistic                      Hellenistic
#> 82                       Hellenistic                      Hellenistic
#> 83                     Late Imperial                    Late Imperial
#> 84                  Late Hellenistic                 Late Hellenistic
#> 85                     Late Imperial Early Byzantine / Late Antiquity
#> 86                       Hellenistic                   Roman imperial
#> 87                              <NA>                             <NA>
#> 88                       Hellenistic                   Early imperial
#> 89                   Early classical                  Early classical
#> 90                       Hellenistic                        Byzantine
#> 91                 Early hellenistic                   Roman imperial
#> 92                       Hellenistic                   Roman imperial
#> 93                       Hellenistic                   Roman imperial
#> 94  Early Byzantine / Late Antiquity Early Byzantine / Late Antiquity
#> 95                         Classical                        Classical
#> 96                  Late Hellenistic                   Roman imperial
#> 97                     Late Imperial                    Late Imperial
#> 98                   Early classical                   Late Classical
#> 99                       Hellenistic                   Roman imperial
#> 100                   Early imperial                   Early imperial
#> 101                      Hellenistic                      Hellenistic
#> 102                      Hellenistic                      Hellenistic
#> 103                   Roman imperial                   Roman imperial
#> 104                 Late Hellenistic                   Early imperial
#> 105 Early Byzantine / Late Antiquity Early Byzantine / Late Antiquity
#> 106                   Roman imperial                   Roman imperial
#> 107                   Roman imperial                   Roman imperial
#> 108                        Byzantine                        Byzantine
#> 109                  Recent / Modern                  Recent / Modern
#> 110                   Late Classical                    Late Imperial
#>     dating.min dating.max       dating.source
#> 1         -177        699 Derived from period
#> 2          -31        305 Derived from period
#> 3         -323        -32 Derived from period
#> 4         1900       2023 Derived from period
#> 5           NA         NA Derived from period
#> 6           NA        305 Derived from period
#> 7          306       1299 Derived from period
#> 8          -31        305 Derived from period
#> 9         -323        -32 Derived from period
#> 10         100        192 Derived from period
#> 11        -177         98 Derived from period
#> 12         306       1299 Derived from period
#> 13        -323        699 Derived from period
#> 14         306        699 Derived from period
#> 15         306        699 Derived from period
#> 16        -323       -178 Derived from period
#> 17         100        699 Derived from period
#> 18        -323        305 Derived from period
#> 19         -31        305 Derived from period
#> 20         -31         98 Derived from period
#> 21        -323        -32 Derived from period
#> 22         -31        305 Derived from period
#> 23        -323        -32 Derived from period
#> 24         -31        305 Derived from period
#> 25        -323        -32 Derived from period
#> 26         193        305 Derived from period
#> 27        1900       2023 Derived from period
#> 28         -31        305 Derived from period
#> 29        -177        699 Derived from period
#> 30        -323        -32 Derived from period
#> 31         -31        699 Derived from period
#> 32         193        699 Derived from period
#> 33         193        305 Derived from period
#> 34         193       1299 Derived from period
#> 35         306       1299 Derived from period
#> 36        -323       -178 Derived from period
#> 37         100       1299 Derived from period
#> 38        -323        -32 Derived from period
#> 39        1000       1299 Derived from period
#> 40        1000       1299 Derived from period
#> 41         -31        305 Derived from period
#> 42         193        699 Derived from period
#> 43         100        699 Derived from period
#> 44          NA         NA Derived from period
#> 45        1000       1899 Derived from period
#> 46         306       1299 Derived from period
#> 47         -31        305 Derived from period
#> 48         -31        305 Derived from period
#> 49         193        699 Derived from period
#> 50         306        699 Derived from period
#> 51          NA         NA Derived from period
#> 52         -31        305 Derived from period
#> 53        -323       -178 Derived from period
#> 54          NA         NA Derived from period
#> 55        -323        -32 Derived from period
#> 56         306        699 Derived from period
#> 57         306        699 Derived from period
#> 58        -323        305 Derived from period
#> 59        -480        -32 Derived from period
#> 60        -323        305 Derived from period
#> 61        -323        699 Derived from period
#> 62        -323        305 Derived from period
#> 63        -323        -32 Derived from period
#> 64        -425       -324 Derived from period
#> 65         -31        305 Derived from period
#> 66         100        192 Derived from period
#> 67         306       1299 Derived from period
#> 68         -31        699 Derived from period
#> 69         100        192 Derived from period
#> 70         -31        305 Derived from period
#> 71        -323        192 Derived from period
#> 72        -425        305 Derived from period
#> 73          NA         NA Derived from period
#> 74         -31         98 Derived from period
#> 75        1900       2023 Derived from period
#> 76        1000       1899 Derived from period
#> 77        -323        -32 Derived from period
#> 78        -323        305 Derived from period
#> 79        -323        -32 Derived from period
#> 80         -31        699 Derived from period
#> 81        -323        -32 Derived from period
#> 82        -323        -32 Derived from period
#> 83         193        305 Derived from period
#> 84        -177        -32 Derived from period
#> 85         193        699 Derived from period
#> 86        -323        305 Derived from period
#> 87          NA         NA Derived from period
#> 88        -323         98 Derived from period
#> 89        -480       -426 Derived from period
#> 90        -323       1299 Derived from period
#> 91        -323        305 Derived from period
#> 92        -323        305 Derived from period
#> 93        -323        305 Derived from period
#> 94         306        699 Derived from period
#> 95        -480       -324 Derived from period
#> 96        -177        305 Derived from period
#> 97         193        305 Derived from period
#> 98        -480       -324 Derived from period
#> 99        -323        305 Derived from period
#> 100        -31         98 Derived from period
#> 101       -323        -32 Derived from period
#> 102       -323        -32 Derived from period
#> 103        -31        305 Derived from period
#> 104       -177         98 Derived from period
#> 105        306        699 Derived from period
#> 106        -31        305 Derived from period
#> 107        -31        305 Derived from period
#> 108        306       1299 Derived from period
#> 109       1900       2023 Derived from period
#> 110       -425        305 Derived from period
```
