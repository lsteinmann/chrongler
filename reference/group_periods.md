# Replaces Periods with their Associated Grouped Versions

For each row in `data`, the group each period in the `start` and `end`
column belongs to is found. The groups are ordered factors as noted in
the concordance supplied to the `conc`
([`make_chrongler_conc()`](https://lsteinmann.github.io/chrongler/reference/make_chrongler_conc.md))
argument. Values that already are listed as groups are not changed.

## Usage

``` r
group_periods(data, conc, start, end)
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

## Value

The input `data` as a `data.frame`, with additional columns:

- `start.grpd` – *ordered factor* of the group the value in the start
  column belongs to.

- `end.grpd` – *ordered factor* of the group the value in the end column
  belongs to.

## See also

- [`ungroup_periods()`](https://lsteinmann.github.io/chrongler/reference/ungroup_periods.md)

## Examples

``` r
data("BuildingsMilet")
data("PeriodsMilet")
conc <- make_chrongler_conc(PeriodsMilet)
group_periods(
  BuildingsMilet,
  conc,
  start = "period.start",
  end = "period.end"
 )
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
#>          start.grpd        end.grpd
#> 1       Hellenistic       Byzantine
#> 2    Roman imperial  Roman imperial
#> 3       Hellenistic     Hellenistic
#> 4   Recent / Modern Recent / Modern
#> 5              <NA>            <NA>
#> 6              <NA>  Roman imperial
#> 7         Byzantine       Byzantine
#> 8    Roman imperial  Roman imperial
#> 9       Hellenistic     Hellenistic
#> 10   Roman imperial  Roman imperial
#> 11      Hellenistic  Roman imperial
#> 12        Byzantine       Byzantine
#> 13      Hellenistic       Byzantine
#> 14        Byzantine       Byzantine
#> 15   Late Antiquity  Late Antiquity
#> 16      Hellenistic     Hellenistic
#> 17   Roman imperial  Late Antiquity
#> 18      Hellenistic  Roman imperial
#> 19   Roman imperial  Roman imperial
#> 20   Roman imperial  Roman imperial
#> 21      Hellenistic     Hellenistic
#> 22   Roman imperial  Roman imperial
#> 23      Hellenistic     Hellenistic
#> 24   Roman imperial  Roman imperial
#> 25      Hellenistic     Hellenistic
#> 26   Roman imperial  Roman imperial
#> 27  Recent / Modern Recent / Modern
#> 28   Roman imperial  Roman imperial
#> 29      Hellenistic  Late Antiquity
#> 30      Hellenistic     Hellenistic
#> 31   Roman imperial  Late Antiquity
#> 32   Roman imperial       Byzantine
#> 33   Roman imperial  Roman imperial
#> 34   Roman imperial       Byzantine
#> 35        Byzantine       Byzantine
#> 36      Hellenistic     Hellenistic
#> 37   Roman imperial       Byzantine
#> 38      Hellenistic     Hellenistic
#> 39        Byzantine       Byzantine
#> 40        Byzantine       Byzantine
#> 41   Roman imperial  Roman imperial
#> 42   Roman imperial       Byzantine
#> 43   Roman imperial  Late Antiquity
#> 44             <NA>            <NA>
#> 45        Byzantine         Ottoman
#> 46        Byzantine       Byzantine
#> 47   Roman imperial  Roman imperial
#> 48   Roman imperial  Roman imperial
#> 49   Roman imperial  Late Antiquity
#> 50   Late Antiquity  Late Antiquity
#> 51             <NA>            <NA>
#> 52   Roman imperial  Roman imperial
#> 53      Hellenistic     Hellenistic
#> 54             <NA>            <NA>
#> 55      Hellenistic     Hellenistic
#> 56        Byzantine       Byzantine
#> 57        Byzantine       Byzantine
#> 58      Hellenistic  Roman imperial
#> 59        Classical     Hellenistic
#> 60      Hellenistic  Roman imperial
#> 61      Hellenistic  Late Antiquity
#> 62      Hellenistic  Roman imperial
#> 63      Hellenistic     Hellenistic
#> 64        Classical       Classical
#> 65   Roman imperial  Roman imperial
#> 66   Roman imperial  Roman imperial
#> 67        Byzantine       Byzantine
#> 68   Roman imperial       Byzantine
#> 69   Roman imperial  Roman imperial
#> 70   Roman imperial  Roman imperial
#> 71      Hellenistic  Roman imperial
#> 72        Classical  Roman imperial
#> 73             <NA>            <NA>
#> 74   Roman imperial  Roman imperial
#> 75  Recent / Modern Recent / Modern
#> 76        Byzantine         Ottoman
#> 77      Hellenistic     Hellenistic
#> 78      Hellenistic  Roman imperial
#> 79      Hellenistic     Hellenistic
#> 80   Roman imperial       Byzantine
#> 81      Hellenistic     Hellenistic
#> 82      Hellenistic     Hellenistic
#> 83   Roman imperial  Roman imperial
#> 84      Hellenistic     Hellenistic
#> 85   Roman imperial       Byzantine
#> 86      Hellenistic  Roman imperial
#> 87             <NA>            <NA>
#> 88      Hellenistic  Roman imperial
#> 89        Classical       Classical
#> 90      Hellenistic       Byzantine
#> 91      Hellenistic  Roman imperial
#> 92      Hellenistic  Roman imperial
#> 93      Hellenistic  Roman imperial
#> 94        Byzantine       Byzantine
#> 95        Classical       Classical
#> 96      Hellenistic  Roman imperial
#> 97   Roman imperial  Roman imperial
#> 98        Classical       Classical
#> 99      Hellenistic  Roman imperial
#> 100  Roman imperial  Roman imperial
#> 101     Hellenistic     Hellenistic
#> 102     Hellenistic     Hellenistic
#> 103  Roman imperial  Roman imperial
#> 104     Hellenistic  Roman imperial
#> 105       Byzantine       Byzantine
#> 106  Roman imperial  Roman imperial
#> 107  Roman imperial  Roman imperial
#> 108       Byzantine       Byzantine
#> 109 Recent / Modern Recent / Modern
#> 110       Classical  Roman imperial
```
