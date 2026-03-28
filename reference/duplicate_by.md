# Duplicate each Row for each Period in the Range from start to end

For each period one object may be placed in according to the range
provided by the `start` and `end` columns of the data.frame `data`, the
associated row is duplicated as many times as possible periods the
objects might be placed in according to the concordance supplied to
`conc`
([`make_chrongler_conc()`](https://lsteinmann.github.io/chrongler/reference/make_chrongler_conc.md)).
This way, each objects is represented by multiple rows in the result! A
`fraction` column tracks the resulting *fraction* or weight each row
represents in regards to the object.

## Usage

``` r
duplicate_by(data, conc, start, end, by_group)
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

- by_group:

  *TRUE/FALSE*, *required*. Should the rows be duplicated along period
  groups? If `TRUE`: all rows are duplicated according to their grouped
  periods (see
  [`group_periods()`](https://lsteinmann.github.io/chrongler/reference/group_periods.md)).
  If `FALSE`: all rows are duplicated according to their single periods
  (see
  [`ungroup_periods()`](https://lsteinmann.github.io/chrongler/reference/ungroup_periods.md)).

## Value

The input `data` as a `data.frame`, with duplicated rows and additional
columns:

- `period` – *ordered factor* of the period represented by this row.

- `fraction` – *numeric* value of 1 divided by the number of periods an
  object can belong to: the fraction of each object the row can
  represent.

- Additional columns produced by
  [`group_periods()`](https://lsteinmann.github.io/chrongler/reference/group_periods.md)
  or
  [`ungroup_periods()`](https://lsteinmann.github.io/chrongler/reference/ungroup_periods.md)

## See also

- [`group_periods()`](https://lsteinmann.github.io/chrongler/reference/group_periods.md)

- [`ungroup_periods()`](https://lsteinmann.github.io/chrongler/reference/ungroup_periods.md)

## Examples

``` r
data("BuildingsMilet")
data("PeriodsMilet")
conc <- make_chrongler_conc(PeriodsMilet)

duplicate_by(BuildingsMilet, conc,
             start = "period.start", end = "period.end",
             by_group = FALSE)
#>                                                            identifier
#> 1                                Stadtmauer mit Hafentor am Humeitepe
#> 2                                Stadtmauer mit Hafentor am Humeitepe
#> 3                                Stadtmauer mit Hafentor am Humeitepe
#> 4                                Stadtmauer mit Hafentor am Humeitepe
#> 5                                Stadtmauer mit Hafentor am Humeitepe
#> 6                                Stadtmauer mit Hafentor am Humeitepe
#> 7                                            Tabernae am Athenatempel
#> 8                                            Tabernae am Athenatempel
#> 9                                            Tabernae am Athenatempel
#> 10                               Delphinion - hellenistischer Umbau I
#> 11                               Delphinion - hellenistischer Umbau I
#> 12                                                   von-Gerkan-Mauer
#> 13                                       Insula QR/18-19 Byzantinisch
#> 14                                                    Insula KL/12-13
#> 15                                                         Rundkirche
#> 16                                                         Rundkirche
#> 17                                                         Rundkirche
#> 18                                            Apsidenbau mit Propylon
#> 19                                            Apsidenbau mit Propylon
#> 20                                            Apsidenbau mit Propylon
#> 21                                                  Pi-foermige Halle
#> 22                                                  Pi-foermige Halle
#> 23                                               Thermen am Westmarkt
#> 24                                              Grosses Hafenmonument
#> 25                                              Grosses Hafenmonument
#> 26  Propylon des Serapistempels - Nutzung als byzantinisches Stadttor
#> 27  Propylon des Serapistempels - Nutzung als byzantinisches Stadttor
#> 28  Propylon des Serapistempels - Nutzung als byzantinisches Stadttor
#> 29                                             Stadtmauer am Kaletepe
#> 30                                             Stadtmauer am Kaletepe
#> 31                                             Stadtmauer am Kaletepe
#> 32                                             Stadtmauer am Kaletepe
#> 33                                             Stadtmauer am Kaletepe
#> 34                                             Stadtmauer am Kaletepe
#> 35                                             Stadtmauer am Kaletepe
#> 36                                                       Tempelkirche
#> 37                                                   Doppelapsidenbau
#> 38                                          Magazinhalle am Suedmarkt
#> 39                                  Graeber unter der Friedhofskirche
#> 40                                  Graeber unter der Friedhofskirche
#> 41                                  Graeber unter der Friedhofskirche
#> 42                                                          Nordmarkt
#> 43                                                          Nordmarkt
#> 44                                                          Nordmarkt
#> 45                                                          Nordmarkt
#> 46                                                          Nordmarkt
#> 47                                                         Heroon III
#> 48                                                         Heroon III
#> 49                                                         Heroon III
#> 50                                                Nymphaeumsaquaedukt
#> 51                          Hellenistisches Gebaeude unter Heroon III
#> 52                          Hellenistisches Gebaeude unter Heroon III
#> 53                                                 Temenos mit Tempel
#> 54                                                 Temenos mit Tempel
#> 55                                                 Temenos mit Tempel
#> 56                     Stadtmauer - Suedliche Quermauer, erste Anlage
#> 57                     Stadtmauer - Suedliche Quermauer, erste Anlage
#> 58                                        Hallenbau am sog. Nordmarkt
#> 59                                        Hallenbau am sog. Nordmarkt
#> 60                                        Hallenbau am sog. Nordmarkt
#> 61                                                           Tempel A
#> 62                                                           Tempel A
#> 63                                                Suedmarkt - Latrine
#> 64                                                Eski Balat Gebaeude
#> 65                                                     Ionische Halle
#> 66                                                     Ionische Halle
#> 67                                                     Ionische Halle
#> 68      Stadtmauer - Suedliche Quermauer, spaethellenistischer Ausbau
#> 69      Stadtmauer - Suedliche Quermauer, spaethellenistischer Ausbau
#> 70      Stadtmauer - Suedliche Quermauer, spaethellenistischer Ausbau
#> 71      Stadtmauer - Suedliche Quermauer, spaethellenistischer Ausbau
#> 72      Stadtmauer - Suedliche Quermauer, spaethellenistischer Ausbau
#> 73                                           Hellenistisches Peristyl
#> 74                                           Hellenistisches Peristyl
#> 75                                                    Faustinathermen
#> 76                                                    Faustinathermen
#> 77                                                    Faustinathermen
#> 78                                                    Faustinathermen
#> 79                                                         Stadiontor
#> 80                                                         Stadiontor
#> 81                                                         Stadiontor
#> 82                                                  Thermen am Museum
#> 83                                               Hafenhalle - Latrine
#> 84                                               Hafenhalle - Latrine
#> 85                                               Hafenhalle - Latrine
#> 86                                               Hafenhalle - Latrine
#> 87                                               Hafenhalle - Latrine
#> 88                                                    Friedhofskirche
#> 89                                                    Friedhofskirche
#> 90                                                    Friedhofskirche
#> 91                                                Kleiner Hallenplatz
#> 92                                        Propylon des Serapistempels
#> 93                                        Propylon des Serapistempels
#> 94                                        Propylon des Serapistempels
#> 95                                        Propylon des Serapistempels
#> 96                                        Propylon des Serapistempels
#> 97                                        Propylon des Serapistempels
#> 98                                                          Westmarkt
#> 99                                                          Westmarkt
#> 100                                                    Theaterkastell
#> 101                                          Befestigung des Kaletepe
#> 102                                             Basilika am Suedmarkt
#> 103                                             Basilika am Suedmarkt
#> 104                                             Basilika am Suedmarkt
#> 105                                    Befestigungsmauer am Humeitepe
#> 106                                    Befestigungsmauer am Humeitepe
#> 107                                    Befestigungsmauer am Humeitepe
#> 108                                                         Heroon II
#> 109                                                         Heroon II
#> 110                                                         Heroon II
#> 111                                Insula QR/18-19 Spaethellenistisch
#> 112                              Toepferwerkstatt beim Bischofspalast
#> 113                              Toepferwerkstatt beim Bischofspalast
#> 114                              Toepferwerkstatt beim Bischofspalast
#> 115                                           Byzantinischer Wohnturm
#> 116                                           Byzantinischer Wohnturm
#> 117                                           Byzantinischer Wohnturm
#> 118                                         Peristylhaus am Suedmarkt
#> 119                                         Peristylhaus am Suedmarkt
#> 120                                         Peristylhaus am Suedmarkt
#> 121                                Roemisches Hofhaus am Athenatempel
#> 122                                Roemisches Hofhaus am Athenatempel
#> 123                                Roemisches Hofhaus am Athenatempel
#> 124                                                    Thermenkastell
#> 125                                                    Thermenkastell
#> 126                                                          Synagoge
#> 127                                        Insula QR/18-19 Spaetantik
#> 128                                                          Tempel B
#> 129                                                          Tempel B
#> 130                                                          Tempel B
#> 131                                   Delphinion - Fruehhellenistisch
#> 132                                         Insula QR/18-19 Klassisch
#> 133                                          Altar des Demetertempels
#> 134                                          Altar des Demetertempels
#> 135                                          Byzantinische Stadtmauer
#> 136                                                     Grosse Kirche
#> 137                                                 Dionysosheiligtum
#> 138                                                 Dionysosheiligtum
#> 139                                                 Dionysosheiligtum
#> 140                                                 Dionysosheiligtum
#> 141                                                 Dionysosheiligtum
#> 142                                             Aelteres heiliges Tor
#> 143                                             Aelteres heiliges Tor
#> 144                                             Aelteres heiliges Tor
#> 145                                             Aelteres heiliges Tor
#> 146                              Kaianlagen am suedlichen Loewenhafen
#> 147                              Kaianlagen am suedlichen Loewenhafen
#> 148                              Kaianlagen am suedlichen Loewenhafen
#> 149                              Kaianlagen am suedlichen Loewenhafen
#> 150                              Kaianlagen am suedlichen Loewenhafen
#> 151                                                         Loewentor
#> 152                                                         Loewentor
#> 153                                                         Loewentor
#> 154                                                         Loewentor
#> 155                                                         Loewentor
#> 156                                                         Loewentor
#> 157                                             Stadtmauer - Ostmauer
#> 158                                             Stadtmauer - Ostmauer
#> 159                                             Stadtmauer - Ostmauer
#> 160                                             Stadtmauer - Ostmauer
#> 161                                             Stadtmauer - Ostmauer
#> 162                                                     Demetertempel
#> 163                                                     Demetertempel
#> 164                                         Wohnhaus FG/24-25 Phase 2
#> 165                                                   Osthafenthermen
#> 166                                                   Osthafenthermen
#> 167                                                   Osthafenthermen
#> 168                                         Magazinbau am Loewenhafen
#> 169                                                Suedstadt-Basilika
#> 170                                                Suedstadt-Basilika
#> 171                                                Suedstadt-Basilika
#> 172                                                   Insula DE/25-26
#> 173                                                   Insula DE/25-26
#> 174                                                   Insula DE/25-26
#> 175                                                   Insula DE/25-26
#> 176                                                   Insula DE/25-26
#> 177                                              Thermen am Humeitepe
#> 178                                                         Nymphaeum
#> 179                                                         Nymphaeum
#> 180                                                         Nymphaeum
#> 181                                                          Heroon I
#> 182                                                          Heroon I
#> 183                                                          Heroon I
#> 184                                                          Heroon I
#> 185                                            Juengeres heiliges Tor
#> 186                                            Juengeres heiliges Tor
#> 187                                            Juengeres heiliges Tor
#> 188                                            Juengeres heiliges Tor
#> 189                                            Juengeres heiliges Tor
#> 190                                            Juengeres heiliges Tor
#> 191                                        Insula QR/18-19 Kaiserzeit
#> 192                                             Kleines Hafenmonument
#> 193                                          Brunnen in der Suedstadt
#> 194                                              Vier-Saeulen-Moschee
#> 195                                              Vier-Saeulen-Moschee
#> 196                                              Vier-Saeulen-Moschee
#> 197                                          Hellenistische Hofanlage
#> 198                                          Hellenistische Hofanlage
#> 199                                                      Bouleuterion
#> 200                                                      Bouleuterion
#> 201                                                      Bouleuterion
#> 202                                                      Bouleuterion
#> 203                                                      Bouleuterion
#> 204                                                       Antentempel
#> 205                                                       Antentempel
#> 206                                       Delphinion - Kaiserzeitlich
#> 207                                       Delphinion - Kaiserzeitlich
#> 208                                       Delphinion - Kaiserzeitlich
#> 209                                       Delphinion - Kaiserzeitlich
#> 210                                       Delphinion - Kaiserzeitlich
#> 211                                                        Hafenmauer
#> 212                                                        Hafenmauer
#> 213                                            Heiligtum am Nordmarkt
#> 214                                            Heiligtum am Nordmarkt
#> 215 Stadtmauer - Suedliche Quermauer, Wiederherstellung der Gotenzeit
#> 216                             Delphinion - hellenistischer Umbau II
#> 217                              Peristylhaus unter dem Bischospalast
#> 218                              Peristylhaus unter dem Bischospalast
#> 219                              Peristylhaus unter dem Bischospalast
#> 220                                       Suedmarkt mit Antiochosstoa
#> 221                                       Suedmarkt mit Antiochosstoa
#> 222                                       Suedmarkt mit Antiochosstoa
#> 223                                       Suedmarkt mit Antiochosstoa
#> 224                                       Suedmarkt mit Antiochosstoa
#> 225                                Insula QR/18-19 Fruehhellenistisch
#> 226                                    Altar auf der Heiligen Strasse
#> 227                                    Altar auf der Heiligen Strasse
#> 228                                    Altar auf der Heiligen Strasse
#> 229                                         Wohnhaus FG/24-25 Phase 1
#> 230                                                Markttor von Milet
#> 231                                                Markttor von Milet
#> 232                                                Markttor von Milet
#> 233                                                Markttor von Milet
#> 234                                                Markttor von Milet
#> 235                                                Markttor von Milet
#> 236                                                Markttor von Milet
#> 237                                                Markttor von Milet
#> 238                                                Markttor von Milet
#> 239                                                        Hafenhalle
#> 240                                                        Hafenhalle
#> 241                                                        Hafenhalle
#> 242                                                        Hafenhalle
#> 243                                                        Hafenhalle
#> 244                                                           Stadion
#> 245                                                           Stadion
#> 246                                                           Stadion
#> 247                                                           Stadion
#> 248                                                           Stadion
#> 249                                                           Theater
#> 250                                                           Theater
#> 251                                                           Theater
#> 252                                                           Theater
#> 253                                                           Theater
#> 254                                                    Bischofspalast
#> 255                          Wohngebaeude unter dem Dionysosheiligtum
#> 256                          Wohngebaeude unter dem Dionysosheiligtum
#> 257                              Magazinhalle am Suedmarkt (Rueckbau)
#> 258                              Magazinhalle am Suedmarkt (Rueckbau)
#> 259                              Magazinhalle am Suedmarkt (Rueckbau)
#> 260                              Magazinhalle am Suedmarkt (Rueckbau)
#> 261                                                  Suedstadtthermen
#> 262                                      Delphinion - Fruehklassissch
#> 263                                      Delphinion - Fruehklassissch
#> 264                                                     Antiochosstoa
#> 265                                                     Antiochosstoa
#> 266                                                     Antiochosstoa
#> 267                                                     Antiochosstoa
#> 268                                                     Antiochosstoa
#> 269                                                     Capitothermen
#> 270                                                 Eumenes-Gymnasion
#> 271                                                 Eumenes-Gymnasion
#> 272                                         Wohnhaus FG/24-25 Phase 3
#> 273                                         Wohnhaus FG/24-25 Phase 3
#> 274                                                          Hafentor
#> 275                                                          Hafentor
#> 276                                                          Hafentor
#> 277                              Bau im Peristylhof des Bouleuterions
#> 278                              Bau im Peristylhof des Bouleuterions
#> 279                            Spaetantike Platzanlage am Kalabaktepe
#> 280                 Gebaeude nordwestlich des Hafentores am Humeitepe
#> 281                 Gebaeude nordwestlich des Hafentores am Humeitepe
#> 282                 Gebaeude nordwestlich des Hafentores am Humeitepe
#> 283                                                         Serapeion
#> 284                                                         Serapeion
#> 285                                                         Serapeion
#> 286                                                    Michaelskirche
#> 287                                                    Michaelskirche
#> 288                                                    Michaelskirche
#> 289                                                            Museum
#> 290                                       Stadtmauer um den Humeitepe
#> 291                                       Stadtmauer um den Humeitepe
#> 292                                       Stadtmauer um den Humeitepe
#> 293                                       Stadtmauer um den Humeitepe
#> 294                                       Stadtmauer um den Humeitepe
#> 295                                       Stadtmauer um den Humeitepe
#>                         period.start                       period.end
#> 1                   Late Hellenistic Early Byzantine / Late Antiquity
#> 2                   Late Hellenistic Early Byzantine / Late Antiquity
#> 3                   Late Hellenistic Early Byzantine / Late Antiquity
#> 4                   Late Hellenistic Early Byzantine / Late Antiquity
#> 5                   Late Hellenistic Early Byzantine / Late Antiquity
#> 6                   Late Hellenistic Early Byzantine / Late Antiquity
#> 7                     Roman imperial                   Roman imperial
#> 8                     Roman imperial                   Roman imperial
#> 9                     Roman imperial                   Roman imperial
#> 10                       Hellenistic                      Hellenistic
#> 11                       Hellenistic                      Hellenistic
#> 12                   Recent / Modern                  Recent / Modern
#> 13                              <NA>                             <NA>
#> 14                              <NA>                   Roman imperial
#> 15                         Byzantine                        Byzantine
#> 16                         Byzantine                        Byzantine
#> 17                         Byzantine                        Byzantine
#> 18                    Roman imperial                   Roman imperial
#> 19                    Roman imperial                   Roman imperial
#> 20                    Roman imperial                   Roman imperial
#> 21                       Hellenistic                      Hellenistic
#> 22                       Hellenistic                      Hellenistic
#> 23                   Middle Imperial                  Middle Imperial
#> 24                  Late Hellenistic                   Early imperial
#> 25                  Late Hellenistic                   Early imperial
#> 26                         Byzantine                        Byzantine
#> 27                         Byzantine                        Byzantine
#> 28                         Byzantine                        Byzantine
#> 29                       Hellenistic Early Byzantine / Late Antiquity
#> 30                       Hellenistic Early Byzantine / Late Antiquity
#> 31                       Hellenistic Early Byzantine / Late Antiquity
#> 32                       Hellenistic Early Byzantine / Late Antiquity
#> 33                       Hellenistic Early Byzantine / Late Antiquity
#> 34                       Hellenistic Early Byzantine / Late Antiquity
#> 35                       Hellenistic Early Byzantine / Late Antiquity
#> 36  Early Byzantine / Late Antiquity Early Byzantine / Late Antiquity
#> 37                    Late Antiquity                   Late Antiquity
#> 38                 Early hellenistic                Early hellenistic
#> 39                   Middle Imperial                   Late Antiquity
#> 40                   Middle Imperial                   Late Antiquity
#> 41                   Middle Imperial                   Late Antiquity
#> 42                 Early hellenistic                   Roman imperial
#> 43                 Early hellenistic                   Roman imperial
#> 44                 Early hellenistic                   Roman imperial
#> 45                 Early hellenistic                   Roman imperial
#> 46                 Early hellenistic                   Roman imperial
#> 47                    Roman imperial                   Roman imperial
#> 48                    Roman imperial                   Roman imperial
#> 49                    Roman imperial                   Roman imperial
#> 50                    Early imperial                   Early imperial
#> 51                       Hellenistic                      Hellenistic
#> 52                       Hellenistic                      Hellenistic
#> 53                    Roman imperial                   Roman imperial
#> 54                    Roman imperial                   Roman imperial
#> 55                    Roman imperial                   Roman imperial
#> 56                 Early hellenistic                 Late Hellenistic
#> 57                 Early hellenistic                 Late Hellenistic
#> 58                    Roman imperial                   Roman imperial
#> 59                    Roman imperial                   Roman imperial
#> 60                    Roman imperial                   Roman imperial
#> 61                       Hellenistic                      Hellenistic
#> 62                       Hellenistic                      Hellenistic
#> 63                     Late Imperial                    Late Imperial
#> 64                   Recent / Modern                  Recent / Modern
#> 65                    Roman imperial                   Roman imperial
#> 66                    Roman imperial                   Roman imperial
#> 67                    Roman imperial                   Roman imperial
#> 68                  Late Hellenistic                   Late Antiquity
#> 69                  Late Hellenistic                   Late Antiquity
#> 70                  Late Hellenistic                   Late Antiquity
#> 71                  Late Hellenistic                   Late Antiquity
#> 72                  Late Hellenistic                   Late Antiquity
#> 73                       Hellenistic                      Hellenistic
#> 74                       Hellenistic                      Hellenistic
#> 75                    Roman imperial                   Late Antiquity
#> 76                    Roman imperial                   Late Antiquity
#> 77                    Roman imperial                   Late Antiquity
#> 78                    Roman imperial                   Late Antiquity
#> 79                     Late Imperial Early Byzantine / Late Antiquity
#> 80                     Late Imperial Early Byzantine / Late Antiquity
#> 81                     Late Imperial Early Byzantine / Late Antiquity
#> 82                     Late Imperial                    Late Imperial
#> 83                     Late Imperial                        Byzantine
#> 84                     Late Imperial                        Byzantine
#> 85                     Late Imperial                        Byzantine
#> 86                     Late Imperial                        Byzantine
#> 87                     Late Imperial                        Byzantine
#> 88                         Byzantine                        Byzantine
#> 89                         Byzantine                        Byzantine
#> 90                         Byzantine                        Byzantine
#> 91                 Early hellenistic                Early hellenistic
#> 92                   Middle Imperial                        Byzantine
#> 93                   Middle Imperial                        Byzantine
#> 94                   Middle Imperial                        Byzantine
#> 95                   Middle Imperial                        Byzantine
#> 96                   Middle Imperial                        Byzantine
#> 97                   Middle Imperial                        Byzantine
#> 98                       Hellenistic                      Hellenistic
#> 99                       Hellenistic                      Hellenistic
#> 100                   Late Byzantine                   Late Byzantine
#> 101                   Late Byzantine                   Late Byzantine
#> 102                   Roman imperial                   Roman imperial
#> 103                   Roman imperial                   Roman imperial
#> 104                   Roman imperial                   Roman imperial
#> 105                    Late Imperial Early Byzantine / Late Antiquity
#> 106                    Late Imperial Early Byzantine / Late Antiquity
#> 107                    Late Imperial Early Byzantine / Late Antiquity
#> 108                  Middle Imperial                   Late Antiquity
#> 109                  Middle Imperial                   Late Antiquity
#> 110                  Middle Imperial                   Late Antiquity
#> 111                             <NA>                             <NA>
#> 112                   Late Byzantine                          Ottoman
#> 113                   Late Byzantine                          Ottoman
#> 114                   Late Byzantine                          Ottoman
#> 115                        Byzantine                        Byzantine
#> 116                        Byzantine                        Byzantine
#> 117                        Byzantine                        Byzantine
#> 118                   Roman imperial                   Roman imperial
#> 119                   Roman imperial                   Roman imperial
#> 120                   Roman imperial                   Roman imperial
#> 121                   Roman imperial                   Roman imperial
#> 122                   Roman imperial                   Roman imperial
#> 123                   Roman imperial                   Roman imperial
#> 124                    Late Imperial                   Late Antiquity
#> 125                    Late Imperial                   Late Antiquity
#> 126                   Late Antiquity                   Late Antiquity
#> 127                             <NA>                             <NA>
#> 128                   Roman imperial                   Roman imperial
#> 129                   Roman imperial                   Roman imperial
#> 130                   Roman imperial                   Roman imperial
#> 131                Early hellenistic                Early hellenistic
#> 132                             <NA>                             <NA>
#> 133                      Hellenistic                      Hellenistic
#> 134                      Hellenistic                      Hellenistic
#> 135 Early Byzantine / Late Antiquity Early Byzantine / Late Antiquity
#> 136 Early Byzantine / Late Antiquity Early Byzantine / Late Antiquity
#> 137                      Hellenistic                   Roman imperial
#> 138                      Hellenistic                   Roman imperial
#> 139                      Hellenistic                   Roman imperial
#> 140                      Hellenistic                   Roman imperial
#> 141                      Hellenistic                   Roman imperial
#> 142                        Classical                      Hellenistic
#> 143                        Classical                      Hellenistic
#> 144                        Classical                      Hellenistic
#> 145                        Classical                      Hellenistic
#> 146                      Hellenistic                   Roman imperial
#> 147                      Hellenistic                   Roman imperial
#> 148                      Hellenistic                   Roman imperial
#> 149                      Hellenistic                   Roman imperial
#> 150                      Hellenistic                   Roman imperial
#> 151                      Hellenistic                   Late Antiquity
#> 152                      Hellenistic                   Late Antiquity
#> 153                      Hellenistic                   Late Antiquity
#> 154                      Hellenistic                   Late Antiquity
#> 155                      Hellenistic                   Late Antiquity
#> 156                      Hellenistic                   Late Antiquity
#> 157                Early hellenistic                    Late Imperial
#> 158                Early hellenistic                    Late Imperial
#> 159                Early hellenistic                    Late Imperial
#> 160                Early hellenistic                    Late Imperial
#> 161                Early hellenistic                    Late Imperial
#> 162                      Hellenistic                      Hellenistic
#> 163                      Hellenistic                      Hellenistic
#> 164                   Late Classical                   Late Classical
#> 165                   Roman imperial                   Roman imperial
#> 166                   Roman imperial                   Roman imperial
#> 167                   Roman imperial                   Roman imperial
#> 168                  Middle Imperial                  Middle Imperial
#> 169                        Byzantine                        Byzantine
#> 170                        Byzantine                        Byzantine
#> 171                        Byzantine                        Byzantine
#> 172                   Early imperial Early Byzantine / Late Antiquity
#> 173                   Early imperial Early Byzantine / Late Antiquity
#> 174                   Early imperial Early Byzantine / Late Antiquity
#> 175                   Early imperial Early Byzantine / Late Antiquity
#> 176                   Early imperial Early Byzantine / Late Antiquity
#> 177                  Middle Imperial                  Middle Imperial
#> 178                   Roman imperial                   Roman imperial
#> 179                   Roman imperial                   Roman imperial
#> 180                   Roman imperial                   Roman imperial
#> 181                      Hellenistic                  Middle Imperial
#> 182                      Hellenistic                  Middle Imperial
#> 183                      Hellenistic                  Middle Imperial
#> 184                      Hellenistic                  Middle Imperial
#> 185                   Late Classical                   Roman imperial
#> 186                   Late Classical                   Roman imperial
#> 187                   Late Classical                   Roman imperial
#> 188                   Late Classical                   Roman imperial
#> 189                   Late Classical                   Roman imperial
#> 190                   Late Classical                   Roman imperial
#> 191                             <NA>                             <NA>
#> 192                   Early imperial                   Early imperial
#> 193                  Recent / Modern                  Recent / Modern
#> 194                   Late Byzantine                          Ottoman
#> 195                   Late Byzantine                          Ottoman
#> 196                   Late Byzantine                          Ottoman
#> 197                      Hellenistic                      Hellenistic
#> 198                      Hellenistic                      Hellenistic
#> 199                      Hellenistic                   Roman imperial
#> 200                      Hellenistic                   Roman imperial
#> 201                      Hellenistic                   Roman imperial
#> 202                      Hellenistic                   Roman imperial
#> 203                      Hellenistic                   Roman imperial
#> 204                      Hellenistic                      Hellenistic
#> 205                      Hellenistic                      Hellenistic
#> 206                   Roman imperial Early Byzantine / Late Antiquity
#> 207                   Roman imperial Early Byzantine / Late Antiquity
#> 208                   Roman imperial Early Byzantine / Late Antiquity
#> 209                   Roman imperial Early Byzantine / Late Antiquity
#> 210                   Roman imperial Early Byzantine / Late Antiquity
#> 211                      Hellenistic                      Hellenistic
#> 212                      Hellenistic                      Hellenistic
#> 213                      Hellenistic                      Hellenistic
#> 214                      Hellenistic                      Hellenistic
#> 215                    Late Imperial                    Late Imperial
#> 216                 Late Hellenistic                 Late Hellenistic
#> 217                    Late Imperial Early Byzantine / Late Antiquity
#> 218                    Late Imperial Early Byzantine / Late Antiquity
#> 219                    Late Imperial Early Byzantine / Late Antiquity
#> 220                      Hellenistic                   Roman imperial
#> 221                      Hellenistic                   Roman imperial
#> 222                      Hellenistic                   Roman imperial
#> 223                      Hellenistic                   Roman imperial
#> 224                      Hellenistic                   Roman imperial
#> 225                             <NA>                             <NA>
#> 226                      Hellenistic                   Early imperial
#> 227                      Hellenistic                   Early imperial
#> 228                      Hellenistic                   Early imperial
#> 229                  Early classical                  Early classical
#> 230                      Hellenistic                        Byzantine
#> 231                      Hellenistic                        Byzantine
#> 232                      Hellenistic                        Byzantine
#> 233                      Hellenistic                        Byzantine
#> 234                      Hellenistic                        Byzantine
#> 235                      Hellenistic                        Byzantine
#> 236                      Hellenistic                        Byzantine
#> 237                      Hellenistic                        Byzantine
#> 238                      Hellenistic                        Byzantine
#> 239                Early hellenistic                   Roman imperial
#> 240                Early hellenistic                   Roman imperial
#> 241                Early hellenistic                   Roman imperial
#> 242                Early hellenistic                   Roman imperial
#> 243                Early hellenistic                   Roman imperial
#> 244                      Hellenistic                   Roman imperial
#> 245                      Hellenistic                   Roman imperial
#> 246                      Hellenistic                   Roman imperial
#> 247                      Hellenistic                   Roman imperial
#> 248                      Hellenistic                   Roman imperial
#> 249                      Hellenistic                   Roman imperial
#> 250                      Hellenistic                   Roman imperial
#> 251                      Hellenistic                   Roman imperial
#> 252                      Hellenistic                   Roman imperial
#> 253                      Hellenistic                   Roman imperial
#> 254 Early Byzantine / Late Antiquity Early Byzantine / Late Antiquity
#> 255                        Classical                        Classical
#> 256                        Classical                        Classical
#> 257                 Late Hellenistic                   Roman imperial
#> 258                 Late Hellenistic                   Roman imperial
#> 259                 Late Hellenistic                   Roman imperial
#> 260                 Late Hellenistic                   Roman imperial
#> 261                    Late Imperial                    Late Imperial
#> 262                  Early classical                   Late Classical
#> 263                  Early classical                   Late Classical
#> 264                      Hellenistic                   Roman imperial
#> 265                      Hellenistic                   Roman imperial
#> 266                      Hellenistic                   Roman imperial
#> 267                      Hellenistic                   Roman imperial
#> 268                      Hellenistic                   Roman imperial
#> 269                   Early imperial                   Early imperial
#> 270                      Hellenistic                      Hellenistic
#> 271                      Hellenistic                      Hellenistic
#> 272                      Hellenistic                      Hellenistic
#> 273                      Hellenistic                      Hellenistic
#> 274                   Roman imperial                   Roman imperial
#> 275                   Roman imperial                   Roman imperial
#> 276                   Roman imperial                   Roman imperial
#> 277                 Late Hellenistic                   Early imperial
#> 278                 Late Hellenistic                   Early imperial
#> 279 Early Byzantine / Late Antiquity Early Byzantine / Late Antiquity
#> 280                   Roman imperial                   Roman imperial
#> 281                   Roman imperial                   Roman imperial
#> 282                   Roman imperial                   Roman imperial
#> 283                   Roman imperial                   Roman imperial
#> 284                   Roman imperial                   Roman imperial
#> 285                   Roman imperial                   Roman imperial
#> 286                        Byzantine                        Byzantine
#> 287                        Byzantine                        Byzantine
#> 288                        Byzantine                        Byzantine
#> 289                  Recent / Modern                  Recent / Modern
#> 290                   Late Classical                    Late Imperial
#> 291                   Late Classical                    Late Imperial
#> 292                   Late Classical                    Late Imperial
#> 293                   Late Classical                    Late Imperial
#> 294                   Late Classical                    Late Imperial
#> 295                   Late Classical                    Late Imperial
#>                           start.ungr                         end.ungr
#> 1                   Late Hellenistic Early Byzantine / Late Antiquity
#> 2                   Late Hellenistic Early Byzantine / Late Antiquity
#> 3                   Late Hellenistic Early Byzantine / Late Antiquity
#> 4                   Late Hellenistic Early Byzantine / Late Antiquity
#> 5                   Late Hellenistic Early Byzantine / Late Antiquity
#> 6                   Late Hellenistic Early Byzantine / Late Antiquity
#> 7                     Early imperial                    Late Imperial
#> 8                     Early imperial                    Late Imperial
#> 9                     Early imperial                    Late Imperial
#> 10                 Early hellenistic                 Late Hellenistic
#> 11                 Early hellenistic                 Late Hellenistic
#> 12                   Recent / Modern                  Recent / Modern
#> 13                              <NA>                             <NA>
#> 14                              <NA>                    Late Imperial
#> 15  Early Byzantine / Late Antiquity                   Late Byzantine
#> 16  Early Byzantine / Late Antiquity                   Late Byzantine
#> 17  Early Byzantine / Late Antiquity                   Late Byzantine
#> 18                    Early imperial                    Late Imperial
#> 19                    Early imperial                    Late Imperial
#> 20                    Early imperial                    Late Imperial
#> 21                 Early hellenistic                 Late Hellenistic
#> 22                 Early hellenistic                 Late Hellenistic
#> 23                   Middle Imperial                  Middle Imperial
#> 24                  Late Hellenistic                   Early imperial
#> 25                  Late Hellenistic                   Early imperial
#> 26  Early Byzantine / Late Antiquity                   Late Byzantine
#> 27  Early Byzantine / Late Antiquity                   Late Byzantine
#> 28  Early Byzantine / Late Antiquity                   Late Byzantine
#> 29                 Early hellenistic Early Byzantine / Late Antiquity
#> 30                 Early hellenistic Early Byzantine / Late Antiquity
#> 31                 Early hellenistic Early Byzantine / Late Antiquity
#> 32                 Early hellenistic Early Byzantine / Late Antiquity
#> 33                 Early hellenistic Early Byzantine / Late Antiquity
#> 34                 Early hellenistic Early Byzantine / Late Antiquity
#> 35                 Early hellenistic Early Byzantine / Late Antiquity
#> 36  Early Byzantine / Late Antiquity Early Byzantine / Late Antiquity
#> 37                    Late Antiquity                   Late Antiquity
#> 38                 Early hellenistic                Early hellenistic
#> 39                   Middle Imperial                   Late Antiquity
#> 40                   Middle Imperial                   Late Antiquity
#> 41                   Middle Imperial                   Late Antiquity
#> 42                 Early hellenistic                    Late Imperial
#> 43                 Early hellenistic                    Late Imperial
#> 44                 Early hellenistic                    Late Imperial
#> 45                 Early hellenistic                    Late Imperial
#> 46                 Early hellenistic                    Late Imperial
#> 47                    Early imperial                    Late Imperial
#> 48                    Early imperial                    Late Imperial
#> 49                    Early imperial                    Late Imperial
#> 50                    Early imperial                   Early imperial
#> 51                 Early hellenistic                 Late Hellenistic
#> 52                 Early hellenistic                 Late Hellenistic
#> 53                    Early imperial                    Late Imperial
#> 54                    Early imperial                    Late Imperial
#> 55                    Early imperial                    Late Imperial
#> 56                 Early hellenistic                 Late Hellenistic
#> 57                 Early hellenistic                 Late Hellenistic
#> 58                    Early imperial                    Late Imperial
#> 59                    Early imperial                    Late Imperial
#> 60                    Early imperial                    Late Imperial
#> 61                 Early hellenistic                 Late Hellenistic
#> 62                 Early hellenistic                 Late Hellenistic
#> 63                     Late Imperial                    Late Imperial
#> 64                   Recent / Modern                  Recent / Modern
#> 65                    Early imperial                    Late Imperial
#> 66                    Early imperial                    Late Imperial
#> 67                    Early imperial                    Late Imperial
#> 68                  Late Hellenistic                   Late Antiquity
#> 69                  Late Hellenistic                   Late Antiquity
#> 70                  Late Hellenistic                   Late Antiquity
#> 71                  Late Hellenistic                   Late Antiquity
#> 72                  Late Hellenistic                   Late Antiquity
#> 73                 Early hellenistic                 Late Hellenistic
#> 74                 Early hellenistic                 Late Hellenistic
#> 75                    Early imperial                   Late Antiquity
#> 76                    Early imperial                   Late Antiquity
#> 77                    Early imperial                   Late Antiquity
#> 78                    Early imperial                   Late Antiquity
#> 79                     Late Imperial Early Byzantine / Late Antiquity
#> 80                     Late Imperial Early Byzantine / Late Antiquity
#> 81                     Late Imperial Early Byzantine / Late Antiquity
#> 82                     Late Imperial                    Late Imperial
#> 83                     Late Imperial                   Late Byzantine
#> 84                     Late Imperial                   Late Byzantine
#> 85                     Late Imperial                   Late Byzantine
#> 86                     Late Imperial                   Late Byzantine
#> 87                     Late Imperial                   Late Byzantine
#> 88  Early Byzantine / Late Antiquity                   Late Byzantine
#> 89  Early Byzantine / Late Antiquity                   Late Byzantine
#> 90  Early Byzantine / Late Antiquity                   Late Byzantine
#> 91                 Early hellenistic                Early hellenistic
#> 92                   Middle Imperial                   Late Byzantine
#> 93                   Middle Imperial                   Late Byzantine
#> 94                   Middle Imperial                   Late Byzantine
#> 95                   Middle Imperial                   Late Byzantine
#> 96                   Middle Imperial                   Late Byzantine
#> 97                   Middle Imperial                   Late Byzantine
#> 98                 Early hellenistic                 Late Hellenistic
#> 99                 Early hellenistic                 Late Hellenistic
#> 100                   Late Byzantine                   Late Byzantine
#> 101                   Late Byzantine                   Late Byzantine
#> 102                   Early imperial                    Late Imperial
#> 103                   Early imperial                    Late Imperial
#> 104                   Early imperial                    Late Imperial
#> 105                    Late Imperial Early Byzantine / Late Antiquity
#> 106                    Late Imperial Early Byzantine / Late Antiquity
#> 107                    Late Imperial Early Byzantine / Late Antiquity
#> 108                  Middle Imperial                   Late Antiquity
#> 109                  Middle Imperial                   Late Antiquity
#> 110                  Middle Imperial                   Late Antiquity
#> 111                             <NA>                             <NA>
#> 112                   Late Byzantine                          Ottoman
#> 113                   Late Byzantine                          Ottoman
#> 114                   Late Byzantine                          Ottoman
#> 115 Early Byzantine / Late Antiquity                   Late Byzantine
#> 116 Early Byzantine / Late Antiquity                   Late Byzantine
#> 117 Early Byzantine / Late Antiquity                   Late Byzantine
#> 118                   Early imperial                    Late Imperial
#> 119                   Early imperial                    Late Imperial
#> 120                   Early imperial                    Late Imperial
#> 121                   Early imperial                    Late Imperial
#> 122                   Early imperial                    Late Imperial
#> 123                   Early imperial                    Late Imperial
#> 124                    Late Imperial                   Late Antiquity
#> 125                    Late Imperial                   Late Antiquity
#> 126                   Late Antiquity                   Late Antiquity
#> 127                             <NA>                             <NA>
#> 128                   Early imperial                    Late Imperial
#> 129                   Early imperial                    Late Imperial
#> 130                   Early imperial                    Late Imperial
#> 131                Early hellenistic                Early hellenistic
#> 132                             <NA>                             <NA>
#> 133                Early hellenistic                 Late Hellenistic
#> 134                Early hellenistic                 Late Hellenistic
#> 135 Early Byzantine / Late Antiquity Early Byzantine / Late Antiquity
#> 136 Early Byzantine / Late Antiquity Early Byzantine / Late Antiquity
#> 137                Early hellenistic                    Late Imperial
#> 138                Early hellenistic                    Late Imperial
#> 139                Early hellenistic                    Late Imperial
#> 140                Early hellenistic                    Late Imperial
#> 141                Early hellenistic                    Late Imperial
#> 142                  Early classical                 Late Hellenistic
#> 143                  Early classical                 Late Hellenistic
#> 144                  Early classical                 Late Hellenistic
#> 145                  Early classical                 Late Hellenistic
#> 146                Early hellenistic                    Late Imperial
#> 147                Early hellenistic                    Late Imperial
#> 148                Early hellenistic                    Late Imperial
#> 149                Early hellenistic                    Late Imperial
#> 150                Early hellenistic                    Late Imperial
#> 151                Early hellenistic                   Late Antiquity
#> 152                Early hellenistic                   Late Antiquity
#> 153                Early hellenistic                   Late Antiquity
#> 154                Early hellenistic                   Late Antiquity
#> 155                Early hellenistic                   Late Antiquity
#> 156                Early hellenistic                   Late Antiquity
#> 157                Early hellenistic                    Late Imperial
#> 158                Early hellenistic                    Late Imperial
#> 159                Early hellenistic                    Late Imperial
#> 160                Early hellenistic                    Late Imperial
#> 161                Early hellenistic                    Late Imperial
#> 162                Early hellenistic                 Late Hellenistic
#> 163                Early hellenistic                 Late Hellenistic
#> 164                   Late Classical                   Late Classical
#> 165                   Early imperial                    Late Imperial
#> 166                   Early imperial                    Late Imperial
#> 167                   Early imperial                    Late Imperial
#> 168                  Middle Imperial                  Middle Imperial
#> 169 Early Byzantine / Late Antiquity                   Late Byzantine
#> 170 Early Byzantine / Late Antiquity                   Late Byzantine
#> 171 Early Byzantine / Late Antiquity                   Late Byzantine
#> 172                   Early imperial Early Byzantine / Late Antiquity
#> 173                   Early imperial Early Byzantine / Late Antiquity
#> 174                   Early imperial Early Byzantine / Late Antiquity
#> 175                   Early imperial Early Byzantine / Late Antiquity
#> 176                   Early imperial Early Byzantine / Late Antiquity
#> 177                  Middle Imperial                  Middle Imperial
#> 178                   Early imperial                    Late Imperial
#> 179                   Early imperial                    Late Imperial
#> 180                   Early imperial                    Late Imperial
#> 181                Early hellenistic                  Middle Imperial
#> 182                Early hellenistic                  Middle Imperial
#> 183                Early hellenistic                  Middle Imperial
#> 184                Early hellenistic                  Middle Imperial
#> 185                   Late Classical                    Late Imperial
#> 186                   Late Classical                    Late Imperial
#> 187                   Late Classical                    Late Imperial
#> 188                   Late Classical                    Late Imperial
#> 189                   Late Classical                    Late Imperial
#> 190                   Late Classical                    Late Imperial
#> 191                             <NA>                             <NA>
#> 192                   Early imperial                   Early imperial
#> 193                  Recent / Modern                  Recent / Modern
#> 194                   Late Byzantine                          Ottoman
#> 195                   Late Byzantine                          Ottoman
#> 196                   Late Byzantine                          Ottoman
#> 197                Early hellenistic                 Late Hellenistic
#> 198                Early hellenistic                 Late Hellenistic
#> 199                Early hellenistic                    Late Imperial
#> 200                Early hellenistic                    Late Imperial
#> 201                Early hellenistic                    Late Imperial
#> 202                Early hellenistic                    Late Imperial
#> 203                Early hellenistic                    Late Imperial
#> 204                Early hellenistic                 Late Hellenistic
#> 205                Early hellenistic                 Late Hellenistic
#> 206                   Early imperial Early Byzantine / Late Antiquity
#> 207                   Early imperial Early Byzantine / Late Antiquity
#> 208                   Early imperial Early Byzantine / Late Antiquity
#> 209                   Early imperial Early Byzantine / Late Antiquity
#> 210                   Early imperial Early Byzantine / Late Antiquity
#> 211                Early hellenistic                 Late Hellenistic
#> 212                Early hellenistic                 Late Hellenistic
#> 213                Early hellenistic                 Late Hellenistic
#> 214                Early hellenistic                 Late Hellenistic
#> 215                    Late Imperial                    Late Imperial
#> 216                 Late Hellenistic                 Late Hellenistic
#> 217                    Late Imperial Early Byzantine / Late Antiquity
#> 218                    Late Imperial Early Byzantine / Late Antiquity
#> 219                    Late Imperial Early Byzantine / Late Antiquity
#> 220                Early hellenistic                    Late Imperial
#> 221                Early hellenistic                    Late Imperial
#> 222                Early hellenistic                    Late Imperial
#> 223                Early hellenistic                    Late Imperial
#> 224                Early hellenistic                    Late Imperial
#> 225                             <NA>                             <NA>
#> 226                Early hellenistic                   Early imperial
#> 227                Early hellenistic                   Early imperial
#> 228                Early hellenistic                   Early imperial
#> 229                  Early classical                  Early classical
#> 230                Early hellenistic                   Late Byzantine
#> 231                Early hellenistic                   Late Byzantine
#> 232                Early hellenistic                   Late Byzantine
#> 233                Early hellenistic                   Late Byzantine
#> 234                Early hellenistic                   Late Byzantine
#> 235                Early hellenistic                   Late Byzantine
#> 236                Early hellenistic                   Late Byzantine
#> 237                Early hellenistic                   Late Byzantine
#> 238                Early hellenistic                   Late Byzantine
#> 239                Early hellenistic                    Late Imperial
#> 240                Early hellenistic                    Late Imperial
#> 241                Early hellenistic                    Late Imperial
#> 242                Early hellenistic                    Late Imperial
#> 243                Early hellenistic                    Late Imperial
#> 244                Early hellenistic                    Late Imperial
#> 245                Early hellenistic                    Late Imperial
#> 246                Early hellenistic                    Late Imperial
#> 247                Early hellenistic                    Late Imperial
#> 248                Early hellenistic                    Late Imperial
#> 249                Early hellenistic                    Late Imperial
#> 250                Early hellenistic                    Late Imperial
#> 251                Early hellenistic                    Late Imperial
#> 252                Early hellenistic                    Late Imperial
#> 253                Early hellenistic                    Late Imperial
#> 254 Early Byzantine / Late Antiquity Early Byzantine / Late Antiquity
#> 255                  Early classical                   Late Classical
#> 256                  Early classical                   Late Classical
#> 257                 Late Hellenistic                    Late Imperial
#> 258                 Late Hellenistic                    Late Imperial
#> 259                 Late Hellenistic                    Late Imperial
#> 260                 Late Hellenistic                    Late Imperial
#> 261                    Late Imperial                    Late Imperial
#> 262                  Early classical                   Late Classical
#> 263                  Early classical                   Late Classical
#> 264                Early hellenistic                    Late Imperial
#> 265                Early hellenistic                    Late Imperial
#> 266                Early hellenistic                    Late Imperial
#> 267                Early hellenistic                    Late Imperial
#> 268                Early hellenistic                    Late Imperial
#> 269                   Early imperial                   Early imperial
#> 270                Early hellenistic                 Late Hellenistic
#> 271                Early hellenistic                 Late Hellenistic
#> 272                Early hellenistic                 Late Hellenistic
#> 273                Early hellenistic                 Late Hellenistic
#> 274                   Early imperial                    Late Imperial
#> 275                   Early imperial                    Late Imperial
#> 276                   Early imperial                    Late Imperial
#> 277                 Late Hellenistic                   Early imperial
#> 278                 Late Hellenistic                   Early imperial
#> 279 Early Byzantine / Late Antiquity Early Byzantine / Late Antiquity
#> 280                   Early imperial                    Late Imperial
#> 281                   Early imperial                    Late Imperial
#> 282                   Early imperial                    Late Imperial
#> 283                   Early imperial                    Late Imperial
#> 284                   Early imperial                    Late Imperial
#> 285                   Early imperial                    Late Imperial
#> 286 Early Byzantine / Late Antiquity                   Late Byzantine
#> 287 Early Byzantine / Late Antiquity                   Late Byzantine
#> 288 Early Byzantine / Late Antiquity                   Late Byzantine
#> 289                  Recent / Modern                  Recent / Modern
#> 290                   Late Classical                    Late Imperial
#> 291                   Late Classical                    Late Imperial
#> 292                   Late Classical                    Late Imperial
#> 293                   Late Classical                    Late Imperial
#> 294                   Late Classical                    Late Imperial
#> 295                   Late Classical                    Late Imperial
#>                               period  fraction
#> 1                   Late Hellenistic 0.1666667
#> 2                     Early imperial 0.1666667
#> 3                    Middle Imperial 0.1666667
#> 4                      Late Imperial 0.1666667
#> 5                     Late Antiquity 0.1666667
#> 6   Early Byzantine / Late Antiquity 0.1666667
#> 7                     Early imperial 0.3333333
#> 8                    Middle Imperial 0.3333333
#> 9                      Late Imperial 0.3333333
#> 10                 Early hellenistic 0.5000000
#> 11                  Late Hellenistic 0.5000000
#> 12                   Recent / Modern 1.0000000
#> 13                              <NA>        NA
#> 14                              <NA>        NA
#> 15  Early Byzantine / Late Antiquity 0.3333333
#> 16                  Middle Byzantine 0.3333333
#> 17                    Late Byzantine 0.3333333
#> 18                    Early imperial 0.3333333
#> 19                   Middle Imperial 0.3333333
#> 20                     Late Imperial 0.3333333
#> 21                 Early hellenistic 0.5000000
#> 22                  Late Hellenistic 0.5000000
#> 23                   Middle Imperial 1.0000000
#> 24                  Late Hellenistic 0.5000000
#> 25                    Early imperial 0.5000000
#> 26  Early Byzantine / Late Antiquity 0.3333333
#> 27                  Middle Byzantine 0.3333333
#> 28                    Late Byzantine 0.3333333
#> 29                 Early hellenistic 0.1428571
#> 30                  Late Hellenistic 0.1428571
#> 31                    Early imperial 0.1428571
#> 32                   Middle Imperial 0.1428571
#> 33                     Late Imperial 0.1428571
#> 34                    Late Antiquity 0.1428571
#> 35  Early Byzantine / Late Antiquity 0.1428571
#> 36  Early Byzantine / Late Antiquity 1.0000000
#> 37                    Late Antiquity 1.0000000
#> 38                 Early hellenistic 1.0000000
#> 39                   Middle Imperial 0.3333333
#> 40                     Late Imperial 0.3333333
#> 41                    Late Antiquity 0.3333333
#> 42                 Early hellenistic 0.2000000
#> 43                  Late Hellenistic 0.2000000
#> 44                    Early imperial 0.2000000
#> 45                   Middle Imperial 0.2000000
#> 46                     Late Imperial 0.2000000
#> 47                    Early imperial 0.3333333
#> 48                   Middle Imperial 0.3333333
#> 49                     Late Imperial 0.3333333
#> 50                    Early imperial 1.0000000
#> 51                 Early hellenistic 0.5000000
#> 52                  Late Hellenistic 0.5000000
#> 53                    Early imperial 0.3333333
#> 54                   Middle Imperial 0.3333333
#> 55                     Late Imperial 0.3333333
#> 56                 Early hellenistic 0.5000000
#> 57                  Late Hellenistic 0.5000000
#> 58                    Early imperial 0.3333333
#> 59                   Middle Imperial 0.3333333
#> 60                     Late Imperial 0.3333333
#> 61                 Early hellenistic 0.5000000
#> 62                  Late Hellenistic 0.5000000
#> 63                     Late Imperial 1.0000000
#> 64                   Recent / Modern 1.0000000
#> 65                    Early imperial 0.3333333
#> 66                   Middle Imperial 0.3333333
#> 67                     Late Imperial 0.3333333
#> 68                  Late Hellenistic 0.2000000
#> 69                    Early imperial 0.2000000
#> 70                   Middle Imperial 0.2000000
#> 71                     Late Imperial 0.2000000
#> 72                    Late Antiquity 0.2000000
#> 73                 Early hellenistic 0.5000000
#> 74                  Late Hellenistic 0.5000000
#> 75                    Early imperial 0.2500000
#> 76                   Middle Imperial 0.2500000
#> 77                     Late Imperial 0.2500000
#> 78                    Late Antiquity 0.2500000
#> 79                     Late Imperial 0.3333333
#> 80                    Late Antiquity 0.3333333
#> 81  Early Byzantine / Late Antiquity 0.3333333
#> 82                     Late Imperial 1.0000000
#> 83                     Late Imperial 0.2000000
#> 84                    Late Antiquity 0.2000000
#> 85  Early Byzantine / Late Antiquity 0.2000000
#> 86                  Middle Byzantine 0.2000000
#> 87                    Late Byzantine 0.2000000
#> 88  Early Byzantine / Late Antiquity 0.3333333
#> 89                  Middle Byzantine 0.3333333
#> 90                    Late Byzantine 0.3333333
#> 91                 Early hellenistic 1.0000000
#> 92                   Middle Imperial 0.1666667
#> 93                     Late Imperial 0.1666667
#> 94                    Late Antiquity 0.1666667
#> 95  Early Byzantine / Late Antiquity 0.1666667
#> 96                  Middle Byzantine 0.1666667
#> 97                    Late Byzantine 0.1666667
#> 98                 Early hellenistic 0.5000000
#> 99                  Late Hellenistic 0.5000000
#> 100                   Late Byzantine 1.0000000
#> 101                   Late Byzantine 1.0000000
#> 102                   Early imperial 0.3333333
#> 103                  Middle Imperial 0.3333333
#> 104                    Late Imperial 0.3333333
#> 105                    Late Imperial 0.3333333
#> 106                   Late Antiquity 0.3333333
#> 107 Early Byzantine / Late Antiquity 0.3333333
#> 108                  Middle Imperial 0.3333333
#> 109                    Late Imperial 0.3333333
#> 110                   Late Antiquity 0.3333333
#> 111                             <NA>        NA
#> 112                   Late Byzantine 0.3333333
#> 113                  Emirates Period 0.3333333
#> 114                          Ottoman 0.3333333
#> 115 Early Byzantine / Late Antiquity 0.3333333
#> 116                 Middle Byzantine 0.3333333
#> 117                   Late Byzantine 0.3333333
#> 118                   Early imperial 0.3333333
#> 119                  Middle Imperial 0.3333333
#> 120                    Late Imperial 0.3333333
#> 121                   Early imperial 0.3333333
#> 122                  Middle Imperial 0.3333333
#> 123                    Late Imperial 0.3333333
#> 124                    Late Imperial 0.5000000
#> 125                   Late Antiquity 0.5000000
#> 126                   Late Antiquity 1.0000000
#> 127                             <NA>        NA
#> 128                   Early imperial 0.3333333
#> 129                  Middle Imperial 0.3333333
#> 130                    Late Imperial 0.3333333
#> 131                Early hellenistic 1.0000000
#> 132                             <NA>        NA
#> 133                Early hellenistic 0.5000000
#> 134                 Late Hellenistic 0.5000000
#> 135 Early Byzantine / Late Antiquity 1.0000000
#> 136 Early Byzantine / Late Antiquity 1.0000000
#> 137                Early hellenistic 0.2000000
#> 138                 Late Hellenistic 0.2000000
#> 139                   Early imperial 0.2000000
#> 140                  Middle Imperial 0.2000000
#> 141                    Late Imperial 0.2000000
#> 142                  Early classical 0.2500000
#> 143                   Late Classical 0.2500000
#> 144                Early hellenistic 0.2500000
#> 145                 Late Hellenistic 0.2500000
#> 146                Early hellenistic 0.2000000
#> 147                 Late Hellenistic 0.2000000
#> 148                   Early imperial 0.2000000
#> 149                  Middle Imperial 0.2000000
#> 150                    Late Imperial 0.2000000
#> 151                Early hellenistic 0.1666667
#> 152                 Late Hellenistic 0.1666667
#> 153                   Early imperial 0.1666667
#> 154                  Middle Imperial 0.1666667
#> 155                    Late Imperial 0.1666667
#> 156                   Late Antiquity 0.1666667
#> 157                Early hellenistic 0.2000000
#> 158                 Late Hellenistic 0.2000000
#> 159                   Early imperial 0.2000000
#> 160                  Middle Imperial 0.2000000
#> 161                    Late Imperial 0.2000000
#> 162                Early hellenistic 0.5000000
#> 163                 Late Hellenistic 0.5000000
#> 164                   Late Classical 1.0000000
#> 165                   Early imperial 0.3333333
#> 166                  Middle Imperial 0.3333333
#> 167                    Late Imperial 0.3333333
#> 168                  Middle Imperial 1.0000000
#> 169 Early Byzantine / Late Antiquity 0.3333333
#> 170                 Middle Byzantine 0.3333333
#> 171                   Late Byzantine 0.3333333
#> 172                   Early imperial 0.2000000
#> 173                  Middle Imperial 0.2000000
#> 174                    Late Imperial 0.2000000
#> 175                   Late Antiquity 0.2000000
#> 176 Early Byzantine / Late Antiquity 0.2000000
#> 177                  Middle Imperial 1.0000000
#> 178                   Early imperial 0.3333333
#> 179                  Middle Imperial 0.3333333
#> 180                    Late Imperial 0.3333333
#> 181                Early hellenistic 0.2500000
#> 182                 Late Hellenistic 0.2500000
#> 183                   Early imperial 0.2500000
#> 184                  Middle Imperial 0.2500000
#> 185                   Late Classical 0.1666667
#> 186                Early hellenistic 0.1666667
#> 187                 Late Hellenistic 0.1666667
#> 188                   Early imperial 0.1666667
#> 189                  Middle Imperial 0.1666667
#> 190                    Late Imperial 0.1666667
#> 191                             <NA>        NA
#> 192                   Early imperial 1.0000000
#> 193                  Recent / Modern 1.0000000
#> 194                   Late Byzantine 0.3333333
#> 195                  Emirates Period 0.3333333
#> 196                          Ottoman 0.3333333
#> 197                Early hellenistic 0.5000000
#> 198                 Late Hellenistic 0.5000000
#> 199                Early hellenistic 0.2000000
#> 200                 Late Hellenistic 0.2000000
#> 201                   Early imperial 0.2000000
#> 202                  Middle Imperial 0.2000000
#> 203                    Late Imperial 0.2000000
#> 204                Early hellenistic 0.5000000
#> 205                 Late Hellenistic 0.5000000
#> 206                   Early imperial 0.2000000
#> 207                  Middle Imperial 0.2000000
#> 208                    Late Imperial 0.2000000
#> 209                   Late Antiquity 0.2000000
#> 210 Early Byzantine / Late Antiquity 0.2000000
#> 211                Early hellenistic 0.5000000
#> 212                 Late Hellenistic 0.5000000
#> 213                Early hellenistic 0.5000000
#> 214                 Late Hellenistic 0.5000000
#> 215                    Late Imperial 1.0000000
#> 216                 Late Hellenistic 1.0000000
#> 217                    Late Imperial 0.3333333
#> 218                   Late Antiquity 0.3333333
#> 219 Early Byzantine / Late Antiquity 0.3333333
#> 220                Early hellenistic 0.2000000
#> 221                 Late Hellenistic 0.2000000
#> 222                   Early imperial 0.2000000
#> 223                  Middle Imperial 0.2000000
#> 224                    Late Imperial 0.2000000
#> 225                             <NA>        NA
#> 226                Early hellenistic 0.3333333
#> 227                 Late Hellenistic 0.3333333
#> 228                   Early imperial 0.3333333
#> 229                  Early classical 1.0000000
#> 230                Early hellenistic 0.1111111
#> 231                 Late Hellenistic 0.1111111
#> 232                   Early imperial 0.1111111
#> 233                  Middle Imperial 0.1111111
#> 234                    Late Imperial 0.1111111
#> 235                   Late Antiquity 0.1111111
#> 236 Early Byzantine / Late Antiquity 0.1111111
#> 237                 Middle Byzantine 0.1111111
#> 238                   Late Byzantine 0.1111111
#> 239                Early hellenistic 0.2000000
#> 240                 Late Hellenistic 0.2000000
#> 241                   Early imperial 0.2000000
#> 242                  Middle Imperial 0.2000000
#> 243                    Late Imperial 0.2000000
#> 244                Early hellenistic 0.2000000
#> 245                 Late Hellenistic 0.2000000
#> 246                   Early imperial 0.2000000
#> 247                  Middle Imperial 0.2000000
#> 248                    Late Imperial 0.2000000
#> 249                Early hellenistic 0.2000000
#> 250                 Late Hellenistic 0.2000000
#> 251                   Early imperial 0.2000000
#> 252                  Middle Imperial 0.2000000
#> 253                    Late Imperial 0.2000000
#> 254 Early Byzantine / Late Antiquity 1.0000000
#> 255                  Early classical 0.5000000
#> 256                   Late Classical 0.5000000
#> 257                 Late Hellenistic 0.2500000
#> 258                   Early imperial 0.2500000
#> 259                  Middle Imperial 0.2500000
#> 260                    Late Imperial 0.2500000
#> 261                    Late Imperial 1.0000000
#> 262                  Early classical 0.5000000
#> 263                   Late Classical 0.5000000
#> 264                Early hellenistic 0.2000000
#> 265                 Late Hellenistic 0.2000000
#> 266                   Early imperial 0.2000000
#> 267                  Middle Imperial 0.2000000
#> 268                    Late Imperial 0.2000000
#> 269                   Early imperial 1.0000000
#> 270                Early hellenistic 0.5000000
#> 271                 Late Hellenistic 0.5000000
#> 272                Early hellenistic 0.5000000
#> 273                 Late Hellenistic 0.5000000
#> 274                   Early imperial 0.3333333
#> 275                  Middle Imperial 0.3333333
#> 276                    Late Imperial 0.3333333
#> 277                 Late Hellenistic 0.5000000
#> 278                   Early imperial 0.5000000
#> 279 Early Byzantine / Late Antiquity 1.0000000
#> 280                   Early imperial 0.3333333
#> 281                  Middle Imperial 0.3333333
#> 282                    Late Imperial 0.3333333
#> 283                   Early imperial 0.3333333
#> 284                  Middle Imperial 0.3333333
#> 285                    Late Imperial 0.3333333
#> 286 Early Byzantine / Late Antiquity 0.3333333
#> 287                 Middle Byzantine 0.3333333
#> 288                   Late Byzantine 0.3333333
#> 289                  Recent / Modern 1.0000000
#> 290                   Late Classical 0.1666667
#> 291                Early hellenistic 0.1666667
#> 292                 Late Hellenistic 0.1666667
#> 293                   Early imperial 0.1666667
#> 294                  Middle Imperial 0.1666667
#> 295                    Late Imperial 0.1666667
```
