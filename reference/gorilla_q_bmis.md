# Score the BMIS mood survey

Scores the Brief Mood Introspection Scale (BMIS, Mayer & Gaschke, 1988)
from long format gorilla questionnaire data. It looks for questionnaires
with "BMIS" in their name.

## Usage

``` r
gorilla_q_bmis(data, full = F)
```

## Arguments

- data:

  The list of data from a gorilla import, or just the questionnaire data
  (`data_q`).

- full:

  If `TRUE`, also return the individual item scores (with names starting
  `BMIS_Q_`). Default is `FALSE`, which returns only the four scores.

## Value

A data.table with one row per participant (`pid`), and the scores.

## Details

Responses are converted to numbers from 1 to 4. The four scores are:
`BMIS_pu` (pleasant-unpleasant), `BMIS_ac` (arousal-calm), `BMIS_pt`
(positive-tired) and `BMIS_nr` (negative-relaxed). If someone answered
an item more than once, their last answer is used.

## See also

[`gorilla_q_keyed()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_keyed.md)
for scoring other questionnaires.
