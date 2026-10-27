# Score the TIPI ten item personality scale

Scores the Ten Item Personality Inventory (Gosling, Rentfrow & Swann,
2003) from long format gorilla questionnaire data.

## Usage

``` r
gorilla_q_tipi(data, qname = c("Big 5 ten items", "Big 5 Personality", "TIPI"))
```

## Arguments

- data:

  The list of data from a gorilla import, or just the questionnaire data
  (`data_q`).

- qname:

  The questionnaire name or names (Task.Name in gorilla) to look for. By
  default, "Big 5 ten items", "Big 5 Personality" and "TIPI".

## Value

A data.table with one row per participant (`pid`) and the five scores:
`TIPI_O` (openness), `TIPI_C` (conscientiousness), `TIPI_E`
(extroversion), `TIPI_A` (agreeableness) and `TIPI_N` (emotional
stability).

## Details

The questionnaire should have `*_score` questions (agreeableness_score,
conscientiousness_score, emotional_stability_score, extroversion_score,
openness_score), as in the gorilla template. People who answered the
questionnaire more than once are listed and left out of the results.

## See also

[`gorilla_q_keyed()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_keyed.md)
