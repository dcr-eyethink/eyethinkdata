# Score ranked items

Takes the output from a ranking question in a gorilla questionnaire
(QB1) and returns the data in wide format, with one column for each item
that was ranked, containing the rank given to it by each participant.

## Usage

``` r
gorilla_q_rankscore(data, rankkey = "rankpredict", keepcols = F, respid = NULL)
```

## Arguments

- data:

  The list of data from a gorilla import, or just the questionnaire data
  (`data_q`).

- rankkey:

  The key that was used for the ranking question in gorilla. Columns
  whose names contain this text are used, and their names should end
  with the rank number (eg `rankpredict_1`).

- keepcols:

  If `TRUE`, merge the ranks into the full original data.

- respid:

  Text to put at the start of the new column names, which are otherwise
  called `rank_<item>`.

## Value

A data.table with the rank of each item.
