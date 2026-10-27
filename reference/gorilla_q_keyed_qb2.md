# Score a gorilla Questionnaire Builder 2 questionnaire using a survey key

Like
[`gorilla_q_keyed()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_keyed.md),
but for questionnaires made with Questionnaire Builder 2. See that
function for how survey keys work and what the columns of the key
(`sum`, `rev`, `adj`, `scaleName`, `Subscore`, `qual` and `ignore`)
mean.

## Usage

``` r
gorilla_q_keyed_qb2(
  data,
  keyout = FALSE,
  qlist = NULL,
  keylist = NULL,
  items = F,
  key_folder = NULL
)
```

## Arguments

- data:

  The list of data from a gorilla import, or just the questionnaire data
  (`data_q`). It has to be in long format.

- keyout:

  Set to `TRUE` to write out a new blank key for each questionnaire,
  from the data.

- qlist:

  The names of the questionnaires (Task.Name in gorilla) to score. If
  `NULL`, all of them.

- keylist:

  Names of the key files to use, if they are not the same as the names
  in `qlist`. Should be the same length as `qlist`.

- items:

  If `TRUE`, also return the individual item scores.

- key_folder:

  Folder where the survey keys are kept. If `NULL`, the folder
  `survey_key` in the working directory is used.

## Value

A data.table with one row per participant (`pid`) and a column for each
scale (and subscale).

## Details

Differences from
[`gorilla_q_keyed()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_keyed.md):
the items are identified by the `Key` column of the QB2 data, only rows
with `Response.Type` "response" are used, duplicated answers are not
removed, and items with `sum = 0` are not returned separately.

## See also

[`gorilla_q_keyed()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_keyed.md),
[`gorilla_q_parse_qb2()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_parse_qb2.md)
