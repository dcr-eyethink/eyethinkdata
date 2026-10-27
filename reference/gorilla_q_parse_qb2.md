# Get unscored questionnaire answers from gorilla (Questionnaire Builder 2)

Like
[`gorilla_q_parse()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_parse.md),
but for questionnaires made with Questionnaire Builder 2. It returns the
answers with one row per participant and one column per question.

## Usage

``` r
gorilla_q_parse_qb2(
  data,
  qlist = NULL,
  preface = NULL,
  strip = NULL,
  pd = NULL
)
```

## Arguments

- data:

  The list of data from a gorilla import, or just the questionnaire data
  (`data_q`). It has to be in long format.

- qlist:

  The names of the questionnaires (Task.Name in gorilla) to use. If
  `NULL`, all of them.

- preface:

  Text to add to the start of all the question names. Use "task" to add
  the name of the questionnaire.

- strip:

  "quant" drops the numeric `-quantised` columns, "qual" drops the text
  answers and keeps the numeric ones. Default `NULL` keeps both.

- pd:

  Optional data.table of participant data (with a `pid` column) to merge
  the answers into.

## Value

A data.table with one row per participant (`pid`).

## Details

In this version, if a person gave the same answer to a question more
than once, the *first* is kept (in
[`gorilla_q_parse()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_parse.md)
it is the last), and the duplicates are printed. "other" response
columns are dropped, and `_value` is removed from the column names.

## See also

[`gorilla_q_parse()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_parse.md),
[`gorilla_q_keyed_qb2()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_keyed_qb2.md)
