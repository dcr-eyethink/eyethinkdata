# Score checkbox items

Takes the output from a checkbox question in a gorilla questionnaire
(QB1, long format) and converts it so that there is one column for each
checkbox option, scored 1 if it was ticked and 0 if it was not.

## Usage

``` r
gorilla_q_checkbox(data, checkkey = "problem", newcolnames = NULL)
```

## Arguments

- data:

  The list of data from a gorilla import, or just the questionnaire data
  (`data_q`).

- checkkey:

  The key that was used for this item in the gorilla questionnaire.
  Columns whose names contain this text are used (but those with
  "\_text" in the name are ignored).

- newcolnames:

  Optional vector of names for the new columns, one for each checkbox
  option. If `NULL`, the names are made from the text of the option.

## Value

A data.table with the checkbox columns recoded as 1 or 0, and renamed
starting with `checkkey`.
