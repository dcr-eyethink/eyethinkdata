# Score a gorilla questionnaire using a survey key

Processes questionnaires made with Questionnaire Builder 1 (long format
data). A survey key is a .csv file that tells this function how to score
each item, which items to reverse, and which items belong to which scale
or subscale. The result has one row per participant.

## Usage

``` r
gorilla_q_keyed(
  data,
  keyout = FALSE,
  qlist = NULL,
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
  from the data. This replaces any existing key in `survey_key`.

- qlist:

  The names of the questionnaires (Task.Name in gorilla) to score. If
  `NULL`, all of the questionnaires in the data.

- items:

  If `TRUE`, also return the individual item scores (after reversing
  etc.), one column for each item.

- key_folder:

  Folder where the survey keys are kept. If `NULL`, the folder
  `survey_key` in the working directory is used.

## Value

A data.table with one row per participant (`pid`) and a column for each
scale (and subscale), and for any items that were left out of scoring.
If nothing could be scored, there is no return value.

## Details

**How to use it**

1.  The first time, run with `keyout = TRUE`. This writes a blank key
    for each questionnaire in `qlist` to a folder called `survey_key` in
    your working directory.

2.  Edit the key in Excel or a text editor, and save it as a .csv file.
    There is one key per questionnaire in gorilla, named after the
    questionnaire.

3.  Run the function again (with `keyout = FALSE`). It looks for the key
    in `key_folder`, or if that is not given in the folder `survey_key`.

Each row of the key is one item, identified by the `Question.Key` (what
you typed in the "key" box when you made the questionnaire in gorilla).
The columns you can edit are:

- `sum` - how the item is used in the score: 1 to add it, -1 to reverse
  score it, and 0 to leave it out of the scores (it is returned by
  itself in its own column).

- `rev` - for reverse scored items, the answer is subtracted from this
  number. eg for a 7 point scale, use 8 (the default).

- `adj` - a number added to the item, if the scale needs adjusting (eg
  it starts at 0 instead of 1). Optional: keys without this column are
  treated as 0.

- `scaleName` - the name of the scale that the item adds to. The output
  has a column with this name. One questionnaire can have many scales.

- `Subscore` - optional name of a subscale. It is returned as an extra
  column `<scaleName>_<Subscore>`.

- `qual` - put 1 here if the item is a text (qualitative) answer. It is
  not scored but returned in its own column.

- `ignore` - an answer that should be ignored when scoring, eg 8 for
  "not applicable" on a scale of 1 to 7.

Scales are the *sum* of the items (not the mean). People who did not
answer all the items are listed.

Questionnaires without a key are skipped, unless you make one with
`keyout = TRUE`. If you only want to look at the data without scoring
it, see
[`gorilla_q_parse()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_parse.md).

## See also

[`gorilla_q_keyed_qb2()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_keyed_qb2.md),
[`gorilla_q_parse()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_parse.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# make a blank key
gorilla_q_keyed(data, qlist = "Interpersonal Reactivity Index (Davies, 1983)", keyout = TRUE)
# ... edit the key in survey_key, then score:
gorilla_q_keyed(data, qlist = "Interpersonal Reactivity Index (Davies, 1983)")
} # }
```
