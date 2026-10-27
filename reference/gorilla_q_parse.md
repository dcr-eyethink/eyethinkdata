# Get unscored questionnaire answers from gorilla

Takes the answers from gorilla questionnaires built with Questionnaire
Builder 1 (long format data), and returns them with one row per
participant and one column per question. Use this for answers such as
age and gender that do not need to be scored. For scales that need
scoring, see
[`gorilla_q_keyed()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_keyed.md).

## Usage

``` r
gorilla_q_parse(data, qlist = NULL, preface = NULL, strip = NULL, pd = NULL)
```

## Arguments

- data:

  The list of data from a gorilla import, or just the questionnaire data
  (`data_q`). It has to be in long format.

- qlist:

  The names of the questionnaires (Task.Name in gorilla) to use. If
  `NULL`, all of the questionnaires are processed.

- preface:

  Text to add to the start of all the question names. Use "task" to add
  the name of the questionnaire.

- strip:

  Which version of the answers to drop. "quant" drops the numeric
  `_quantised` columns, keeping the text answers. "qual" drops the text
  answers, and keeps the numeric ones (renamed to the plain question
  name). Default `NULL` keeps both.

- pd:

  Optional data.table of participant data (with a `pid` column). If
  given, the questionnaire answers are merged into it with
  [`pid_merge()`](https://dcr-eyethink.github.io/eyethinkdata/reference/pid_merge.md).

## Value

A data.table with one row per participant (`pid`).

## Details

Gorilla gives you each answer as the text the person chose (or typed),
and for many questions also as a number (the "quantised" version, with
`_quantised` on the end of the question name). Use `strip` to keep one
or the other. If an answer is entirely numbers, it is returned as a
number. If a person answered a question more than once, the later answer
is used and the people affected are listed.

## See also

[`gorilla_q_keyed()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_keyed.md),
[`gorilla_q_parse_qb2()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_q_parse_qb2.md)

## Examples

``` r
if (FALSE) { # \dontrun{
gorilla_q_parse(data, qlist = "basic demographics", strip = "quant")
} # }
```
