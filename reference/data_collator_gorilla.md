# Merge and load in data from multiple gorilla downloads

Reads in the raw data from one or more downloads from the gorilla
experiment platform and collates them into a single list of data.tables.
This is usually the first function to run in an analysis of gorilla
data.

## Usage

``` r
data_collator_gorilla(datafolder = NULL, ...)
```

## Arguments

- datafolder:

  The folder containing the gorilla downloads. If left as `NULL`, you
  will be asked to choose a folder.

- ...:

  Further arguments passed on to
  [`data_merger_gorilla()`](https://dcr-eyethink.github.io/eyethinkdata/reference/data_merger_gorilla.md),
  for example `gorilla_clean = TRUE` to drop some of the less useful
  gorilla columns.

## Value

A list with these items:

- data_task:

  data.table of all the task (experiment) data, with added columns `pid`
  (anonymous participant id, a factor), `lid` (public id), and `rt`
  (numeric reaction time)

- data_q:

  data.table of the questionnaire data

- data_qtype:

  "long", "wide" or "none"

- data_continuous:

  data.table of mouse or eye tracking data from the `uploads` folder, if
  there is any

- uploads:

  table of the names and paths of any other uploaded files

- task_names:

  the names of the tasks found

- q_names:

  the names of the questionnaires found

## Details

`datafolder` should be a folder that contains the downloads from
gorilla. Each download can either be a .zip file, or an already unzipped
folder that contains the gorilla .csv files. The data from all of the
downloads are stacked together.

**Note:** any .zip files found in `datafolder` are unzipped into a
folder with the same name, and the .zip file is then deleted.

Each download folder is processed by
[`data_merger_gorilla()`](https://dcr-eyethink.github.io/eyethinkdata/reference/data_merger_gorilla.md),
and `...` is passed on to that function (and from there to
[`data_merger()`](https://dcr-eyethink.github.io/eyethinkdata/reference/data_merger.md)).

The questionnaire data can be downloaded from gorilla in wide (one row
per participant) or long format. This function works out which you have
and reports it in `data_qtype`. The questionnaire functions in this
package (`gorilla_q_...`) need long format.

The names of the tasks and questionnaires found are printed to the
console, so that you can copy them into later analysis code.

## See also

[`data_merger_gorilla()`](https://dcr-eyethink.github.io/eyethinkdata/reference/data_merger_gorilla.md),
[`data_merger()`](https://dcr-eyethink.github.io/eyethinkdata/reference/data_merger.md),
[`pid_merge()`](https://dcr-eyethink.github.io/eyethinkdata/reference/pid_merge.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- data_collator_gorilla("gorilla_data")
head(data$data_task)
} # }
```
