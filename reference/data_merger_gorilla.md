# Load in and merge the data from one gorilla download

Reads one unzipped gorilla download folder. The task data and
questionnaire data are loaded and merged separately, and any uploaded
files (for example mouse or eye tracking data) are loaded too. You will
usually use
[`data_collator_gorilla()`](https://dcr-eyethink.github.io/eyethinkdata/reference/data_collator_gorilla.md),
which calls this function on every download in a folder.

## Usage

``` r
data_merger_gorilla(datafile = NULL, ending = "csv", ...)
```

## Arguments

- datafile:

  An example file from the unzipped download folder. If `NULL`, you will
  be asked to choose one.

- ending:

  The file type to read, default "csv".

- ...:

  Further arguments passed on to
  [`data_merger()`](https://dcr-eyethink.github.io/eyethinkdata/reference/data_merger.md),
  eg `gorilla_clean = TRUE`.

## Value

A list with items `data_task`, `data_q`, `data_qtype` ("long", "wide" or
"none"), `data_continuous` (tracking data from the uploads folder) and
`uploads` (a table of other uploaded files). See
[`data_collator_gorilla()`](https://dcr-eyethink.github.io/eyethinkdata/reference/data_collator_gorilla.md)
for details.

## Details

You can give any file from the unzipped download folder as `datafile`,
and the rest are found from that. If you give nothing, you will be asked
to choose a file. All the files with "task" in their name are merged to
make the task data, and all those with "quest" in their name make the
questionnaire data.

Adds a column `pid` (the anonymised participant id, as a factor, from
`Participant.Private.ID`) and `lid` (from `Participant.Public.ID`). The
questionnaire data is checked to see if it is in long or wide format. In
the questionnaire data, hyphens in `Question.Key` are changed to
underscores.

## See also

[`data_collator_gorilla()`](https://dcr-eyethink.github.io/eyethinkdata/reference/data_collator_gorilla.md),
[`data_merger()`](https://dcr-eyethink.github.io/eyethinkdata/reference/data_merger.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- data_merger_gorilla("gorilla_data/my_download/data_exp_1.csv")
} # }
```
