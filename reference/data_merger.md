# Load in and merge data from multiple files into one data.table

Reads all of the files of one type in a folder and stacks them into one
data.table, adding a `filename` column to say where each row came from.
It is a general function, but has some options for tidying up gorilla
data.

## Usage

``` r
data_merger(
  ending = "txt",
  contains = "",
  d = ",",
  h = TRUE,
  datafile = NULL,
  gorilla_clean = FALSE,
  gorilla_checkwideq = F,
  datafolder = NULL
)
```

## Arguments

- ending:

  The end of the file names, ie the file type. Default is "txt"; use
  "csv", "xlsx" or "xls" as needed.

- contains:

  Text that must appear in the file name, eg "task". Default is "" which
  accepts any name.

- d:

  The delimiter (separator) used in the data files. Default is a comma.

- h:

  Do the data files have a header row of column names? Default `TRUE`.

- datafile:

  One example file from the folder you want to read. If supplied, you
  will not be asked to choose a file.

- gorilla_clean:

  If `TRUE`, discard a set of gorilla columns that are rarely needed
  (timestamps, device information, etc.) to make the data more
  manageable.

- gorilla_checkwideq:

  For gorilla questionnaire data. If `TRUE`, files that turn out to be
  wide format questionnaire data are merged side by side (matched on
  `Participant.Private.ID`) rather than stacked.

- datafolder:

  The folder containing the files. If given, this is used instead of
  `datafile`.

## Value

A data.table of all of the files. If the data has a `Reaction.Time`
column, a numeric copy of it called `rt` is added.

## Details

You can either name the folder (`datafolder`) or give an example file
from that folder (`datafile`). If you give neither, you will be asked to
choose an example file, and the function will use all of the matching
files in the same folder.

Files are picked out if their name contains the text in `contains` and
ends with `ending` (upper and lower case are ignored; `contains` can be
a regular expression). Empty files (4 bytes or less) and the temporary
`~$` files that Excel makes are skipped. For Excel files, only the first
sheet is read.

If files have different columns, the missing columns are filled with
`NA`.

## See also

[`data_merger_gorilla()`](https://dcr-eyethink.github.io/eyethinkdata/reference/data_merger_gorilla.md),
[`data_collator_gorilla()`](https://dcr-eyethink.github.io/eyethinkdata/reference/data_collator_gorilla.md)

## Examples

``` r
if (FALSE) { # \dontrun{
all_csvs <- data_merger(ending = "csv", contains = "task", datafolder = "my_folder")
} # }
```
