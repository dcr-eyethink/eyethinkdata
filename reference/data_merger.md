# load in and merge data from multiple files into one data.table

It asks for one of the files in the DATA folder and merges all the files
in there strung together in one data table

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

  string at the end of the filename, ie filetype.

- contains:

  string to identify datafile, eg 'task'

- d:

  delimiter for data, default comma

- h:

  are there headers in data files?

- datafile:

  example file. if this is supplied then don't ask user

- datafolder:

  give the file location of a folder instead of eg file

## Value

A data table of all datafiles ending in `ending` containing `contains`.
