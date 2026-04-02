# Merge and load in data from a gorilla download

It asks for one of the files in the un zipped DATA folder and loads in
the task and the questionnaire data separately It will try and figure
out if your questionnaire data is wide or long and collate it
accordingly Note that the functions for processing questionnaire data in
this package assume long

## Usage

``` r
data_merger_gorilla(datafile = NULL, ending = "csv", ...)
```

## Arguments

- datafile:

  example file. if this is supplied then don't ask user

- ending:

  string at the end of the filename, ie filetype.

## Value

A list of two or three data tables of gorilla data.
