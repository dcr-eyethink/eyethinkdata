# Merge and load in data from multiple gorilla downloads

Asks for a folder that can contain multiple data folders, zipped or un
zipped, downloaded from gorilla. It will try and figure out if your
questionnaire data has been downloaded wide or long format, and collate
it accordingly Note that the functions for processing questionnaire data
in this package assume long

## Usage

``` r
data_collator_gorilla(datafolder = NULL, ...)
```

## Arguments

- datafolder:

  the folder with a collection of gorilla downloads

## Value

Returns a list of data.tables for each type (task, questionnaire,
continuous)
