# Filter a whole list of data tables on one column

Goes through a list of data.tables (such as that returned by
[`data_collator_gorilla()`](https://dcr-eyethink.github.io/eyethinkdata/reference/data_collator_gorilla.md))
and keeps only the rows where the column `filter_col` has one of the
values in `filter_value`. Tables in the list that do not have a column
with that name are left as they are.

## Usage

``` r
filtered_datalist(data, filter_col = NULL, filter_value = NULL)
```

## Arguments

- data:

  A list of data.tables.

- filter_col:

  Name of the column to filter on, eg "pid".

- filter_value:

  The value (or vector of values) to keep.

## Value

The list of data.tables, filtered.

## Details

If either `filter_col` or `filter_value` is missing, the list is
returned unchanged.

## Examples

``` r
if (FALSE) { # \dontrun{
# keep only the data for two participants, in every table that has a pid column
two_people <- filtered_datalist(data, filter_col = "pid", filter_value = c(101, 102))
} # }
```
