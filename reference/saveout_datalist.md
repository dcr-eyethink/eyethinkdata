# Save a list of data tables to a folder of .csv files

Saves each data.table in a list as a .csv file in a folder, named after
the item in the list. Items that are not data frames, or have no
columns, are skipped. It reverses
[`readin_datalist()`](https://dcr-eyethink.github.io/eyethinkdata/reference/readin_datalist.md).

## Usage

``` r
saveout_datalist(
  data,
  folder = "processed",
  filter_col = NULL,
  filter_value = NULL
)
```

## Arguments

- data:

  A list of data.tables, with names.

- folder:

  The folder to save into (created if needed). Default is "processed".

- filter_col:

  Optional column name. If you give this and `filter_value`, only the
  rows with those values are saved (tables without that column are saved
  in full).

- filter_value:

  The value (or values) of `filter_col` to keep.

## Value

Nothing is returned; the names of the items saved or skipped are
printed.

## See also

[`readin_datalist()`](https://dcr-eyethink.github.io/eyethinkdata/reference/readin_datalist.md),
[`filtered_datalist()`](https://dcr-eyethink.github.io/eyethinkdata/reference/filtered_datalist.md)
