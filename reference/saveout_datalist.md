# saves out data tables in a list to a folder containing .csv files

saves out data tables in a list to a folder containing .csv files

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

  list of data.tables

- folder:

  to save into

- filter_col:

  save out a subset of data, based on this column

- filter_value:

  using this value as filter
