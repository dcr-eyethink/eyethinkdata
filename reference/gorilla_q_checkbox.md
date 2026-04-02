# Scores checkbox items

Takes output from a checkbox questionniare (QB1) in gorilla and returns
data in a wide format with one column per ranked item

## Usage

``` r
gorilla_q_checkbox(data, checkkey = "problem", newcolnames = NULL)
```

## Arguments

- data:

  data list or just the data_q

- checkkey:

  what key was used in QB1 for this item
