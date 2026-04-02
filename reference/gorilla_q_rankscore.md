# Scores ranked items

Takes output from a ranking questionniare (QB1) in gorilla and returns
data in a wide format with one column per ranked item

## Usage

``` r
gorilla_q_rankscore(data, rankkey = "rankpredict", keepcols = F, respid = NULL)
```

## Arguments

- data:

  data list or just the data_q

- rankkey:

  what key was used in QB1 for this item

- respid:

  word to append to column names
