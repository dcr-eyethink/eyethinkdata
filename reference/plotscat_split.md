# Plot scattergram for two variables, split by a group

this plots x against y, split by z and it will plot x y for the two
groups and tests for slope differences

## Usage

``` r
plotscat_split(
  data,
  x,
  y,
  z,
  cl = 2,
  condcols = NULL,
  results_title = TRUE,
  title_text = NULL,
  outp = "",
  bayes_factor = F
)
```

## Arguments

- data:

  data frame with two cols to be correlated

- x:

  name of x column

- y:

  name of y column

- z:

  name of column, a factor splitting groups

- cl:

  =2 plots the sub groups, =1 plots just main.

- condcols:

  colours for two groups
