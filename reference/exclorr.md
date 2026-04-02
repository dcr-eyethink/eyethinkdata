# Explore correlations between large numbers of DVs

Takes a data set and two lists of DVs looks at correlations between one
set and another r values, p values and bayes factors

## Usage

``` r
exclorr(
  data,
  var1,
  var2,
  bayes_factor = F,
  plim = 0.05,
  cplot = F,
  cplot_title = "Correlation Matrix",
  outp = "analysis"
)
```

## Arguments

- data:

  data

- var1:

  list of DV names that will be rows

- var2:

  list of DV names that will be cols

- bayes_factor:

  do you want bayes analysis? banal

- plim:

  cut off for p-value reporting

- cplot:

  generate a correlation matrix plot

- outp:

  name of folder to save plot in, set to blank if no save needed
