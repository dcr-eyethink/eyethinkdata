# Plot scattergram for two or three variables, optionally split by group

this plots x against y if there are two; if z is given and is a number,
plots x against z as well Or z can be is a between subjects factor, will
plot x y for the groups given in z and tests for slope differences

## Usage

``` r
plotscat(
  data,
  x,
  y,
  z = NULL,
  cl = 2,
  condcols = NULL,
  cleand = FALSE,
  results_title = TRUE,
  title_text = NULL,
  outp = "analysis",
  w = 6,
  h = 6,
  label = NULL,
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

  name of column, either numeric DV or a factor for splitting groups

- results_title:

  do you want all the stats in the title?

- title_text:

  optional text for title

- outp:

  name of folder to save plot in, set to blank if no save needed

- bayes_factor:

  do you want bayes factor calculated, needs BayesFactor package
