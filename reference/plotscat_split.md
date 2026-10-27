# Plot a scatterplot of two variables, split by a group

This is called by
[`plotscat()`](https://dcr-eyethink.github.io/eyethinkdata/reference/plotscat.md)
when `z` is a grouping variable. It plots `x` against `y` for the first
two levels of `z`, and tests whether the correlations differ (cocor,
Fisher 1925).

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

  A data frame, one row per participant.

- x:

  Name of the x column.

- y:

  Name of the y column.

- z:

  Name of the column splitting the groups.

- cl:

  2 plots a line for each sub group, 1 plots just the main line.

- condcols:

  Colours for the groups. By default they are chosen by
  [`replacecondcols()`](https://dcr-eyethink.github.io/eyethinkdata/reference/replacecondcols.md).

- results_title:

  Show the statistics in the plot title?

- title_text:

  Optional text for the title.

- outp:

  Not used.

- bayes_factor:

  Calculate the Bayes factor for the difference between groups?

## Value

A list with the plot (`p`) and the statistics. The results for the first
and second group are `sigyz1.*` and `sigyz2.*`.
