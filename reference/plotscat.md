# Plot a scatterplot of two or three variables, optionally split by group

Plots `x` against `y`, with a regression line, and puts the correlation
statistics in the plot title. What happens depends on `z`:

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

  A data frame with the columns to be plotted, one row per participant.

- x:

  Name of the column for the x axis.

- y:

  Name of the column for the y axis.

- z:

  Optional name of a third column, either numeric or a grouping factor.

- cl:

  For a grouping factor: 2 draws a regression line for each group, 1
  draws one line for everyone.

- condcols:

  Colours for the groups when `z` is a grouping factor. By default they
  are chosen by
  [`replacecondcols()`](https://dcr-eyethink.github.io/eyethinkdata/reference/replacecondcols.md).

- cleand:

  If `TRUE`, remove rows with missing values first.

- results_title:

  Show the statistics in the plot title?

- title_text:

  Optional extra text for the top of the title.

- outp:

  Name of the folder in which to save the plot as a pdf (it is created
  if needed). Set to `NULL` or "" to not save.

- w:

  Width of the saved plot, in inches.

- h:

  Height of the saved plot, in inches.

- label:

  Optional name of a column to use to label each point.

- bayes_factor:

  Calculate Bayes factors? This needs the BayesFactor package. The Bayes
  factors are shown in the plot title.

## Value

A list containing the plot (`p`) and the statistics: r, p value, Bayes
factor and Kendall's tau for each correlation, the p value for the
difference between the correlations (`cc`), and the interaction Bayes
factor (`intbf`), when relevant.

## Details

- no `z`: just `y` against `x`.

- `z` is a number: `y` and `z` are both plotted against `x`, in
  different colours, and the two correlations are compared, using cocor
  (Hittner et al., 2003).

- `z` is a factor or text: `y` against `x` is plotted separately for
  each group in `z` (only the first two groups are used), and the two
  correlations are compared (see `plotscat_split`).

## See also

[`singlecor()`](https://dcr-eyethink.github.io/eyethinkdata/reference/singlecor.md),
[`exclorr()`](https://dcr-eyethink.github.io/eyethinkdata/reference/exclorr.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plotscat(mtcars, x = "mpg", y = "hp", outp = NULL)
plotscat(mtcars, x = "mpg", y = "hp", z = "am", outp = NULL)
} # }
```
