# Explore correlations between large numbers of variables

Takes a data set and two lists of column names, and correlates every
variable in the first list with every variable in the second. It returns
the r values, the p values, and a table showing only the r values that
are significant. It can also draw a correlation matrix plot, and
calculate Bayes factors.

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

  A data frame or data.table with one row per participant.

- var1:

  Vector of column names. These will be the rows of the output.

- var2:

  Vector of column names. These will be the columns of the output.

- bayes_factor:

  Do you want Bayes factors for each correlation? Needs the BayesFactor
  package, and can be slow for large numbers of variables.

- plim:

  Cut off for which p-values count as significant. Default 0.05.

- cplot:

  Make a correlation matrix plot (circles for the significant
  correlations)? If `TRUE` it is saved as a pdf in `outp`.

- cplot_title:

  Title for the plot, which is also used as its file name.

- outp:

  Name of a folder in which to save the plot (it will be created if it
  does not exist). If `NULL` or "", the plot is drawn on the current
  device rather than saved.

## Value

A list with data frames `corrsig` (r values, rounded, with
non-significant ones set to 0), `pvalues` and `rvalues`. Also `plot` if
`cplot = TRUE`, and `bfs` (Bayes factors, rounded down to a whole
number) if `bayes_factor = TRUE`.

## Details

Correlations are Pearson's r, calculated on all the pairs of values
available ([`Hmisc::rcorr`](https://rdrr.io/pkg/Hmisc/man/rcorr.html)).
Spaces and hyphens in the variable names are converted to full stops, as
R does when it reads in column names. If any of the names you give are
not columns in `data`, the function prints a message and returns the
missing names instead of results.

## Examples

``` r
if (FALSE) { # \dontrun{
exclorr(mtcars, var1 = c("mpg", "disp"), var2 = c("hp", "wt", "qsec"))
} # }
```
