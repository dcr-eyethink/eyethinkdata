# Single correlation with r, p value and optionally a Bayes factor

Correlates two columns of a data frame, and returns Pearson's r with its
p value, and Kendall's tau as a rank based alternative. Optionally, it
adds a Bayes factor (using the BayesFactor package).

## Usage

``` r
singlecor(data, x, y, bayes_factor = F)
```

## Arguments

- data:

  A data frame with the two columns to correlate.

- x:

  Name of the first column.

- y:

  Name of the second column.

- bayes_factor:

  Calculate a Bayes factor? Needs the BayesFactor package.

## Value

A list with `r`, `p` (both rounded to 3 places), `bf` (the Bayes factor
for the correlation, rounded down to a whole number, or 0 if not
requested) and `k.tau` (Kendall's tau).

## See also

[`plotscat()`](https://dcr-eyethink.github.io/eyethinkdata/reference/plotscat.md),
[`exclorr()`](https://dcr-eyethink.github.io/eyethinkdata/reference/exclorr.md)

## Examples

``` r
singlecor(mtcars, "mpg", "hp")
#> Warning: cannot compute exact p-value with ties
#> $r
#>    cor 
#> -0.776 
#> 
#> $p
#> [1] 0
#> 
#> $bf
#> [1] 0
#> 
#> $k.tau
#>    tau 
#> -0.743 
#> 
```
