# Get colours for the levels of a condition

Returns a set of default colours for a vector of condition names. If the
names are themselves colours (eg "red", "blue" or "#FF0000"), those
colours are used instead for those levels.

## Usage

``` r
replacecondcols(condlevels)
```

## Arguments

- condlevels:

  The levels of a factor, eg `levels(data$condition)`.

## Value

A vector of colours, in the same order as `condlevels`.

## Examples

``` r
replacecondcols(c("red", "blue", "green"))
#> [1] "red"   "blue"  "green"
replacecondcols(c("low", "high"))
#> [1] "#F8766D" "#00BFC4"
```
