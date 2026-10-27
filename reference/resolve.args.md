# Combine argument lists, with earlier ones taking priority

A helper for passing arguments on to other functions, so you can set
some defaults while letting users override them. Joins `PRE.ARGS`, then
the arguments in `...`, then `POST.ARGS`. If a name appears more than
once, the first one is used.

## Usage

``` r
resolve.args(..., PRE.ARGS = list(), POST.ARGS = list())
```

## Arguments

- ...:

  Named arguments.

- PRE.ARGS:

  A list of arguments that take priority over everything else.

- POST.ARGS:

  A list of arguments that are used only if the name is not given in
  `PRE.ARGS` or `...`.

## Value

A list of arguments, with no repeated names.

## Examples

``` r
resolve.args(a = 1, POST.ARGS = list(a = 2, b = 3))
#> $a
#> [1] 1
#> 
#> $b
#> [1] 3
#> 
# a = 1, b = 3
```
