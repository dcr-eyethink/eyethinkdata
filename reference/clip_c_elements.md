# Copy a vector to the clipboard as R code

Takes a vector of values and puts a string like `c("a", "b", "c")` on
the clipboard, ready to paste into a script. Handy for turning column
names or file names into a vector you can edit.

## Usage

``` r
clip_c_elements(x, containing = NULL)
```

## Arguments

- x:

  A vector of values (they are converted to text and quoted).

- containing:

  Optional text pattern (a regular expression). If given, only the
  elements of `x` that match it are copied.

## Value

Nothing is returned; called for its side effect of writing to the
clipboard. Needs the clipr package to be installed.

## Examples

``` r
if (FALSE) { # \dontrun{
clip_c_elements(colnames(mtcars), containing = "^c")
} # }
```
