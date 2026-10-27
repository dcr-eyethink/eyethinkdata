# Merge data tables together on participant id

Takes two or more data tables and joins them up, matching rows by `pid`
(participant id) or whichever other column(s) you name in `link`. For
example, you can add trial data to participant data, or add
questionnaire scores to a table of reaction times.

## Usage

``` r
pid_merge(..., link = "pid")
```

## Arguments

- ...:

  Two or more data.tables (or data frames) to join.

- link:

  The name or names of the column(s) to match on. Default "pid".

## Value

A data.table, with the columns of the first table followed by the new
columns of each later one.

## Details

All of the rows of the *first* table are kept. Rows in later tables that
match no row in the first are dropped, and rows of the first table with
no match get `NA`. Where there is more than one row in a later table for
a participant, the first table's rows are repeated for each of them.

If a later table has columns with the same name as those that already
exist (other than the link columns), the later values replace the
earlier ones, and the repeated names are printed.

The link columns are converted to factors in the result (so that, for
example, numeric and text versions of an id still match).

**Note** that data frames you pass in are converted to data.tables in
place (by reference), as with
[`data.table::setDT`](https://rdrr.io/pkg/data.table/man/setDT.html).

## See also

[`data_collator_gorilla()`](https://dcr-eyethink.github.io/eyethinkdata/reference/data_collator_gorilla.md)

## Examples

``` r
a <- data.table::data.table(pid = 1:3, age = c(20, 25, 30))
b <- data.table::data.table(pid = c(1, 2, 4), meanRT = c(500, 450, 600))
pid_merge(a, b)
#>       pid   age meanRT
#>    <fctr> <num>  <num>
#> 1:      1    20    500
#> 2:      2    25    450
#> 3:      3    30     NA
```
