# Process and analyse IAT data from gorilla

Runs a full analysis on data from an Implicit Association Test (IAT)
that was run in gorilla using the standard template. It picks out the
trial data, calculates each person's mean reaction time in the congruent
and incongruent blocks, and tests the difference.

## Usage

``` r
gorilla_iatanalysis(
  data = NULL,
  outp = "analysis",
  exclude_pids = NULL,
  exclude_items = NULL,
  ...
)
```

## Arguments

- data:

  The list of data from a gorilla import. If missing, you will be asked
  to choose a folder of gorilla downloads.

- outp:

  Name of the folder to save results in (it is created if needed). Set
  to `NULL` or "" to not save anything, and just return the results.

- exclude_pids:

  Vector of participant ids to leave out.

- exclude_items:

  Vector of items (stimuli) to leave out.

- ...:

  Further arguments passed to
  [`plot_model()`](https://dcr-eyethink.github.io/eyethinkdata/reference/plot_model.md),
  and from there to
  [`pirateye()`](https://dcr-eyethink.github.io/eyethinkdata/reference/pirateye.md),
  to change the plots.

## Value

A list. The first (unnamed) item is the output of
[`plot_model()`](https://dcr-eyethink.github.io/eyethinkdata/reference/plot_model.md),
then `ParticipantMeans` (one row per person, with the IAT score) and
`rawdata` (the selected trial data).

## Details

The analysis follows these steps:

1.  Trials are selected where `metadata` is "congruent" or
    "incongruent", at the first attempt, in the display called "trials".
    Incorrect trials and those with reaction times over 3000 ms are
    removed.

2.  Mean reaction times are calculated for each person, and participants
    with a missing condition are dropped. The IAT score is incongruent
    minus congruent.

3.  A paired t-test is run and saved in `IAT_ttest.txt` in `outp`.

4.  A mixed ANOVA is run with block order (`firstblock`) between
    participants and condition within participants. It is plotted and
    printed using
    [`plot_model()`](https://dcr-eyethink.github.io/eyethinkdata/reference/plot_model.md).

5.  The participant means are saved as `ParticipantMeans.csv` in `outp`.

## See also

[`plot_model()`](https://dcr-eyethink.github.io/eyethinkdata/reference/plot_model.md),
[`data_collator_gorilla()`](https://dcr-eyethink.github.io/eyethinkdata/reference/data_collator_gorilla.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- data_collator_gorilla("gorilla_data")
iat <- gorilla_iatanalysis(data, exclude_pids = c(1234567))
iat$ParticipantMeans
} # }
```
