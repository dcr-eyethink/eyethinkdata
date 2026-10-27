# Process gorilla eye tracking data and label it with zone information

For gorilla Task Builder 2 data with eye tracking. Takes the data from a
gorilla import, picks out the gaze predictions and the zones that were
defined on each trial, and works out for each trial how long, how much
and how soon people looked at each zone.

## Usage

``` r
gorilla_eye_processing(data, td = NULL, conf_threshold = 0.5)
```

## Arguments

- data:

  The list of data from a gorilla import.

- td:

  Optional trial level data.table (with `pid`, `sid` and `tid` columns).
  If given, the eye measures are merged into it.

- conf_threshold:

  Not currently used.

## Value

The list `data`, with `ed`, `zd` and `td` added.

## Details

The function needs `data$data_continuous` (see
[`data_collator_gorilla()`](https://dcr-eyethink.github.io/eyethinkdata/reference/data_collator_gorilla.md))
to contain the gorilla eye tracking columns. It adds three items to the
list:

- `ed` - the gaze samples (`pid`, `sid`, `tid`, time `t`, gaze position
  `x` and `y` and the `duration` of each sample) with a 0/1 column for
  each zone to say if the gaze was inside it.

- `zd` - the coordinates of each zone on each trial.

- `td` - one row per trial, with three columns for each zone:
  `<zone>_ftot` (total time looking at it), `<zone>_ftot_prop` (that as
  a proportion of the trial time) and `<zone>_fons` (time of the first
  look).

## See also

[`gorilla_eye_plot()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_eye_plot.md),
[`gorilla_eye_processing_old()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_eye_processing_old.md)
for the earlier gorilla format
