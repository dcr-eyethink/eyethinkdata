# Process gorilla eye tracking data (earlier gorilla format)

The earlier version of
[`gorilla_eye_processing()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_eye_processing.md),
kept so that old scripts still work. It works on the older gorilla eye
tracking columns (`type`, `x_pred_normalised`, `zone_name` etc.), and
gives one row per file (`filename`) rather than per trial. For current
Task Builder 2 data use
[`gorilla_eye_processing()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_eye_processing.md).

## Usage

``` r
gorilla_eye_processing_old(
  data,
  td = NULL,
  link = c("pid", "sid"),
  zone = "gorilla",
  conf_threshold = 0.5
)
```

## Arguments

- data:

  The list of data from a gorilla import (or the continuous data table
  itself).

- td:

  Optional trial level data.table. If given, the eye measures are merged
  into it using the columns in `link`.

- link:

  The columns to merge `td` on, default `pid` and `sid`.

- zone:

  Not currently used.

- conf_threshold:

  Minimum face confidence (`face_conf`) for a sample to count towards
  the time spent in a zone.

## Value

A list with `trial_data`, `eye_data` and `zone_data`.
