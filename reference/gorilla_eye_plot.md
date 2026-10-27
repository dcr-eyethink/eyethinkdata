# Plot eye movements for one trial (or trial type)

Takes eye tracking data that has been processed by
[`gorilla_eye_processing()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_eye_processing.md)
and makes a heat map and/or a path plot of where people looked. You can
add a background image and mark the zones of interest. Gaze position is
plotted in normalised screen coordinates, from 0 to 1 in both
directions.

## Usage

``` r
gorilla_eye_plot(
  data,
  background = NULL,
  zones = NULL,
  plot_types = c("heat", "path"),
  outp = "analysis",
  title = "plot",
  contrast = NULL
)
```

## Arguments

- data:

  The list returned by
  [`gorilla_eye_processing()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_eye_processing.md);
  the gaze samples are taken from its `ed` item (columns `t`, `x` and
  `y`). You will usually want to filter this to a single trial or
  condition first.

- background:

  Optional file name of a .png image to draw behind the gaze data, eg a
  screenshot of the stimulus.

- zones:

  Optional data.table describing zones to draw as boxes, with columns
  `zone_name`, `left`, `right`, `bottom` and `top`. A zone called
  "screen" is ignored.

- plot_types:

  Which plots to make: "heat" for a heat map, "path" for the gaze path.
  Default is both.

- outp:

  Name of a folder to save the plots in, as pdfs (it is created if
  needed). Set to `NULL` or "" to not save.

- title:

  Start of the file name for the saved plots; the plot type is added to
  the end.

- contrast:

  Optional name of a column in `data$ed` used to colour the gaze path,
  eg to compare two conditions.

## Value

A named list of ggplot objects, one for each of `plot_types`.

## See also

[`gorilla_eye_processing()`](https://dcr-eyethink.github.io/eyethinkdata/reference/gorilla_eye_processing.md)
