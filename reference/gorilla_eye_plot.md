# Takes one trial (or trial type) of data and optional image file an makes heat map

Generates plots from imported gorilla data that has been processed by
gorilla_eye_processing

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

  gorilla ET data

- background:

  for background

## Value

image
