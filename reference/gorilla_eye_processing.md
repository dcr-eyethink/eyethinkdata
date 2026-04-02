# TASK BUILDER 2 version: Takes all data from gorilla import, gets eye movement data and labels with zone info

TASK BUILDER 2 version: Takes all data from gorilla import, gets eye
movement data and labels with zone info

Takes all data from gorilla import, filters for named displays and
returns simplified data,

## Usage

``` r
gorilla_eye_processing(
  data,
  td = NULL,
  link = c("pid", "sid"),
  zone = "gorilla",
  conf_threshold = 0.5
)

gorilla_eye_processing(
  data,
  td = NULL,
  link = c("pid", "sid"),
  zone = "gorilla",
  conf_threshold = 0.5
)
```

## Arguments

- data:

  compiled data from a gorilla import

- td:

  if trail data already generated, will add eye info to this

## Value

eye data with simplified data

eye data with simplified data
