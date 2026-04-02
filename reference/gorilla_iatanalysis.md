# processes IAT data from gorilla

Runs a full analysis on IAT data from a gorilla experiment that is based
on standard template. You can pass arguments to mypirate that is drawing
the main plot

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

  data list from gorilla import, if missing, I'll ask fora folder of
  gorilla downloads

- outp:

  output foldername

- exclude_pids:

  get rid of these guys

- exclude_items:

  get rid of these guys in items column

- ...:

  pass to mypirate for plotting
