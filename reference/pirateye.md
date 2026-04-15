# Outputs a pirate plot (RDI)

## Usage

``` r
pirateye(
  data,
  colour_condition = NULL,
  x_condition = "variable",
  facet_condition = NULL,
  facet_scales = "fixed",
  facet_row = NULL,
  pid_average = F,
  norm_pid = "no",
  dv,
  reorder = "no",
  pid = "pid",
  dodgewidth = 0.8,
  plot_condition = NULL,
  cond = NULL,
  cond2 = NULL,
  facetby = NULL,
  ylim = NULL,
  xlim = NULL,
  w = NULL,
  h = 6,
  title = NULL,
  outp = "analysis",
  cols = NULL,
  pred_line = F,
  error_bar_data = NULL,
  ypercent = F,
  x_axis = NULL,
  y_axis = NULL,
  error_dim = F,
  error_dim_value = 0,
  pred = NULL,
  pred_means = NULL,
  pred_bar = T,
  xlabs = NULL,
  xlabpos = 0.7,
  error_data = NULL,
  cflip = F,
  norm = F,
  norm_iq = F,
  norm_by = "none",
  norm_scale = "z",
  bars = F,
  violin = T,
  dots = T,
  splitV = F,
  svw = 1,
  dot_h_jitter = 0,
  line = F,
  error_bars = T,
  useall = F,
  legend = T,
  title_overide = F,
  combine_plots = list(),
  combine_position = "right",
  elementinc = NULL,
  type = NULL,
  saveoutdata = F,
  ...
)
```

## Arguments

- data:

  data with one person per line, excluding rows with use=0

- colour_condition:

  colour split

- x_condition:

  x axis split (if not, specified colour condition used for x axis too)

- facet_condition:

  for faceting

- pid_average:

  plot an average of each participant's dv over the named conditions

- norm_pid, :

  DEPRECATED use norm_by and norm_scale (normalise / z score for each
  participant, either "no","z" or "iq")

- dv:

  name of single dv column, or multiple columns, in which case they will
  be split by x_condition unless colour or facet condition set to
  'variable'

- pid:

  whats the name of col that identifies individuals

- plot_condition:

  instead of passing individually, you can give a vector of up to 3

- cols:

  specify the colours to use, can be a set of colours, or of
  condition_level to colour

- error_bar_data:

  error bar data

- ypercent:

  Convert y scale to

  x_axistitles

  y_axistitles

  xlabposHow high vertically should they be, as proportion of plot
  height

  error_datadistribution for mean, eg from Bayes analysis, to replace SE

  cflipflip to horizontal plot

  normDEPRECATED use norm_by and norm_scale (normalise / z-score values
  for comparison across scales when multiple dvs)

  norm_iqDEPRECATED use norm_by and norm_scale

  norm_bynormalise "pid" across each individual (to see within cond
  diffs) or across "dv" measures to compare, or "none"

  norm_scale"z" score, default, or use "iq" scale

  useallignore the use column and plot all rows

  typeshortcuts: m=just error bars, b=just bars

  saveoutdatasave the plot data to a sav file for sharing

  redordercan be "increasing" or "decreasing", default "no"

  xlabDo we have labels to go across x axis, such as post hoc pvalues or
  MPEs

Defaults to violin plot with error bars and dots, but elements such as
dots, bars, violin, error_bars can be turned on or off
