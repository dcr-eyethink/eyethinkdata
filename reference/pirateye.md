# Plot data as a pirate plot (RDI: raw data, descriptives, inferences)

Makes a plot that shows the raw data points, a violin (the
distribution), the mean, and error bars for the mean, for the conditions
of an experiment. By default it draws all of those things, but you can
turn on and off each element (`dots`, `violin`, `bars`, `line`,
`error_bars`) and change the layout. Plots are saved as a pdf in `outp`,
and returned as a ggplot that you can customise further. `mypirate` is
an older name for the same function.

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
  col_pc_change = F,
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
  display_values = F,
  ...
)
```

## Arguments

- data:

  A data frame or data.table (converted to a data.table in place).

- colour_condition:

  Name of the column to split by colour.

- x_condition:

  Name of the column for the x axis. If not given, the colour condition
  is used.

- facet_condition:

  Name of the column that makes separate panels.

- facet_scales:

  Scales for the panels: "fixed" (the same), "free" or "free_y" etc.
  (see
  [`ggplot2::facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)).

- facet_row:

  Number of rows of panels.

- pid_average:

  Average each participant's dv over the conditions, before plotting?

- dv:

  Name of the column to plot, or a vector of names of columns.

- reorder:

  "increasing" or "decreasing" to put the conditions in order of their
  means. Default "no".

- pid:

  Name of the column that identifies participants. Default "pid".

- dodgewidth:

  How far apart to put the colour conditions within each x position.

- plot_condition:

  A vector of up to three condition names, used instead of
  `colour_condition`, `x_condition` and `facet_condition` (in that
  order).

- cond, cond2, facetby:

  Old names for `x_condition`, `colour_condition` and `facet_condition`.

- ylim:

  Limits for the y axis, as a vector of two numbers.

- xlim:

  Limits for the x axis.

- w:

  Width of the saved plot in inches. By default this is chosen from the
  number of conditions.

- h:

  Height of the saved plot in inches.

- title:

  Title of the plot.

- outp:

  Name of the folder for saving the plot as a pdf (it is created if
  needed). Set to `NULL` or "" to not save.

- cols:

  The colours to use: a vector of colours, or a named vector matching
  condition levels to colours. If the condition levels are themselves
  colour names, those are used.

- pred_line:

  Draw `pred_means` as a line rather than as bars?

- error_bar_data:

  A table of ymin and ymax values to use for the error bars, instead of
  calculating standard errors from the data, with the same condition
  columns as the plot. Used by
  [`plot_model()`](https://dcr-eyethink.github.io/eyethinkdata/reference/plot_model.md).

- ypercent:

  Show the y axis as a percentage?

- x_axis:

  Label for the x axis (default is the condition name).

- y_axis:

  Label for the y axis (default is the dv name).

- error_dim:

  Fade out the data for the conditions whose mean (with SE) excludes
  `error_dim_value`.

- error_dim_value:

  The value used by `error_dim`.

- pred:

  Optional table of predicted data (eg samples from a model), shown as
  violins.

- pred_means:

  Optional table of predicted means, shown as bars or lines.

- pred_bar:

  Draw `pred_means` as bars? Default `TRUE`.

- xlabs:

  A table of labels to show above the x axis, such as post hoc p-values.
  It needs a column called `lab`, and the same x condition column as the
  plot.

- xlabpos:

  How high up the labels in `xlabs` go, as a proportion of the height of
  the plot.

- col_pc_change:

  Calculate the percentage change from the first to the second colour
  condition and show it along the x axis.

- error_data:

  Not currently used.

- cflip:

  Flip the plot so that the x axis is vertical?

- norm, norm_pid, norm_iq:

  Old arguments for normalising. Use `norm_by` and `norm_scale` instead.

- norm_by:

  Normalise by "pid" or by "dv", or "none" (the default).

- norm_scale:

  "z" (the default) for z scores, or "iq" for a scale with a mean of 100
  and an SD of 15.

- bars:

  Show means as bars?

- violin:

  Show violins of the distributions?

- dots:

  Show a point for each data point?

- splitV:

  Draw a split violin (half for each colour) when there are two colour
  conditions.

- svw:

  Width of the split violins.

- dot_h_jitter:

  Amount of vertical jitter for the dots.

- line:

  Join the means with lines?

- error_bars:

  Show error bars (standard errors)?

- useall:

  Plot all rows, ignoring any `use` column.

- legend:

  Show the legend? It is hidden when colour and x axis show the same
  condition.

- title_overide:

  If `TRUE`, the file name is made from the title alone rather than also
  adding the dv and conditions.

- combine_plots:

  A list of other plots to put together with this one (using cowplot).

- combine_position:

  Where to put the combined plots: "right" or "under".

- elementinc:

  An extra ggplot element (eg a theme) added to the plot early on.

- type:

  Shortcut: "m" shows only the means and error bars, "b" shows just bars
  and error bars.

- saveoutdata:

  Also save the data used for the plot, as a .csv file.

- display_values:

  Write the mean values on the plot?

- ...:

  Not currently used.

## Value

A ggplot object (or a combined cowplot figure if `combine_plots` is
used).

## Details

**Specifying the conditions**

- `x_condition` is the column shown along the x axis, `colour_condition`
  splits by colour, and `facet_condition` makes a separate panel for
  each level. `plot_condition` is a shortcut where you give a vector of
  up to three names, in the order colour, x, facet.

- If you give only `colour_condition` it is also used for the x axis.

- `dv` is the column with the data to plot. If you give several columns,
  they are stacked into a column called `variable`, which you can use as
  a condition.

- Data should be in long format, with a row for each observation (for
  example each trial), and a column identifying participants (`pid`).
  Set `pid_average = TRUE` to average each participant's data within
  conditions before plotting.

- If the data has a column called `use`, only rows where `use` is 1 are
  plotted, unless you set `useall = TRUE`.

**Normalising**: `norm_by = "dv"` converts values to z scores for each
of the dvs, so that you can compare them on one axis. `norm_by = "pid"`
does that for each participant. Use `norm_scale = "iq"` for a scale with
mean 100 and standard deviation 15 instead.

**Saving**: the file name is made from the title, the dv and the names
of the conditions. Set `outp = NULL` to avoid saving a file.

**Old arguments** `cond`, `cond2`, `facetby`, `norm`, `norm_iq`,
`norm_pid` are kept so that old code still works. Use `x_condition`,
`colour_condition`, `facet_condition`, `norm_by` and `norm_scale` in new
code.

## See also

[`plot_model()`](https://dcr-eyethink.github.io/eyethinkdata/reference/plot_model.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pirateye(data = iat_data, colour_condition = "congruency",
         x_condition = "firstblock", dv = "rt", outp = NULL)
# average over participants, with bars rather than violins
pirateye(iat_data, colour_condition = "congruency", x_condition = "firstblock",
         dv = "rt", pid_average = TRUE, violin = FALSE, bars = TRUE, outp = NULL)
} # }
```
