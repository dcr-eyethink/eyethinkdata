# Plot the output of an ANOVA or mixed model

Plots the means for the main effects and interactions in a model made
with the afex package (`aov_ez`, `aov_car`, `aov_4` or `mixed`), and
shows the post hoc contrasts. It also prints the model summary, and the
estimated means and contrasts for each effect, and saves them to a text
file.

## Usage

``` r
plot_model(
  mod,
  outp = "analysis",
  error_type = "SE",
  posthocs = T,
  plot_conditions = NULL,
  ...
)
```

## Arguments

- mod:

  A model from the afex package.

- outp:

  Name of the folder in which to save the plots (pdf) and the results (a
  text file); it is created if it does not exist. Set to `NULL` or "" to
  save nothing.

- error_type:

  The error bars: "SE" for standard errors (the default), "CL" for
  confidence limits, or "none".

- posthocs:

  Not currently used. (The post hoc results are always shown.)

- plot_conditions:

  Which effects to plot, as a list or vector of text, eg
  `c("cond1", "cond1:cond2")`. By default all the main effects and the
  interaction (if there are two factors) are plotted.

- ...:

  Further arguments passed on to
  [`pirateye()`](https://dcr-eyethink.github.io/eyethinkdata/reference/pirateye.md).

## Value

A list with the `model_summary`, and for each effect plotted, the
estimated means (`means_<effect>`) and the plot (`plot_<effect>`).

## Details

For interactions of two factors, the first factor named is shown in
colour and the second along the x axis. The post hoc p-values (shown
above the x axis) compare the levels of the first factor at each level
of the second. The p-value of the effect from the model is shown in the
title.

The plots are drawn with
[`pirateye()`](https://dcr-eyethink.github.io/eyethinkdata/reference/pirateye.md),
and you can pass any of its arguments to change them.

## See also

[`pirateye()`](https://dcr-eyethink.github.io/eyethinkdata/reference/pirateye.md)

## Examples

``` r
if (FALSE) { # \dontrun{
anv <- afex::aov_ez(data = iat_data, id = "pid", dv = "rt",
                    between = "firstblock", within = "congruency",
                    fun_aggregate = mean)
res <- plot_model(anv, outp = NULL)
res$plot_congruency
} # }
```
