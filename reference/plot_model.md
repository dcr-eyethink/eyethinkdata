# Plots the output of an anova or mixed model

Plots the main effects and interactionsof an afex anova or mixed model
And shows posthocs contrasts for the effects of condition 1 across
levels of condition 2 Will pass on arguments to pirateye

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

  output from an afex model

- outp:

  folder for saving. Set blank if not required

- error_type:

  Error bars: none, SE or CL

- posthocs:

  do you want to show posthocs on plot?

- plot_conditions:

  a list of main effects and interactions eg list("cond1",
  "cond1:cond2")

- ...:

  plotting arguments to pass to pirateye
