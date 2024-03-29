% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_model.R
\name{plot_model}
\alias{plot_model}
\title{Plots the output of an anova or mixed model}
\usage{
plot_model(
  mod,
  outp = "analysis",
  error_type = "SE",
  posthocs = T,
  plot_conditions = NULL,
  ...
)
}
\arguments{
\item{mod}{output from an afex model}

\item{outp}{folder for saving. Set blank if not required}

\item{error_type}{Error bars: none, SE or CL}

\item{posthocs}{do you want to show posthocs on plot?}

\item{plot_conditions}{a list of main effects and interactions eg list("cond1", "cond1:cond2")}

\item{...}{plotting arguments to pass to pirateye}
}
\description{
Plots the main effects and interactionsof an afex anova or mixed model
And shows posthocs contrasts for the effects of condition 1 across levels of condition 2
Will pass on arguments to pirateye
}
