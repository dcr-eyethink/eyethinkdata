% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gorilla_q_rankscore.R
\name{gorilla_q_rankscore}
\alias{gorilla_q_rankscore}
\title{Scores ranked items}
\usage{
gorilla_q_rankscore(data, rankkey = "rankpredict", keepcols = F, respid = NULL)
}
\arguments{
\item{data}{data list or just the data_q}

\item{rankkey}{what key was used in QB1 for this item}

\item{respid}{word to append to column names}
}
\description{
Takes output from a ranking questionniare (QB1) in gorilla and returns data in a wide format with one column per ranked item
}
