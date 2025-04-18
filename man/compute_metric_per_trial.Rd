% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameter-tuning.R
\name{compute_metric_per_trial}
\alias{compute_metric_per_trial}
\title{Compute a scalar metric from each trial of an experiment.}
\usage{
compute_metric_per_trial(results_df, eval_function, params_df = NULL)
}
\arguments{
\item{results_df}{A table of results, one row per trial}

\item{eval_function}{A function to evaluate each row, using the result and (optionally-null) parameters}

\item{params_df}{An optionally-null dataframe of paramters, one row per trial}
}
\value{
The list of metric values, one per row
The eval_function is run on the corresponding pairs of rows from results_df and params_df
}
\description{
Compute a scalar metric from each trial of an experiment.
}
