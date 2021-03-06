% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/experiment_results.R
\name{experiment_results}
\alias{experiment_results}
\title{Gather results from the experiment}
\usage{
experiment_results()
}
\value{
A list of 3 tibbles.
}
\description{
This function will access the specific metadata files generated for the experiment conducted and perform some data cleaning.
After the data is cleaned a separate data set is generated for instances, solutions and simulations.
These are stored in a list which is returned by the function.
}
\details{
The purpose of the function is to show how the raw metadata from the instances, solutions and simulations is transformed into the results data set.

The function is tailored to the specific experiment conducted and will need to be revised to handle a new experiment.
This is as a results of, but not limited to, the arrival rate variance and number of UAVs levels being hardcoded.
}
