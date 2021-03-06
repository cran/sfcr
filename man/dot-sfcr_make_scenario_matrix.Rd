% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sfcr_scenario.R
\name{.sfcr_make_scenario_matrix}
\alias{.sfcr_make_scenario_matrix}
\title{Make matrix for scenario calculations}
\usage{
.sfcr_make_scenario_matrix(baseline, scenario, periods)
}
\arguments{
\item{baseline}{a model calculated with the \code{sfcr_sim()} function}

\item{scenario}{a List holding the different scenarios}

\item{periods}{The total number of periods in the model}
}
\description{
Make matrix for scenario calculations
}
\details{
This function generates the base matrix that is going to be
modified in place by the different solvers.
}
\author{
João Macalós
}
\keyword{internal}
