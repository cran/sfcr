% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sfcr_shock.R
\name{sfcr_shock}
\alias{sfcr_shock}
\title{Create shock(s) to add to a \code{sfcr_scenario()}.}
\usage{
sfcr_shock(variables, start, end)
}
\arguments{
\item{variables}{A list with formula(e) containing the name of the variable(s)
that will be shocked on the left-hand side and their new values on the right-
hand side.}

\item{start}{An integer indicating the period when the shock takes place.}

\item{end}{An integer indicating the period when the shock ends.}
}
\description{
Create shock(s) to add to a \code{sfcr_scenario()}.
}
\examples{

sfcr_shock(
 variables = sfcr_set(G_d ~ 30, W ~ 1.5),
 start = 5,
 end = 66)

}
\author{
João Macalós, \email{joaomacalos@gmail.com}
}
