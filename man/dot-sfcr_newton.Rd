% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sfcr_newton.R
\name{.sfcr_newton}
\alias{.sfcr_newton}
\title{Newton-Raphson solver implemented with \code{rootSolve::multiroot()}}
\usage{
.sfcr_newton(m, equations, periods, max_ite, tol, ...)
}
\arguments{
\item{m}{The initialized matrix obtained with \code{.make_matrix()}.}

\item{equations}{Prepared equations with \code{.prep_equations()}.}

\item{periods}{Total number of rows (periods) in the model.}

\item{max_ite}{Maximum number of iterations allowed per block per period.}

\item{tol}{Tolerance accepted to determine convergence.}

\item{...}{Extra parameters to pass to \code{rootSolve::multiroot()}.}
}
\description{
Newton-Raphson solver implemented with \code{rootSolve::multiroot()}
}
\details{
This function implements the Newton-Raphson method to solve the cyclical
blocks of equations. It relies on the \code{multiroot()} function from \code{rootSolve}.
}
\author{
João Macalós
}
\keyword{internal}
