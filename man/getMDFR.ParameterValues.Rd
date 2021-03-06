% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMDFR.ParameterValues.R
\name{getMDFR.ParameterValues}
\alias{getMDFR.ParameterValues}
\title{Get parameter values from a set of TCSAM02 model runs as a dataframe}
\usage{
getMDFR.ParameterValues(tcsams, verbose = FALSE)
}
\arguments{
\item{tcsams}{- a tcsam02.resLst object or named list of such}

\item{verbose}{- flag to print debugging info}
}
\value{
- a dataframe
}
\description{
Function to get parameter values from a set of TCSAM02 model runs as a dataframe.
}
\details{
Returns a dataframe with parameter estimates and standard deviations (if the std object exists) by model run.
}
