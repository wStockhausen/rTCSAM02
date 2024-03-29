% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcmc.CalcABC.R
\name{mcmc.CalcABC}
\alias{mcmc.CalcABC}
\title{Function to calculate ABC and OFL from an MCMC dataframe}
\usage{
mcmc.CalcABC(
  mcmc = NULL,
  dfr = NULL,
  pstar = 0.49,
  buffer = 0.2,
  doPlot = TRUE,
  xlims = NULL,
  verbose = FALSE
)
}
\arguments{
\item{mcmc}{\itemize{
\item mcmc object
}}

\item{dfr}{\itemize{
\item dataframe with OFL as column (and possibly 'case')
}}

\item{pstar}{\itemize{
\item pstar value for ABC calculation
}}

\item{buffer}{\itemize{
\item buffer for ABC as fraction (i.e., 0.20 = 20 \\% yields ABC = 0.8*OFL)
}}

\item{doPlot}{\itemize{
\item flag (T/F) to create plot
}}

\item{xlims}{\itemize{
\item range for x-axis in plot
}}

\item{verbose}{flag to print diagnostic info}

\item{title}{\itemize{
\item title for plot
}}
}
\value{
list with elements
\itemize{
\item{dfrABCs - dataframe with median results, ABC.pstar and ABC.buffer for each case}
\item{dfrOFLs - dataframe with empirical cdf of OFL for each case}
\item{plot - ggplot2 object}
}
}
\description{
This function calculates ABC and OFL from TCSAM02 MCMC output.
}
\details{
uses \code{plotABC} to produce ABC plots for each case
}
