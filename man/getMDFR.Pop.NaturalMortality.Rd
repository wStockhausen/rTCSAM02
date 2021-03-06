% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMDFR.Pop.NaturalMortality.R
\name{getMDFR.Pop.NaturalMortality}
\alias{getMDFR.Pop.NaturalMortality}
\title{Get natural mortality rates from model results from TCSAM02 model runs as a dataframe}
\usage{
getMDFR.Pop.NaturalMortality(
  tcsams = NULL,
  type = c("M_cxm", "M_yxm", "M_yxmsz"),
  verbose = FALSE
)
}
\arguments{
\item{tcsams}{- single TCSAM02 model report object, or named list of such}

\item{type}{- flag indicating which M's to extract ('M_cxm', 'M_yxm' or 'M_yxmsz')}

\item{verbose}{- flag (T/F) to print debug info}
}
\value{
dataframe in canonical format
}
\description{
Function to get natural mortality rates from model results from TCSAM02 model runs as a dataframe.
}
\details{
Extracts natural mortality rates. Uses \code{reshape2} package.
}
