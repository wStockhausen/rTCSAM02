% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMDFR.Surveys.Abundance.R
\name{getMDFR.Surveys.Abundance}
\alias{getMDFR.Surveys.Abundance}
\title{Get survey abundance time series from model results from TCSAM02 model runs as a dataframe}
\usage{
getMDFR.Surveys.Abundance(tcsams, category = "index", cast = "y+x",
  verbose = FALSE)
}
\arguments{
\item{tcsams}{- single tcsam02.rep object, tcsam02.resLst object, or named list of the latter}

\item{category}{- 'index' is only choice}

\item{cast}{- casting formula for excluding y,x,m,s,z factor levels from an average-at-size across unspecified factors}

\item{verbose}{- flag (T/F) to print debug info}
}
\value{
dataframe in canonical format
}
\description{
Function to get survey abundance time series from model results from TCSAM02 model runs as a dataframe.
}
\details{
Extracts the estimated survey abundance time series.
}
