% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMDFR.Pop.Biomass.R
\name{getMDFR.Pop.Biomass}
\alias{getMDFR.Pop.Biomass}
\title{Get population biomass time series from model results from TCSAM02 model runs as a dataframe}
\usage{
getMDFR.Pop.Biomass(tcsams, cast = "x", verbose = FALSE)
}
\arguments{
\item{tcsams}{\itemize{
\item single tcsam02.rep object, tcsam02.resLst object, or named list of the latter
}}

\item{cast}{\itemize{
\item casting formula for excluding x,m,s,z factor levels from a sum of B_yxmsz across the unspecified factors
}}

\item{verbose}{\itemize{
\item flag (T/F) to print debug info
}}
}
\value{
dataframe in canonical format
}
\description{
Function to get population biomass time series from model results from TCSAM02 model runs as a dataframe.
}
\details{
Extracts the estimated population biomass time series.
}
