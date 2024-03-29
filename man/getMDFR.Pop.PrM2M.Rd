% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMDFR.Pop.PrM2M.R
\name{getMDFR.Pop.PrM2M}
\alias{getMDFR.Pop.PrM2M}
\title{Get molt-to-maturity ogives from model results from TCSAM02 model runs as a dataframe}
\usage{
getMDFR.Pop.PrM2M(tcsams, path = "mp/prM2M_cz", verbose = FALSE)
}
\arguments{
\item{tcsams}{\itemize{
\item single tcsam02.rep object, tcsam02.resLst object, named list of the latter, or general list
}}

\item{path}{\itemize{
\item path into tcsams object at which to find the prM2M array
}}

\item{verbose}{\itemize{
\item flag (T/F) to print debug info
}}
}
\value{
dataframe in canonical format
}
\description{
Function to get molt-to-maturity ogives from model results from TCSAM02 model runs as a dataframe.
}
\details{
Extracts molt-to-maturity ogives.
}
