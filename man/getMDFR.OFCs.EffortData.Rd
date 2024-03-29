% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMDFR.OFCs.EffortData.R
\name{getMDFR.OFCs.EffortData}
\alias{getMDFR.OFCs.EffortData}
\title{Get objective function components for effort data from a tcsam02.rep object}
\usage{
getMDFR.OFCs.EffortData(obj, verbose = FALSE)
}
\arguments{
\item{obj}{\itemize{
\item a tcsam02.resLst or tcsam02.rep object
}}

\item{verbose}{\itemize{
\item flag (T/F) to print diagnostic info
}}
}
\value{
a dataframe
}
\description{
Function to get objective function components for effort data from a tcsam02.rep object.
}
\details{
Returned dataframe has columns:
\itemize{
\item{case - model case (blank, to be filled in by caller)}
\item{category - "effort data"}
\item{fleet - dummy value ("")}
\item{catch.type - dummy value ("")}
\item{data.type - data type ("effort data")}
\item{fit.type - dummy value ("")}
\item{nll.type - likelihood type}
\item{y - year (dummy = 'all)}
\item{x - sex}
\item{m - maturity state ("immature")}
\item{s  - shell condition ("new shell")}
\item{ rmse - root mean squared error}
\item{ wgt - likelihood weight}
\item{nll - (unweighted) negative log-likelihood}
\item{objfun - objective function value}
}
}
