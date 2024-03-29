% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMDFR.OFCs.ChelaHeightData.R
\name{getMDFR.OFCs.ChelaHeightData}
\alias{getMDFR.OFCs.ChelaHeightData}
\title{Get objective function components for chela height data from a tcsam02.rep object}
\usage{
getMDFR.OFCs.ChelaHeightData(obj, verbose = FALSE)
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
Function to get objective function components for chela height data from a tcsam02.rep object.
}
\details{
Returned dataframe has columns:
\itemize{
\item{case - model case (blank, to be filled in by caller)}
\item{category - "chela height data"}
\item{fleet - dummy value ("")}
\item{catch.type - dummy value ("")}
\item{data.type - data type ("chela height data")}
\item{fit.type - dummy value ("")}
\item{nll.type - likelihood type}
\item{y - year}
\item{x - sex}
\item{m - chela height state ("")}
\item{s - shell condition ("new shell")}
\item{ rmse - root mean squared error}
\item{ wgt - likelihood weight}
\item{nll - (unweighted) negative log-likelihood}
\item{objfun - objective function value}
}
}
