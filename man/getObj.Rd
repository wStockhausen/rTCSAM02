% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getObj.R
\name{getObj}
\alias{getObj}
\title{Get an object from a list object using the specified path into the list}
\usage{
getObj(path, lst, verbose = FALSE)
}
\arguments{
\item{path}{- path in the list to the desired object (using '/' as separator for list levels)}

\item{lst}{- the list to extract the object from}

\item{verbose}{- flag (T/F) to print diagnostics}
}
\value{
the specified object, or NULL if not found
}
\description{
Function to get an object from a list object using the specified path into the list.
}
\details{
none.
}
