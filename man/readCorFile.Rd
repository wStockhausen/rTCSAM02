% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readCorFile.R
\name{readCorFile}
\alias{readCorFile}
\title{Read the model cor (correlation) file}
\usage{
readCorFile(folder = NULL, model = "tcsam02")
}
\arguments{
\item{folder}{- folder containing the model run}

\item{model}{- model name (default is "tcsam02")}
}
\value{
a dataframe
}
\description{
Function to read a model cor (correlation) file.
}
\details{
This function reads the model par and cor files, based on the given folder and model name, and
returns a dataframe with columns i, p_i, type, p_j, and val. "p_i" and "p_j" are the parameter names, and
"type" reflects the value ("est", "sdv", or "cor").
}
