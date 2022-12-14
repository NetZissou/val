% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{validate_file}
\alias{validate_file}
\title{Wrapper function for \code{validate}}
\usage{
validate_file(file, schema, quiet = FALSE)
}
\arguments{
\item{file}{data file to be validated}

\item{schema}{pre-specified schema}

\item{quiet}{TRUE when validate without console output, only return the invalid stack table}
}
\value{
a list with success indicator and stack of error messages
}
\description{
This function will load all columns as characters and then use the `validate`` function to check if
the data could be parsed based on the specified schema
}
