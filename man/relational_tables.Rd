% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metaMDB.R
\name{relational_tables}
\alias{relational_tables}
\title{Get a list of relational tables}
\usage{
relational_tables(x, recursive = FALSE)
}
\arguments{
\item{x}{a \link{metaMDB} object}

\item{recursive}{if TRUE, function returns also the
relational tables from embedded metaMDBs.}
}
\value{
A list of relational tables (tibbles)
}
\description{
Get a list of relational tables
}
