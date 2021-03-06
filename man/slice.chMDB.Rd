% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chMDB.R
\name{slice.chMDB}
\alias{slice.chMDB}
\title{Subset a \link{chMDB} object according to row position in one table
and return a \link{memoMDB}}
\usage{
\method{slice}{chMDB}(.data, ..., by = 10^5, .preserve = FALSE)
}
\arguments{
\item{.data}{a \link{chMDB} object}

\item{...}{a single argument. The name of this argument should be a table
name of x and the value of this argument should be vector of integers
corresponding to row indexes.}

\item{by}{the size of the batch: number of records to slice
together (default: 10^5)}

\item{.preserve}{not used}
}
\value{
a \link{memoMDB} object
}
\description{
Subset a \link{chMDB} object according to row position in one table
and return a \link{memoMDB}
}
