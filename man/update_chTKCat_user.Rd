% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chTKCat.R
\name{update_chTKCat_user}
\alias{update_chTKCat_user}
\title{Update a chTKCat user information}
\usage{
update_chTKCat_user(x, login, contact, admin, provider)
}
\arguments{
\item{x}{a \link{chTKCat} object}

\item{login}{user login}

\item{contact}{contact information (can be NA)}

\item{admin}{a logical indicating if the user is an admin of the chTKCat
instance}

\item{provider}{a logical indicating if the user is data provider (TRUE)
or a data consumer (FALSE: default)}
}
\value{
No return value, called for side effects
}
\description{
Update a chTKCat user information
}
