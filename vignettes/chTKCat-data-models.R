## ----setup, message=FALSE, echo=FALSE, include=FALSE, cache=FALSE-------------
library(knitr)
opts_chunk$set(
   include=TRUE,
   echo=FALSE,
   message=FALSE,
   warning=FALSE,
   cache=FALSE,
   cache.lazy=FALSE
)
library(devTKCat)

## -----------------------------------------------------------------------------
plot(devTKCat:::DEFAULT_DATA_MODEL)

## -----------------------------------------------------------------------------
plot(devTKCat:::CHMDB_DATA_MODEL)

