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
library(TKCat)

## -----------------------------------------------------------------------------
plot(TKCat:::DEFAULT_DATA_MODEL)

## -----------------------------------------------------------------------------
plot(TKCat:::CHMDB_DATA_MODEL)

## ---- eval=FALSE--------------------------------------------------------------
#  k <- chTKCat(port=9101L, user="pgodard")
#  create_chTKCat_user(k, login="lfrancois", contact="Liesbeth.Francois@ucb.com")

## ---- eval=FALSE--------------------------------------------------------------
#  toCreate <- readLines("~/Shared/Data-Science/Data-Source-Model-Repository/01-lfrancois-Resources/All-Resources.txt")
#  for(r in toCreate){
#     if(!r %in% list_MDBs(k, withInfo=FALSE)){
#        create_chMDB(k, r)
#     }
#     set_chMDB_access(k, r, public=TRUE)
#     add_chMDB_user(k, r, "lfrancois", admin=TRUE)
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  toFeed <- readLines("~/Shared/Data-Science/Data-Source-Model-Repository/01-pgodard-Resources/All-Resources.txt")
#  lc <- scan_fileMDBs(
#     "~/Shared/Data-Science/Data-Source-Model-Repository/",
#     subdirs=toFeed
#  )
#  # explore_MDBs(lc)
#  for(r in toFeed){
#     message(r)
#     lr <- as_memoMDB(lc[[r]])
#     cr <- as_chMDB(lr, k, overwrite=TRUE)
#  }

