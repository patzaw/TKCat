## ----setup, message=FALSE, echo=FALSE, include=FALSE, cache=FALSE-------------
library(knitr)
opts_chunk$set(
   include=TRUE,
   echo=TRUE,
   message=FALSE,
   warning=FALSE,
   cache=FALSE,
   cache.lazy=FALSE
)
## The following line is to avoid building errors on CRAN
knitr::opts_chunk$set(eval=Sys.getenv("USER") %in% c("pgodard"))
library(TKCat)

## ---- eval=FALSE--------------------------------------------------------------
#  k <- chTKCat(user="pgodard")
#  create_chTKCat_user(k, login="lfrancois", contact=NA, admin=FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  drop_chTKCat_user(k, login="lfrancois")

## ---- eval=FALSE--------------------------------------------------------------
#  create_chMDB(k, "CHEMBL", public=FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  set_chMDB_access(k, "CHEMBL", public=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  add_chMDB_user(k, "CHEMBL", "lfrancois", admin=TRUE)
#  # remove_chMDB_user(k, "CHEMBL", "lfrancois")
#  list_chMDB_users(k, "CHEMBL")

## ---- eval=FALSE--------------------------------------------------------------
#  lc <- scan_fileMDBs("fileMDB_directory")
#  ## The commented line below allows the exploration of the data models in lc.
#  # explore_MDBs(lc)
#  for(r in toFeed){
#     message(r)
#     lr <- as_memoMDB(lc[[r]])
#     cr <- as_chMDB(lr, k, overwrite=TRUE)
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  empty_chMDB(k, "CHEMBL")

## ---- eval=FALSE--------------------------------------------------------------
#  drop_chMDB(k, "CHEMBL")

## ---- eval=FALSE--------------------------------------------------------------
#  add_chTKCat_collection(k, "BE")
#  list_chTKCat_collections(k)
#  remove_chTKCat_collection(k, "BE")

## ---- echo=FALSE--------------------------------------------------------------
plot(TKCat:::DEFAULT_DATA_MODEL)

## ---- echo=FALSE--------------------------------------------------------------
plot(TKCat:::CHMDB_DATA_MODEL)

