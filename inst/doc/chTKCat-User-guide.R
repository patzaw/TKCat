## ----setup, message=FALSE, echo=FALSE, include=FALSE, cache=FALSE-------------
library(knitr)
opts_chunk$set(
   include=TRUE,
   echo=TRUE,
   message=TRUE,
   warning=TRUE,
   cache=FALSE,
   cache.lazy=FALSE
)
## The following line is to avoid building errors on CRAN
knitr::opts_chunk$set(eval=Sys.getenv("USER") %in% c("pgodard"))
library(TKCat)

## -----------------------------------------------------------------------------
k <- chTKCat(
   host="localhost", # default parameter
   port=9101L,       # default parameter
   user="default",   # default parameter
   password=""       # if not provided the password is requested interactively 
)

## ---- eval=FALSE--------------------------------------------------------------
#  list_MDBs(k)                     # list all the MDBs in a TKCat object
#  khpo <- get_MDB(k, "HPO")        # get a specific MDBs from the catalog
#  search_MDB_tables(k, "disease")  # Search table about "disease"
#  search_MDB_fields(k, "disease")  # Search a field about "disease"
#  collection_members(k)            # Get collection members of the different MDBs

## ---- eval=FALSE--------------------------------------------------------------
#  lk <- TKCat(read_fileMDB(
#     path=system.file("examples/ClinVar", package="TKCat")
#  ))
#  names(lk)
#  as_chMDB(lk$ClinVar, k, overwrite=FALSE)

