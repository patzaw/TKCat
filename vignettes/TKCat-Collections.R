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
library(TKCat)

## -----------------------------------------------------------------------------
list_local_collections()

## ---- eval=FALSE--------------------------------------------------------------
#  get_local_collection("BE") %>%
#     paste('```json', ., '```', sep="\n") %>% cat()

## ---- echo=FALSE, results='asis'----------------------------------------------
get_local_collection("BE") %>%
   paste('```json', ., '```', sep="\n") %>% cat()

## ---- eval=FALSE--------------------------------------------------------------
#  system.file(
#     "examples/CHEMBL/model/Collections/BE-CHEMBL_BE_1.0.json",
#     package="TKCat"
#  ) %>%
#     readLines() %>% paste(collapse="\n")

## ---- echo=FALSE, results='asis'----------------------------------------------
system.file(
   "examples/CHEMBL/model/Collections/BE-CHEMBL_BE_1.0.json",
   package="TKCat"
) %>% 
   readLines() %>% paste(collapse="\n") %>%
   paste('```json', ., '```', sep="\n") %>% cat()

## -----------------------------------------------------------------------------
jsonvalidate::json_validate(
   json=system.file(
      "examples/CHEMBL/model/Collections/BE-CHEMBL_BE_1.0.json",
      package="TKCat"
   ),
   schema=get_local_collection("BE")
)

## ---- eval=FALSE--------------------------------------------------------------
#  get_collection_mapper("BE")

## ---- echo=FALSE, results='asis'----------------------------------------------
get_collection_mapper("BE") %>% 
   format() %>% paste(collapse="\n") %>% 
   paste('```r', ., '```', sep="\n") %>% cat()

