## ----setup, message=FALSE, echo=FALSE, include=FALSE, cache=FALSE---------------------------------------------------------------------------------------------------------------------
library(knitr)
opts_chunk$set(
   include=TRUE,
   echo=TRUE,
   message=TRUE,
   warning=TRUE,
   cache=FALSE,
   cache.lazy=FALSE
)
library(TKCat)
igraph_available <- "igraph" %in% installed.packages()[,"Package"]

## ----message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(readr)
hpo_data_dir <- system.file("examples/HPO-subset", package="ReDaMoR")

## ----message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------
HPO_hp <- readr::read_tsv(
   file.path(hpo_data_dir, "HPO_hp.txt")
)
HPO_hp

## ----message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------
HPO_diseases <- readr::read_tsv(
   file.path(hpo_data_dir, "HPO_diseases.txt")
)
HPO_diseases

## ----message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------
HPO_diseaseHP <- readr::read_tsv(
   file.path(hpo_data_dir, "HPO_diseaseHP.txt")
)
HPO_diseaseHP

## ----out.height="200px"---------------------------------------------------------------------------------------------------------------------------------------------------------------
mhpo_dm <- ReDaMoR::df_to_model(HPO_hp, HPO_diseases, HPO_diseaseHP)
if(igraph_available){
   mhpo_dm %>%
      ReDaMoR::auto_layout(lengthMultiplier=80) %>% 
      plot()
}else{
   mhpo_dm %>%
      plot()
}

## ----eval=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# mhpo_dm <- ReDaMoR::model_relational_data(mhpo_dm)

## ----echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
mhpo_dm <- ReDaMoR::read_json_data_model(
   system.file("examples/HPO-model.json", package="ReDaMoR")
)[c("HPO_hp", "HPO_diseases", "HPO_diseaseHP")]

## ----out.height="200px"---------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(mhpo_dm)

## ----echo=FALSE, results='hide'-------------------------------------------------------------------------------------------------------------------------------------------------------
try(
   mhpo_db <- memoMDB(
      dataTables=list(
         HPO_hp=HPO_hp, HPO_diseases=HPO_diseases, HPO_diseaseHP=HPO_diseaseHP
      ),
      dataModel=mhpo_dm,
      dbInfo=list(name="miniHPO")
   ),
   silent=TRUE
)

## ----eval=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# mhpo_db <- memoMDB(
#    dataTables=list(
#       HPO_hp=HPO_hp, HPO_diseases=HPO_diseases, HPO_diseaseHP=HPO_diseaseHP
#    ),
#    dataModel=mhpo_dm,
#    dbInfo=list(name="miniHPO")
# )

## ----echo=FALSE, results='asis'-------------------------------------------------------------------------------------------------------------------------------------------------------
get_confrontation_report() %>% 
   format_confrontation_report_md(
      title="miniHPO",
      level=2, numbered=FALSE
   ) %>% 
   cat()

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
HPO_hp <- mutate(HPO_hp, level=as.integer(level))
HPO_diseases <- mutate(HPO_diseases, id=as.character(id))
HPO_diseaseHP <- mutate(HPO_diseaseHP, id=as.character(id))
mhpo_db <- memoMDB(
   dataTables=list(
      HPO_hp=HPO_hp, HPO_diseases=HPO_diseases, HPO_diseaseHP=HPO_diseaseHP
   ),
   dataModel=mhpo_dm,
   dbInfo=list(name="miniHPO")
)

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
f_mhpo_db <- read_fileMDB(
   path=hpo_data_dir,
   dbInfo=list(name="miniHPO"),
   dataModel=mhpo_dm
)

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print(object.size(mhpo_db), units="Kb")
print(object.size(f_mhpo_db), units="Kb")
compare_MDB(former=mhpo_db, new=f_mhpo_db) %>% 
   DT::datatable(
      rownames=FALSE,
      width="75%",
      options=list(dom="t", pageLength=nrow(.))
   )

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
db_info(mhpo_db)$title <- "Very small extract of the human phenotype ontology"
db_info(mhpo_db)$description <- "For demonstrating ReDaMoR and TKCat capabilities, a very few information from the HPO (human phenotype ontology) has been extracted"
db_info(mhpo_db)$url <- "https://hpo.jax.org/"

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
db_info(mhpo_db)$version <- "0.1"
db_info(mhpo_db)$maintainer <- "Patrice Godard"
db_info(mhpo_db)$timestamp <- Sys.time()

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mhpo_db

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
list_local_collections()

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mhpo_db

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
get_local_collection("Condition") %>%
   show_collection_def()

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mhpo_db$HPO_hp
mhpo_db <- add_collection_member(
   mhpo_db, collection="Condition", table="HPO_hp",
   condition=list(value="Phenotype", static=TRUE),
   source=list(value="HP", static=TRUE),
   identifier=list(value="id", static=FALSE)
)

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mhpo_db <- add_collection_member(
   mhpo_db, collection="Condition", table="HPO_diseases",
   condition=list(value="Disease", static=TRUE),
   source=list(value="db", static=FALSE),
   identifier=list(value="id", static=FALSE)
)

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mhpo_db

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
collection_members(mhpo_db)

