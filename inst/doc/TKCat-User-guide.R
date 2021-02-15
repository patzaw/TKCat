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
library(TKCat)

## -----------------------------------------------------------------------------
file_hpo <- read_fileMDB(
   path=system.file("examples/HPO-subset", package="ReDaMoR"),
   dataModel=system.file("examples/HPO-model.json", package="ReDaMoR"),
   dbInfo=list(
      "name"="HPO",
      "title"="Data extracted from the HPO database",
      "description"=paste(
         "This is a very small subset of the HPO!",
         "Visit the reference URL for more information."
      ),
      "url"="http://human-phenotype-ontology.github.io/"
   )
)

## -----------------------------------------------------------------------------
plot(data_model(file_hpo))

## -----------------------------------------------------------------------------
cn <- c(
   "collection", "cid",                "resource", "mid", "table",        "field",     "static", "value",    "type"
)
cm <- matrix(data=c(
   "Condition",  "HPO_conditions_1.0", "HPO",      1,     "HPO_hp",       "condition",  TRUE,    "Phenotype", NA,
   "Condition",  "HPO_conditions_1.0", "HPO",      1,     "HPO_hp",       "source",     TRUE,    "HP",        NA,
   "Condition",  "HPO_conditions_1.0", "HPO",      1,     "HPO_hp",       "identifier", FALSE,   "id",        NA,
   "Condition",  "HPO_conditions_1.0", "HPO",      2,     "HPO_diseases", "condition",  TRUE,    "Disease",   NA,
   "Condition",  "HPO_conditions_1.0", "HPO",      2,     "HPO_diseases", "source",     FALSE,   "db",        NA,
   "Condition",  "HPO_conditions_1.0", "HPO",      2,     "HPO_diseases", "identifier", FALSE,   "id",        NA
   ),
   ncol=9, byrow=TRUE
) %>%
   set_colnames(cn) %>% 
   as_tibble() %>% 
   mutate(mid=as.integer(mid), static=as.logical(static))
collection_members(file_hpo) <- cm
file_hpo

## -----------------------------------------------------------------------------
file_clinvar <- read_fileMDB(
   path=system.file("examples/ClinVar", package="TKCat")
)

## -----------------------------------------------------------------------------
file_chembl <- read_fileMDB(
   path=system.file("examples/CHEMBL", package="TKCat")
)

## -----------------------------------------------------------------------------
memo_clinvar <- as_memoMDB(file_clinvar)
object.size(file_clinvar) %>% print(units="Kb")
object.size(memo_clinvar) %>% print(units="Kb")

## -----------------------------------------------------------------------------
db_info(file_clinvar)

## -----------------------------------------------------------------------------
plot(data_model(file_clinvar))

## -----------------------------------------------------------------------------
names(file_clinvar)

## -----------------------------------------------------------------------------
collection_members(file_clinvar)

## -----------------------------------------------------------------------------
length(file_clinvar)        # Number of tables
lengths(file_clinvar)       # Number of fields per table
count_records(file_clinvar) # Number of records per table

## -----------------------------------------------------------------------------
data_file_size(file_clinvar, hr=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  data_tables(file_clinvar, "ClinVar_traitNames")[[1]]
#  file_clinvar[["ClinVar_traitNames"]]
#  file_clinvar$"ClinVar_traitNames"

## -----------------------------------------------------------------------------
file_clinvar %>% pull(ClinVar_traitNames)

## -----------------------------------------------------------------------------
file_clinvar[1:3]
c(file_clinvar[1:3], file_hpo[c(1,5,7)]) %>% 
   data_model() %>% auto_layout(force=TRUE) %>% plot()

## -----------------------------------------------------------------------------
filtered_clinvar <- file_clinvar %>% 
   set_names(sub("ClinVar_", "", names(.))) %>%
   filter(
      entrezNames = symbol %in% c("PIK3R2", "UGT1A8")
   ) %>% 
   slice(ReferenceClinVarAssertion=grep(
      "pathogen",
      .$ReferenceClinVarAssertion$clinicalSignificance,
      ignore.case=TRUE
   ))

## -----------------------------------------------------------------------------
gene_traits <- filtered_clinvar %>% 
   join_mdb_tables(
      "entrezNames", "varEntrez", "variants", "rcvaVariant",
      "ReferenceClinVarAssertion", "rcvaTraits", "traits"
   )
gene_traits$entrezNames %>%
   select(symbol, name, variants.type, variants.name, traitType, traits.name)

## -----------------------------------------------------------------------------
file_chembl$CHEMBL_component_sequence %>% head()

## -----------------------------------------------------------------------------
file_clinvar$ClinVar_entrezNames %>% head()

## -----------------------------------------------------------------------------
list_local_collections()

## -----------------------------------------------------------------------------
collection_members(file_chembl, "BE")
collection_members(file_clinvar, "BE")

## -----------------------------------------------------------------------------
get_shared_collections(filtered_clinvar, file_chembl)

## ---- eval=FALSE--------------------------------------------------------------
#  sel_coll <- get_shared_collections(file_clinvar, file_chembl) %>%
#     filter(collection=="BE")
#  filtered_cv_chembl <- merge(
#     x=file_clinvar,
#     y=file_chembl,
#     by=sel_coll
#  )

## -----------------------------------------------------------------------------
get_shared_collections(file_hpo, file_clinvar)

## -----------------------------------------------------------------------------
sel_coll <- get_shared_collections(file_hpo, file_clinvar) %>% 
   filter(table.x=="HPO_diseases", table.y=="ClinVar_traitCref") %>% 
   mutate(collection=NA)
sel_coll
hpo_clinvar <- merge(file_hpo, file_clinvar, by=sel_coll)
plot(data_model(hpo_clinvar))
hpo_clinvar$HPO_diseases_ClinVar_traitCref %>% head()

## -----------------------------------------------------------------------------
k <- TKCat(file_hpo, file_clinvar)

## -----------------------------------------------------------------------------
list_MDBs(k)                     # list all the MDBs in a TKCat object
get_MDB(k, "HPO")                # get a specific MDBs from the catalog
search_MDB_tables(k, "disease")  # Search table about "disease"
search_MDB_fields(k, "disease")  # Search a field about "disease"
collection_members(k)            # Get collection members of the different MDBs
c(k, TKCat(file_chembl))         # Merge 2 TKCat objects

## ---- eval=FALSE--------------------------------------------------------------
#  library(TKCat)
#  explore_MDBs(k, download=TRUE)

