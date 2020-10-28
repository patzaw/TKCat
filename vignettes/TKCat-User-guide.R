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
library(devTKCat)

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
   path=system.file("examples/ClinVar", package="devTKCat")
)

## -----------------------------------------------------------------------------
file_chembl <- read_fileMDB(
   path=system.file("examples/CHEMBL", package="devTKCat")
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

