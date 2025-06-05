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
igraph_available <- "igraph" %in% installed.packages()[,"Package"]

## ----message=FALSE------------------------------------------------------------
library(readr)
hpo_data_dir <- system.file("examples/HPO-subset", package="ReDaMoR")

## ----message=FALSE------------------------------------------------------------
HPO_hp <- readr::read_tsv(
   file.path(hpo_data_dir, "HPO_hp.txt")
)
HPO_hp

## ----message=FALSE------------------------------------------------------------
HPO_diseases <- readr::read_tsv(
   file.path(hpo_data_dir, "HPO_diseases.txt")
)
HPO_diseases

## ----message=FALSE------------------------------------------------------------
HPO_diseaseHP <- readr::read_tsv(
   file.path(hpo_data_dir, "HPO_diseaseHP.txt")
)
HPO_diseaseHP

## ----out.height="200px"-------------------------------------------------------
mhpo_dm <- ReDaMoR::df_to_model(HPO_hp, HPO_diseases, HPO_diseaseHP)
if(igraph_available){
   mhpo_dm %>%
      ReDaMoR::auto_layout(lengthMultiplier=80) %>% 
      plot()
}else{
   mhpo_dm %>%
      plot()
}

## ----eval=FALSE---------------------------------------------------------------
# mhpo_dm <- ReDaMoR::model_relational_data(mhpo_dm)

## ----echo=FALSE---------------------------------------------------------------
mhpo_dm <- ReDaMoR::read_json_data_model(
   system.file("examples/HPO-model.json", package="ReDaMoR")
)[c("HPO_hp", "HPO_diseases", "HPO_diseaseHP")]

## ----out.height="200px"-------------------------------------------------------
plot(mhpo_dm)

## ----echo=FALSE, results='hide'-----------------------------------------------
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

## ----eval=FALSE---------------------------------------------------------------
# mhpo_db <- memoMDB(
#    dataTables=list(
#       HPO_hp=HPO_hp, HPO_diseases=HPO_diseases, HPO_diseaseHP=HPO_diseaseHP
#    ),
#    dataModel=mhpo_dm,
#    dbInfo=list(name="miniHPO")
# )

## ----echo=FALSE, results='asis'-----------------------------------------------
get_confrontation_report() %>% 
   format_confrontation_report_md(
      title="miniHPO",
      level=2, numbered=FALSE
   ) %>% 
   cat()

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
f_mhpo_db <- read_fileMDB(
   path=hpo_data_dir,
   dbInfo=list(name="miniHPO"),
   dataModel=mhpo_dm
)

## -----------------------------------------------------------------------------
print(object.size(mhpo_db), units="Kb")
print(object.size(f_mhpo_db), units="Kb")
compare_MDB(former=mhpo_db, new=f_mhpo_db) %>% 
   DT::datatable(
      rownames=FALSE,
      width="75%",
      options=list(dom="t", pageLength=nrow(.))
   )

## -----------------------------------------------------------------------------
db_info(mhpo_db)$title <- "Very small extract of the human phenotype ontology"
db_info(mhpo_db)$description <- "For demonstrating ReDaMoR and TKCat capabilities, a very few information from the HPO (human phenotype ontology) has been extracted"
db_info(mhpo_db)$url <- "https://hpo.jax.org/"

## -----------------------------------------------------------------------------
db_info(mhpo_db)$version <- "0.1"
db_info(mhpo_db)$maintainer <- "Patrice Godard"
db_info(mhpo_db)$timestamp <- Sys.time()

## -----------------------------------------------------------------------------
mhpo_db

## -----------------------------------------------------------------------------
list_local_collections()

## -----------------------------------------------------------------------------
mhpo_db

## -----------------------------------------------------------------------------
get_local_collection("Condition") %>%
   show_collection_def()

## -----------------------------------------------------------------------------
mhpo_db$HPO_hp
mhpo_db <- add_collection_member(
   mhpo_db, collection="Condition", table="HPO_hp",
   condition=list(value="Phenotype", static=TRUE),
   source=list(value="HP", static=TRUE),
   identifier=list(value="id", static=FALSE)
)

## -----------------------------------------------------------------------------
mhpo_db <- add_collection_member(
   mhpo_db, collection="Condition", table="HPO_diseases",
   condition=list(value="Disease", static=TRUE),
   source=list(value="db", static=FALSE),
   identifier=list(value="id", static=FALSE)
)

## -----------------------------------------------------------------------------
mhpo_db

## -----------------------------------------------------------------------------
collection_members(mhpo_db)

## ----results='hide'-----------------------------------------------------------
tmpDir <- tempdir()
as_fileMDB(mhpo_db, path=tmpDir, htmlModel=FALSE)

## ----echo=FALSE---------------------------------------------------------------
list.files(
   path=file.path(tmpDir, "miniHPO"),
   recursive=TRUE
) %>% 
   file.path("miniHPO", .) %>%
   data.frame(pathString=.) %>% 
   data.tree::as.Node() %>%
   data.tree::ToDataFrameTree() %>%
   pull(1) %>% 
   cat(sep="\n")

## -----------------------------------------------------------------------------
read_fileMDB(file.path(tmpDir, "miniHPO"))

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
file_hpo <- file_hpo %>% 
   add_collection_member(
      collection="Condition", table="HPO_hp",
      condition=list(value="Phenotype", static=TRUE),
      source=list(value="HP", static=TRUE),
      identifier=list(value="id", static=FALSE)
   ) %>% 
   add_collection_member(
      collection="Condition", table="HPO_diseases",
      condition=list(value="Disease", static=TRUE),
      source=list(value="db", static=FALSE),
      identifier=list(value="id", static=FALSE)
   )

## -----------------------------------------------------------------------------
file_clinvar <- read_fileMDB(
   path=system.file("examples/ClinVar", package="TKCat")
)

## -----------------------------------------------------------------------------
file_clinvar

## -----------------------------------------------------------------------------
file_chembl <- read_fileMDB(
   path=system.file("examples/CHEMBL", package="TKCat")
)

## -----------------------------------------------------------------------------
file_chembl

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
file_clinvar <- file_clinvar %>% 
   set_names(sub("ClinVar_", "", names(.))) 
names(file_clinvar)

## -----------------------------------------------------------------------------
collection_members(file_clinvar)

## -----------------------------------------------------------------------------
length(file_clinvar)        # Number of tables
lengths(file_clinvar)       # Number of fields per table
count_records(file_clinvar) # Number of records per table

## -----------------------------------------------------------------------------
data_file_size(file_clinvar, hr=TRUE)

## ----eval=FALSE---------------------------------------------------------------
# data_tables(file_clinvar, "traitNames")[[1]]
# file_clinvar[["traitNames"]]
# file_clinvar$"traitNames"
# file_clinvar %>% pull(traitNames)

## ----echo=FALSE---------------------------------------------------------------
file_clinvar %>% pull(traitNames)

## -----------------------------------------------------------------------------
file_clinvar[1:3]
if(igraph_available){
   c(file_clinvar[1:3], file_hpo[c(1,5,7)]) %>% 
      data_model() %>% auto_layout(force=TRUE) %>% plot()
}else{
   c(file_clinvar[1:3], file_hpo[c(1,5,7)]) %>% 
      data_model() %>% plot()
}

## -----------------------------------------------------------------------------
filtered_clinvar <- file_clinvar %>%
   filter(
      entrezNames = symbol %in% c("PIK3R2", "UGT1A8")
   ) %>% 
   slice(ReferenceClinVarAssertion=grep(
      "pathogen",
      .$ReferenceClinVarAssertion$clinicalSignificance,
      ignore.case=TRUE
   ))
left_join(
   dims(file_clinvar) %>% select(name, nrow),
   dims(filtered_clinvar) %>% select(name, nrow),
   by="name",
   suffix=c("_ori", "_filt")
)

## -----------------------------------------------------------------------------
gene_traits <- filtered_clinvar %>% 
   join_mdb_tables(
      "entrezNames", "varEntrez", "variants", "rcvaVariant",
      "ReferenceClinVarAssertion", "rcvaTraits", "traits"
   )
gene_traits$entrezNames %>%
   select(symbol, name, variants.type, variants.name, traitType, traits.name)

## -----------------------------------------------------------------------------
file_chembl$CHEMBL_component_sequence

## -----------------------------------------------------------------------------
file_clinvar$entrezNames

## -----------------------------------------------------------------------------
list_local_collections()

## -----------------------------------------------------------------------------
collection_members(file_chembl, "BE")
collection_members(file_clinvar, "BE")

## -----------------------------------------------------------------------------
get_shared_collections(filtered_clinvar, file_chembl)

## ----message=FALSE------------------------------------------------------------
try(BED::connectToBed(a))
bedCheck <- try(BED::checkBedConn())
if(!inherits(bedCheck, "try-error") && bedCheck){
   sel_coll <- get_shared_collections(file_clinvar, file_chembl) %>% 
      filter(collection=="BE")
   filtered_cv_chembl <- merge(
      x=file_clinvar,
      y=file_chembl,
      by=sel_coll,
      dmAutoLayout=igraph_available
   )
}

## -----------------------------------------------------------------------------
get_shared_collections(file_hpo, file_clinvar)

## -----------------------------------------------------------------------------
sel_coll <- get_shared_collections(file_hpo, file_clinvar) %>% 
   filter(table.x=="HPO_diseases", table.y=="traitCref") %>% 
   mutate(collection=NA)
sel_coll

## -----------------------------------------------------------------------------
hpo_clinvar <- merge(
   file_hpo, file_clinvar, by=sel_coll, dmAutoLayout=igraph_available
)
plot(data_model(hpo_clinvar))
hpo_clinvar$HPO_diseases_traitCref

## -----------------------------------------------------------------------------
k <- TKCat(file_hpo, file_clinvar)

## -----------------------------------------------------------------------------
list_MDBs(k)                     # list all the MDBs in a TKCat object
get_MDB(k, "HPO")                # get a specific MDBs from the catalog
search_MDB_tables(k, "disease")  # Search table about "disease"
search_MDB_fields(k, "disease")  # Search a field about "disease"
collection_members(k)            # Get collection members of the different MDBs
c(k, TKCat(file_chembl))         # Merge 2 TKCat objects

## ----eval=FALSE---------------------------------------------------------------
# library(TKCat)
# explore_MDBs(k, download=TRUE)

## ----echo=FALSE---------------------------------------------------------------
## The following line is to avoid building errors on CRAN
knitr::opts_chunk$set(eval=Sys.getenv("USER") %in% c("pgodard"))

## ----echo=FALSE---------------------------------------------------------------
k <- chTKCat(
   host="localhost",                     # default parameter
   port=9113L,                           # LOCAL PARAMETER
   drv=ClickHouseHTTP::ClickHouseHTTP(), # default parameter
   user="default",                       # default parameter
   password=""                           # if not provided the
                                         # password is requested interactively 
)

## ----eval=FALSE---------------------------------------------------------------
# k <- chTKCat(
#    host="localhost",                     # default parameter
#    port=9111L,                           # default parameter
#    drv=ClickHouseHTTP::ClickHouseHTTP(), # default parameter
#    user="default",                       # default parameter
#    password=""                           # if not provided the
#                                          # password is requested interactively
# )

## -----------------------------------------------------------------------------
list_MDBs(k)             # get a specific MDBs from the catalog
search_MDB_tables(k, "disease")  # Search table about "disease"
search_MDB_fields(k, "disease")  # Search a field about "disease"
collection_members(k)  

## ----eval=FALSE---------------------------------------------------------------
# explore_MDBs(k)

## ----eval=FALSE---------------------------------------------------------------
# kw <- chTKCat(host="localhost", port=9111L, user="pgodard")
# create_chMDB(kw, "HPO", public=TRUE)
# ch_hpo <- as_chMDB(file_hpo, kw)

## -----------------------------------------------------------------------------
ch_hpo <- get_MDB(k, "HPO")

## ----echo=TRUE, eval=FALSE----------------------------------------------------
# get_query(
#    ch_hpo,
#    query="SELECT * from HPO_diseases WHERE lower(label) LIKE '%epilep%'"
# )

## ----echo=FALSE---------------------------------------------------------------
get_query(
   ch_hpo,
   query=sprintf(
      "SELECT * from %s WHERE lower(label) LIKE '%s'",
      ifelse(
         "HPO_diseases" %in% names(ch_hpo),
         "HPO_diseases",
         "Diseases"
      ),
      "%epilep%"
   )
)

## ----echo=FALSE---------------------------------------------------------------
## The following line is to avoid building errors on CRAN
knitr::opts_chunk$set(eval=TRUE)

## ----eval=FALSE---------------------------------------------------------------
# k <- chTKCat(user="pgodard")
# create_chTKCat_user(
#    k, login="lfrancois", contact=NA, admin=FALSE, provider=TRUE
# )

## ----eval=FALSE---------------------------------------------------------------
# k <- chTKCat(user="pgodard")
# change_chTKCat_password(k, "lfrancois")
# update_chTKCat_user(k, contact="email", admin=FALSE)

## ----eval=FALSE---------------------------------------------------------------
# manage_chTKCat_users(k)

## ----eval=FALSE---------------------------------------------------------------
# drop_chTKCat_user(k, login="lfrancois")

## ----eval=FALSE---------------------------------------------------------------
# create_chMDB(k, "CHEMBL", public=FALSE)

## ----eval=FALSE---------------------------------------------------------------
# set_chMDB_access(k, "CHEMBL", public=TRUE)

## ----eval=FALSE---------------------------------------------------------------
# add_chMDB_user(k, "CHEMBL", "lfrancois", admin=TRUE)
# # remove_chMDB_user(k, "CHEMBL", "lfrancois")
# list_chMDB_users(k, "CHEMBL")

## ----eval=FALSE---------------------------------------------------------------
# lc <- scan_fileMDBs("fileMDB_directory")
# ## The commented line below allows the exploration of the data models in lc.
# # explore_MDBs(lc)
# for(r in toFeed){
#    message(r)
#    lr <- as_memoMDB(lc[[r]])
#    cr <- as_chMDB(lr, k, overwrite=FALSE)
# }

## ----eval=FALSE---------------------------------------------------------------
# empty_chMDB(k, "CHEMBL")

## ----eval=FALSE---------------------------------------------------------------
# drop_chMDB(k, "CHEMBL")

## ----eval=FALSE---------------------------------------------------------------
# add_chTKCat_collection(k, "BE")
# list_chTKCat_collections(k)
# remove_chTKCat_collection(k, "BE")

## ----echo=FALSE---------------------------------------------------------------
plot(TKCat:::DEFAULT_DATA_MODEL)

## ----echo=FALSE---------------------------------------------------------------
plot(TKCat:::CHMDB_DATA_MODEL)

## -----------------------------------------------------------------------------
list_local_collections()

## ----eval=FALSE---------------------------------------------------------------
# get_local_collection("BE")

## ----echo=FALSE, results='asis'-----------------------------------------------
get_local_collection("BE") %>%
   paste('```json', ., '```', sep="\n") %>% cat()

## -----------------------------------------------------------------------------
get_local_collection("BE") %>%
   show_collection_def()

## ----eval=FALSE---------------------------------------------------------------
# system.file(
#    "examples/CHEMBL/model/Collections/BE-CHEMBL_BE_1.0.json",
#    package="TKCat"
# ) %>%
#    readLines() %>% paste(collapse="\n")

## ----echo=FALSE, results='asis'-----------------------------------------------
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
   schema=get_local_collection("BE"),
   engine="ajv"
)

## ----eval=FALSE---------------------------------------------------------------
# get_collection_mapper("BE")

## ----echo=FALSE, results='asis'-----------------------------------------------
get_collection_mapper("BE") %>% 
   format() %>% paste(collapse="\n") %>% 
   paste('```r', ., '```', sep="\n") %>% cat()

## -----------------------------------------------------------------------------
d <- matrix(
   rnorm(40), nrow=10,
   dimnames=list(
      paste0("g", 1:10),
      paste0("s", 1:4)
   )
)
m <- ReDaMoR::df_to_model(d) %>% 
   ReDaMoR::rename_field("d", "row", "gene") %>%
   update_field("d", "gene", comment="Gene identifier") %>% 
   ReDaMoR::rename_field("d", "column", "sample") %>% 
   update_field("d", "sample", comment="Sample identifier") %>% 
   ReDaMoR::rename_field("d", "value", "expression") %>% 
   update_field(
      "d", "expression", nullable=FALSE, comment="Gene expression value"
   )
md <- memoMDB(list(d=d), m, list(name="Matrix example"))
plot(data_model(md))

## -----------------------------------------------------------------------------
ch_config_files <- tibble(
   name=c("config.xml", "users.xml"),
   file=c(
      base64enc::base64encode(
         system.file("ClickHouse/config.xml", package="TKCat")
      ),
      base64enc::base64encode(
         system.file("ClickHouse/users.xml", package="TKCat")
      )
   )
)
m <- df_to_model(ch_config_files) %>% 
   update_field(
      "ch_config_files", "name",
      type="base64", comment="Name of the config file",
      nullable=FALSE, unique=TRUE
   ) %>% 
   update_field(
      "ch_config_files", "file",
      type="base64", comment="Config file in base64 format",
      nullable=FALSE
   )
md <- memoMDB(
   list(ch_config_files=ch_config_files), m, list(name="base64 example")
)
plot(data_model(md))

