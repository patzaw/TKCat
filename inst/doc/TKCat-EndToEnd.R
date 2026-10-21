## ----setup, message=FALSE, echo=FALSE, include=FALSE, cache=FALSE, eval=TRUE-------------------------------------------------------------------------------------
library(knitr)
opts_chunk$set(
   include = TRUE,
   echo = TRUE,
   eval = FALSE,
   message = TRUE,
   warning = TRUE,
   cache = FALSE
)
library(TKCat)

## ----eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
# library(TKCat)
# # Connect to your ClickHouse instance with the custom ports
# k <- chTKCat(
#    host = "localhost",
#    port = 9113L, # Using the custom HTTP port
#    user = "admin", # The admin user created by the docker-init script
#    password = "", # No password after initialization !!
#    drv = ClickHouseHTTP::ClickHouseHTTP()
# )
# 
# # Change the admin password (you will be prompted for the new password)
# change_chTKCat_password(k, "admin")
# db_reconnect(k)
# 
# # Add additional users with appropriate permissions
# # Create a regular user with read-only access
# create_chTKCat_user(k, login = "read_only_user", contact = NA, admin = FALSE)
# 
# # Grant access to specific MDBs (after creating them)
# # add_chMDB_user(k, mdb = "HPO", login = "read_only_user", admin = FALSE)

## ----eval=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------
# List available tables in the HPO subset
hpo_files <- list.files(system.file("examples/HPO-subset", package = "ReDaMoR"))
hpo_files

## ----eval=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------
hpo_model <- ReDaMoR::read_json_data_model(system.file(
  "examples/HPO-model.json",
  package = "ReDaMoR"
))
plot(hpo_model)

## ----eval=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------
## Reading the data
hpo_data <- lapply(hpo_files, function(x) {
   readr::read_tsv(
      file.path(
         system.file("examples/HPO-subset", package = "ReDaMoR"),
         x
      ),
      ## Column types taken from the data model
      col_types = ReDaMoR::col_types(hpo_model[[sub("[.]txt", "", x)]])
   )
}) |>
   setNames(sub("[.]txt", "", hpo_files))
## Creating the MDB
memo_hpo <- memoMDB(
   dataTables = hpo_data,
   dataModel = hpo_model,
   dbInfo = list(
      "name" = "HPO",
      "title" = "Data extracted from the HPO database",
      "description" = paste(
         "This is a very small subset of the HPO!",
         "Visit the reference URL for more information."
      ),
      "url" = "http://human-phenotype-ontology.github.io/"
   )
)

## ----eval=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------
memo_hpo <- memo_hpo |>
   add_collection_member(
      collection = "Condition",
      table = "HPO_hp",
      condition = list(value = "Phenotype", static = TRUE),
      source = list(value = "HP", static = TRUE),
      identifier = list(value = "id", static = FALSE)
   ) |>
   add_collection_member(
      collection = "Condition",
      table = "HPO_diseases",
      condition = list(value = "Disease", static = TRUE),
      source = list(value = "db", static = FALSE),
      identifier = list(value = "id", static = FALSE)
   )

## ----eval=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------
# Load ClinVar data from TKCat
file_clinvar <- read_fileMDB(
   path = system.file("examples/ClinVar", package = "TKCat")
)

## ----eval=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------
# Load ChEMBL data from TKCat
file_chembl <- read_fileMDB(
   path = system.file("examples/CHEMBL", package = "TKCat")
)

## ----eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
# # Connect to chTKCat (adjust parameters as needed)
# k <- chTKCat(
#    host = "localhost",
#    port = 9113L, # Using the custom HTTP port
#    user = "admin", # The admin user created by the docker-init script
#    drv = ClickHouseHTTP::ClickHouseHTTP()
# )

## ----eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
# # Create database for HPO
# create_chMDB(k, "HPO", public = TRUE)
# 
# # Create database for ClinVar
# create_chMDB(k, "ClinVar", public = TRUE)
# 
# # Create database for ChEMBL
# create_chMDB(k, "CHEMBL", public = TRUE)

## ----eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
# # Push HPO MDB
# ch_hpo <- as_chMDB(memo_hpo, k)
# 
# # Push ClinVar MDB
# ch_clinvar <- as_chMDB(file_clinvar, k)
# 
# # Push ChEMBL MDB
# ch_chembl <- as_chMDB(file_chembl, k)

## ----eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
# list_MDBs(k)

## ----eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
# k_readonly <- chTKCat(
#    host = "localhost",
#    port = 9113L, # Using the custom HTTP port
#    user = "default", # Default user has read access to public MDBs,
#    password = ""
# )

## ----eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
# # List all MDBs
# list_MDBs(k_readonly)
# 
# # Search for tables related to "disease"
# search_MDB_tables(k_readonly, "disease")
# 
# # Search for fields related to "gene"
# search_MDB_fields(k_readonly, "gene")
# 
# # Get collection members across all MDBs
# collection_members(k_readonly)

## ----eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
# explore_MDBs(k_readonly)

## ----eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
# hpo <- get_MDB(k_readonly, "HPO")
# plot(data_model(hpo))

## ----eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
# # Get first 10 phenotypes
# get_query(
#    hpo,
#    query = "SELECT * FROM HPO_hp LIMIT 10"
# )
# 
# # Search for abnormalities
# get_query(
#    hpo,
#    query = "SELECT * FROM HPO_hp WHERE lower(name) LIKE '%abnorm%'"
# )

## ----eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
# clinvar <- get_MDB(k_readonly, "ClinVar")
# 
# # Get the PIK3R2 gene
# get_query(
#    clinvar,
#    query = "SELECT * FROM ClinVar_entrezNames WHERE symbol = 'PIK3R2'"
# )
# 
# # Get pathogenic variants
# get_query(
#    clinvar,
#    query = paste(
#       "SELECT * FROM ClinVar_ReferenceClinVarAssertion ",
#       "WHERE lower(clinicalSignificance) LIKE '%pathogen%' ",
#       "LIMIT 10"
#    )
# )

## ----eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
# chembl <- get_MDB(k_readonly, "CHEMBL")
# 
# # Get first 10 drug targets
# get_query(
#    chembl,
#    query = "SELECT * FROM CHEMBL_component_sequence LIMIT 10"
# )
# 
# # Search for a molecule
# get_query(
#    chembl,
#    query = "SELECT * FROM CHEMBL_molecule_dictionary WHERE pref_name = 'DOCETAXEL'"
# )

## ----eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
# # Get diseases from HPO and associated traits from ClinVar
# # (assuming they share condition identifiers)
# get_query(
#    hpo,
#    query = paste(
#       "SELECT h.* FROM HPO_diseases h ",
#       "WHERE h.id IN (SELECT DISTINCT id FROM `ClinVar`.ClinVar_traitCref) ",
#       "LIMIT 10"
#    )
# )

## ----eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
# # Connect as admin
# k_admin <- chTKCat(host = "localhost", port = 9113, user = "admin")
# 
# # Create a dedicated MCP user
# create_chTKCat_user(
#   k_admin,
#   login = "mcp_user",
#   password = "XXXX",
#   contact = NA,
#   admin = FALSE
# )
# 
# # All users have read access to public MDBs
# # Give access to public MDBs (or specific private MDBs)
# # Access to private MDBs can be given as follows
# add_chMDB_user(k_admin, mdb = "HPO", login = "mcp_user", admin = FALSE)

## ----eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
# jsonlite::base64_enc("mcp_user:password")

