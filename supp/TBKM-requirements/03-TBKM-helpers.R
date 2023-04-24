library(TKCat)
library(here)

###############################################################################@
## Initialization ----
tbkm <- read_fileMDB(here("supp/TBKM-requirements/TBKM")) %>% 
   as_memoMDB() %>% 
   as_KMR()

###############################################################################@
## Read helpers ----
tbkm <- add_helpers(
   tbkm,
   code=here("supp/TBKM-requirements/Helpers/R-Helpers.R"),
   name="R-Helpers",
   language = "R"
)

###############################################################################@
## Save requirements ----
fp <- here("supp/TBKM-requirements/TBKM")
if(file.exists(fp)){
   file.rename(fp, here(sprintf("supp/TBKM-requirements/%s-TBKM", Sys.time())))
}
ftbkm <- as_fileMDB(tbkm, here("supp/TBKM-requirements/"), htmlModel=FALSE)
