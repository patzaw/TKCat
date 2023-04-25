source("~/opt/TKCat/tkcat-ucb.R")
library(here)

ntbkm <- read_fileMDB(
   here("supp/TBKM-requirements/TBKM/")
) %>% 
   as_memoMDB() %>% 
   as_KMR()

chtbkm <- as_chMDB(ntbkm, .tkcon)
