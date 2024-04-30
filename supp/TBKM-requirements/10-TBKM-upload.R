source("~/opt/KMT.R")
library(here)

ntbkm <- read_KMR(
   here("supp/TBKM-requirements/TBKM/")
)

chtbkm <- as_chMDB(ntbkm, .tkcon)
