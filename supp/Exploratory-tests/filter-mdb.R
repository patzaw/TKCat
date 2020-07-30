library(TKCat)
library(stringr)
source("~/Documents/Development/TKCat/R/OnHold/filter_mdb.R")
dd <- chMDB(chTKCat(), "CortellisDD")
ddf <- filter_mdb(
   dd,
   list(
      CortellisDD_drugBrandNames=expression(
         str_detect(nameBrand, regex("keppra", ignore_case=TRUE))
      )
   )
)
