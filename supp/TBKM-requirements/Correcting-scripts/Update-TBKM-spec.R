source("~/opt/TKCat/tkcat-ucb.R")
library(here)

###################################################@##
## To adapt and run only when data model update     ##
## Should not happen often!                         ##
###################################################@##

sm <- read_json_data_model(here("inst/KM-Data-Model/KMSPEC.json"))

tbkm_list <- get_query(
   .tkcon,
   "SELECT database FROM system.tables WHERE name='___TBKM-Tables___'"
)$database

for(dbn in tbkm_list){
   message(dbn)
   mdb <- get_MDB(.tkcon, dbn, check=FALSE)
   mdbd <- mdb %>% 
      select(-all_of(intersect(names(mdb), sprintf(names(sm), "TBKM"))))
   
   tmp <- add_km_spec(mdbd, .tbkm)
   nmdbs <- get_km_spec(tmp, .tbkm)
   mtToKeep <- data_tables(mdb, all_of(intersect(names(mdb), names(nmdbs))))
   if(any(!names(nmdbs) %in% names(mtToKeep))){
      mt <- c(mtToKeep, data_tables(nmdbs, setdiff(names(nmdbs), names(mtToKeep))))
   }else{
      mt <- mtToKeep
   }
   nmdbs <- memoMDB(
      dataTables = mt,
      dataModel = data_model(nmdbs),
      dbInfo = db_info(nmdbs),
   )
   
   nmdb <- c(mdbd, nmdbs)
   chmdb <- as_chMDB(nmdb, .tkcon)
}
