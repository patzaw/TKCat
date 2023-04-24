###############################################################################@
## list_MDB_with_DE_analyses ----

#' List MDB with DE analyses
#' 
#' @param kmr the KMR object with TBKM specifications (by default, the KMR to which the function is attached). This KMR must also be a chMDB object with 
#' 
#' @return  a tibble with DE analyses description tables
#'   
#' @import TKCat dplyr
#' 
#' @export
#' 
list_MDB_with_DE_analyses <- function(kmr=THISKMR){
   stopifnot(
      TKCat::is_KMR(kmr), TKCat::is.chMDB(kmr)
   )
   k <- unclass(kmr)$tkcon
   n <- TKCat::db_info(kmr)$name
   mdbNames <- TKCat::get_query(
      k,
      sprintf(
         "SELECT database FROM system.tables WHERE name='%s'",
         sprintf("___%s-Tables___", n)
      )
   )$database
   query <- paste(
      sprintf(
         "SELECT *, '%s' as mdb FROM `%s`.`%s` WHERE type='DE analyses'",
         mdbNames, mdbNames, sprintf("___%s-Tables___", n)
      ),
      collapse=" UNION ALL "
   )
   toRet <- TKCat::get_query(k, query)
   return(toRet)
}
