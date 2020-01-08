###############################################################################@
#' Compare two MDB
#' 
#' @param current an MDB object (e.g. [internalMDB] or [chMDB])
#' @param new an MDB object (e.g. [internalMDB] or [chMDB])
#' 
#' @return A tibble with 3 columns:
#' - **Information**: Compared information
#' - **Current**: current value
#' - **New**: new value
#' - **Identical**: a logical indicating if the 2 values are identical
#'
#' @export
#'
compare_MDB <- function(current, new){
   infox <- dbInfo(current, countRecords=TRUE)
   infoy <- dbInfo(new, countRecords=TRUE)
   toRet <- tibble(
      "Information"=c("name", "title", "description", "url", "version"),
      "Current"=unlist(infox[
         c("name", "title", "description", "url", "version")
      ]),
      "New"=unlist(infoy[
         c("name", "title", "description", "url", "version")
      ])
   ) %>% mutate(
      "Identical"=Current==New
   )
   toRet <- rbind(
      toRet,
      tibble(
         "Information"="Model",
         "Current"=sprintf("%s tables", length(current)),
         "New"=sprintf("%s tables", length(new)),
         "Identical"=ReDaMoR::identical_RelDataModel(
            dataModel(current), dataModel(new)
         )
      )
   )
   nrx <- infox$table_records
   nrx <- nrx[sort(names(nrx))]
   nry <- infoy$table_records
   nry <- nry[sort(names(nry))]
   if(
      length(nrx)==length(nry) && length(nrx) > 0 &&
      all(names(nrx)==names(nry))
   ){
      toRet <- rbind(
         toRet,
         tibble(
            "Information"=c(sprintf("Table %s", names(nrx)), "Total"),
            "Current"=format(c(nrx, infox$records), big.mark=",", trim=FALSE),
            "New"=format(c(nry, infoy$records), big.mark=",", trim=FALSE)
         ) %>% mutate(
            "Identical"=c(nrx, infox$records)==c(nry, infoy$records)
         )
      )
   }else{
   }
   return(toRet)
}
