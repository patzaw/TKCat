###############################################################################@
#' Compare two MDB
#' 
#' @param former an MDB object (e.g. [internalMDB] or [chMDB])
#' @param new an MDB object (e.g. [internalMDB] or [chMDB])
#' 
#' @return A tibble with 3 columns:
#' - **Information**: Compared information
#' - **Former**: former value
#' - **New**: new value
#' - **Identical**: a logical indicating if the 2 values are identical
#'
#' @export
#'
compare_MDB <- function(former, new){
   infox <- dbInfo(former, countRecords=TRUE)
   infoy <- dbInfo(new, countRecords=TRUE)
   
   ## General information ----
   mandFields <- c(
      "name", "title", "description", "url",
      "version", "maintainer"
   )
   toRet <- tibble(
      "Information"=mandFields,
      "Former"=unlist(infox[mandFields]),
      "New"=unlist(infoy[mandFields])
   ) %>% mutate(
      "Identical"=Former==New
   )
   
   ## Data model ----
   toRet <- rbind(
      toRet,
      tibble(
         "Information"=c("Model", "Model display"),
         "Former"=c(sprintf("%s tables", length(former)), ""),
         "New"=c(sprintf("%s tables", length(new)), ""),
         "Identical"=c(
            ReDaMoR::identical_RelDataModel(
               dataModel(former), dataModel(new), includeDisplay=FALSE
            ),
            ReDaMoR::identical_RelDataModel(
               dataModel(former), dataModel(new), includeDisplay=TRUE
            )
         )
      )
   )
   
   ## Records ----
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
            "Former"=format(c(nrx, infox$records), big.mark=",", trim=FALSE),
            "New"=format(c(nry, infoy$records), big.mark=",", trim=FALSE)
         ) %>% mutate(
            "Identical"=c(nrx, infox$records)==c(nry, infoy$records)
         )
      )
   }
   
   ## Collection members
   ccm <- collectionMembers(former)
   if(!is.null(ccm)){
      ccm <- ccm %>% arrange_all()
      ccoll <- ccm %>% distinct(collection, resource, cid) %>% nrow
   }else{
      ccoll <- 0
   }
   ncm <- collectionMembers(new)
   if(!is.null(ncm)){
      ncm <- ncm %>% arrange_all()
      ncoll <- ncm %>% distinct(collection, resource, cid) %>% nrow
   }else{
      ncoll <- 0
   }
   toRet <- rbind(
      toRet,
      tibble(
         "Information"="Collections",
         "Former"=ccoll,
         "New"=ncoll,
         "Identical"=(
            ccoll==ncoll && (
               ccoll==0 || identical(ccm, ncm)
            )
         )
      )
   )
   
   return(toRet)
}
