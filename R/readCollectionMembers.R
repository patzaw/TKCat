#' Read a collection member JSON file
#' 
#' @param f the JSON file to read
#' 
#' @return A tibble with the descripition of the collection members of a
#' resource
#' 
#' @importFrom jsonlite fromJSON
#' @importFrom jsonvalidate json_validate
#' @export
#' 
readCollectionMembers <- function(f){
   raw <- readLines(f) %>% paste(collapse="\n")
   mbl <- fromJSON(raw, simplifyVector=FALSE)
   if(!"collection" %in% names(mbl)){
      stop("Not a valid collection members JSON file")
   }
   schema <- TKCat:::tkcatEnv$COLLECTIONS %>%
      filter(title==mbl$collection) %>%
      pull(json)
   if(length(schema)==0){
      stop(sprintf('"%s" is not defined as a collection', mbl$collection))
   }
   if(!json_validate(raw, schema, verbose=TRUE)){
      stop(sprintf("Not valid %s members", mbl$collection))
   }
   mbl$tables <- unique(mbl$tables)
   
   toRet <- c()
   for(i in 1:length(mbl$tables)){
      mb <- mbl$tables[[i]]
      toAdd <- do.call(bind_rows, lapply(
         mb$fields,
         as_tibble
      ))
      toAdd$field <- names(mb$fields)
      toAdd$table <- mb$name
      toAdd$cid <- i
      toRet <- bind_rows(toRet, toAdd)
   }
   
   if(!"type" %in% colnames(toRet)){
      toRet$type <- NA
   }
   toRet$collection <- mbl$collection
   toRet$resource <- mbl$resource
   toRet <- toRet %>%
      mutate(type=as.character(type)) %>%
      select(
         "collection", "resource", "cid", "table",
         "field", "static", "value", "type"
      )
   return(toRet)
}
