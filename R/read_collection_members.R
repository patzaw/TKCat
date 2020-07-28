#' Read a collection member JSON file
#' 
#' @param f the JSON file to read
#' 
#' @return A tibble with the description of the collection members of a
#' resource
#' 
#' @export
#' 
read_collection_members <- function(f){
   raw <- readLines(f) %>% paste(collapse="\n")
   mbl <- jsonlite::fromJSON(raw, simplifyVector=FALSE)
   if(!"collection" %in% names(mbl)){
      stop("Not a valid collection members JSON file")
   }
   schema <- tkcatEnv$COLLECTIONS %>%
      dplyr::filter(.data$title==mbl$collection) %>%
      dplyr:: pull("json")
   if(length(schema)==0){
      stop(sprintf('"%s" is not defined as a collection', mbl$collection))
   }
   if(!jsonvalidate::json_validate(raw, schema, verbose=TRUE)){
      stop(sprintf("Not valid %s members", mbl$collection))
   }
   mbl$tables <- unique(mbl$tables)
   
   toRet <- c()
   for(i in 1:length(mbl$tables)){
      mb <- mbl$tables[[i]]
      toAdd <- do.call(dplyr::bind_rows, lapply(
         mb$fields,
         as_tibble
      ))
      toAdd$field <- names(mb$fields)
      toAdd$table <- mb$name
      toAdd$cid <- i
      toRet <- dplyr::bind_rows(toRet, toAdd)
   }
   
   if(!"type" %in% colnames(toRet)){
      toRet$type <- NA
   }
   toRet$collection <- mbl$collection
   toRet$resource <- mbl$resource
   toRet <- toRet %>%
      dplyr::mutate(type=as.character(.data$type)) %>%
      dplyr::select(
         "collection", "resource", "cid", "table",
         "field", "static", "value", "type"
      )
   return(toRet)
}
