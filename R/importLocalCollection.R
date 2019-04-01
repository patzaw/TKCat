#' Import a collection definition in the local environment
#' 
#' @param f the JSON file to import
#' 
#' @importFrom jsonlite fromJSON
#' @importFrom jsonvalidate json_validate
#' @export
#' 
importLocalCollection <- function(f){
   raw <- readLines(f) %>% paste(collapse="\n")
   if(!json_validate(raw, tkcatEnv$COL_SCHEMA, verbose=TRUE)){
      stop("Not a valid collection")
   }
   def <- fromJSON(raw)
   assign(
      x="COLLECTIONS",
      value=tkcatEnv$COLLECTIONS %>%
         bind_rows(tibble(
            title=def$properties$collection$enum,
            description=def$description,
            json=raw
         )),
      envir=tkcatEnv
   )
}

###############################################################################@
#' List local collections
#' 
#' @export
#'
listLocalCollections <- function(){
   tkcatEnv$COLLECTIONS %>% select(title, description)
}
