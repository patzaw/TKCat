###############################################################################@
#' Import a  the definition of a collection of concepts in the local environment
#' 
#' @param f the JSON file to import
#' @param overwrite a single logical. If TRUE the collection is overwritten
#' if it already exists (default: FALSE)
#' 
#' @importFrom jsonlite fromJSON
#' @importFrom jsonvalidate json_validate
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble filter bind_rows
#' @export
#' 
import_local_collection <- function(f, overwrite=FALSE){
   raw <- readLines(f) %>% paste(collapse="\n")
   if(!jsonvalidate::json_validate(raw, tkcatEnv$COL_SCHEMA, verbose=TRUE)){
      stop("Not a valid collection")
   }
   def <- jsonlite::fromJSON(raw)
   if(
      def$properties$collection$enum %in% list_local_collections()$title &&
      !overwrite
   ){
      stop(
         sprintf(
            'A "%s" has already been imported.',
            def$properties$collection$enum
         ),
         " Set overwrite to TRUE if you want to replace it."
      )
   }
   assign(
      x="COLLECTIONS",
      value=tkcatEnv$COLLECTIONS %>%
         dplyr::filter(title != def$properties$collection$enum) %>%
         dplyr::bind_rows(dplyr::tibble(
            title=def$properties$collection$enum,
            description=def$description,
            json=raw
         )),
      envir=tkcatEnv
   )
}


###############################################################################@
#' List local collections of concepts
#' 
#' @importFrom dplyr select
#' 
#' @export
#'
list_local_collections <- function(){
   tkcatEnv$COLLECTIONS %>% select(title, description)
}
