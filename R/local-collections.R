###############################################################################@
#' Import a  the definition of a collection of concepts in the local environment
#' 
#' @param path the JSON file to import
#' @param overwrite a single logical. If TRUE the collection is overwritten
#' if it already exists (default: FALSE)
#' 
#' @export
#' 
import_local_collection <- function(path, overwrite=FALSE){
   raw <- readLines(path) %>% paste(collapse="\n")
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
         dplyr::filter(.data$title != def$properties$collection$enum) %>%
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
#' @export
#'
list_local_collections <- function(){
   tkcatEnv$COLLECTIONS %>% dplyr::select("title", "description")
}


###############################################################################@
#' Get the json definition of a local collection of concepts
#' 
#' @param title the title of the collection to get
#' 
#' @export
#'
get_local_collection <- function(title){
   if(!title %in% list_local_collections()$title){
      stop("This collection is not available")
   }
   tkcatEnv$COLLECTIONS %>%
      dplyr::filter(.data$title==!!title) %>%
      dplyr::pull("json")
}
