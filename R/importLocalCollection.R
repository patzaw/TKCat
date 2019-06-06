#' Import a collection definition in the local environment
#' 
#' @param f the JSON file to import
#' @param overwrite a single logical. If TRUE the collection is overwritten
#' if it already exists (default: FALSE)
#' 
#' @importFrom jsonlite fromJSON
#' @importFrom jsonvalidate json_validate
#' @importFrom magrittr %>%
#' @importFrom dplyr filter bind_rows
#' @importFrom tibble tibble
#' @export
#' 
importLocalCollection <- function(f, overwrite=FALSE){
   raw <- readLines(f) %>% paste(collapse="\n")
   if(!json_validate(raw, tkcatEnv$COL_SCHEMA, verbose=TRUE)){
      stop("Not a valid collection")
   }
   def <- fromJSON(raw)
   if(
      def$properties$collection$enum %in% listLocalCollections()$title &&
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
         filter(title != def$properties$collection$enum) %>%
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
