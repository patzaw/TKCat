###############################################################################@
#' Import a  the definition of a collection of concepts in the local environment
#' 
#' @param txt a JSON string or file
#' @param overwrite a single logical. If TRUE the collection is overwritten
#' if it already exists (default: FALSE)
#' 
#' @return No return value, called for side effects. The collection will be
#' available and operations will be possible on its members.
#' 
#' @export
#' 
import_local_collection <- function(txt, overwrite=FALSE){
   
   if(file.exists(txt)){
      raw <- readLines(txt) %>% paste(collapse="\n")
   }else{
      raw <- txt
   }
   
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
   invisible()
}


###############################################################################@
#' List local collections of concepts
#' 
#' @param withJson if TRUE, returns the json strings of the collection
#' (default: FALSE)
#' 
#' @return A tibble with the title, the description and optionally the json
#' definition of the collections
#' 
#' @export
#'
list_local_collections <- function(withJson=FALSE){
   toRet <- tkcatEnv$COLLECTIONS
   if(!withJson){
      toRet <- dplyr::select(toRet, "title", "description")
   }
   return(toRet)
}


###############################################################################@
#' Get the json definition of a local collection of concepts
#' 
#' @param title the title of the collection to get
#' 
#' @return The definition of the collection as a JSON string.
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


###############################################################################@
#' Import a  function to map collection members
#' 
#' @param collection the name of the targeted collection
#' (it should belong to local collections: see [list_local_collections()]).
#' @param fun a function which takes 2 data.frames (x an y) with
#' fields described in the collection definition and map the different elements.
#' 
#' @return No return value, called for side effects. The function will be used
#' to map collection members.
#' 
#' @export
#' 
import_collection_mapper <- function(collection, fun){
   stopifnot(
      is.function(fun),
      c("x", "y", "...") %in% names(formals(fun)),
      collection %in% list_local_collections()$title
   )
   toAdd <- list(fun)
   names(toAdd) <- collection
   assign(
      x="MAPPERS",
      value=c(tkcatEnv$MAPPERS, toAdd),
      envir=tkcatEnv
   )
   invisible()
}


###############################################################################@
#' Get the default mapper function for a collection
#' 
#' @param collection the name of the targeted collection
#' (it should belong to local collections: see [list_local_collections()]).
#' 
#' @return A function to map collection members.
#' 
#' @export
#' 
get_collection_mapper <- function(collection){
   get("MAPPERS", envir=tkcatEnv)[[collection]]
}

###############################################################################@
#' Map different collection members
#' 
#' @param x a data.frame
#' @param y a data.frame
#' @param collection the name of the collection.
#' @param xm collection member x: a data.frame with the fields
#' "field", "static", "value", "type" as returned by
#' the [read_collection_members()] function.
#' @param ym collection member y: a data.frame with the fields
#' "field", "static", "value", "type" as returned by
#' the [read_collection_members()] function.
#' @param suffix the suffix to append to field names from x and y tables.
#' Default: `c("_x", "_y")`
#' @param fun the function used to map x and y collection members.
#' By default (NA) it is automatically identified if recorded in the system.
#' The way to write this function is provided in the details section.
#' @param ... additional parameters for the fun function.
#' 
#' @details fun must have at least an x and a y parameters.
#' Each of them should be a data.frame with all the field values
#' given in xm and ym. Additional parameters
#' can be defined and will be forwarded using `...`.
#' fun should return a data frame with all the fields values
#' given in xm and ym followed by "_x" and "_y" suffix.
#' 
#' @return A tibble giving necessary information to map elements in x and y.
#' The columns corresponds to the field values in xm and ym followed by a
#' suffix (default: `c("_x", "_y")`). Only fields documented as non static
#' in xm and ym are kept.
#' 
#' @export
#' 
map_collection_members <- function(
   x, y,
   collection,
   xm, ym,
   suffix=c("_x", "_y"),
   fun=NA, ...
){
   stopifnot(
      all(xm$value[which(!xm$static)] %in% colnames(x)),
      all(ym$value[which(!ym$static)] %in% colnames(y))
   )
   if(!is.function(fun) && !is.na(fun)){
      stop("fun should be NA or a function")
   }
   if(!is.function(fun)){
      fun <- get_collection_mapper(collection=collection)
   }
   
   ## Prepare for conversion ----
   ## x
   xp <- select(
      x,
      dplyr::all_of(xm$value[which(!xm$static)])
   ) %>% 
      magrittr::set_colnames(xm$field[which(!xm$static)])
   for(i in 1:nrow(xm)){
      f <- xm$field[i]
      if(xm$static[i]){
         xp[,f] <- xm$value[i]
      }
      if(!is.na(xm$type[i])){
         xp[,paste0(f, "_type")] <- xm$type[i]
      }
   }
   ## y
   yp <- select(
      y,
      dplyr::all_of(ym$value[which(!ym$static)])
   ) %>% 
      magrittr::set_colnames(ym$field[which(!ym$static)])
   for(i in 1:nrow(ym)){
      f <- ym$field[i]
      if(ym$static[i]){
         yp[,f] <- ym$value[i]
      }
      if(!is.na(ym$type[i])){
         yp[,paste0(f, "_type")] <- ym$type[i]
      }
   }

   ## Conversion ----
   toRet <- dplyr::as_tibble(fun(xp, yp, ...))
   stopifnot(all(
      c(paste0(xm$field, "_x"), paste0(ym$field, "_y")) %in%
      colnames(toRet)
   ))
   
   ## Post-processing ----
   toKeep <- c(
      paste0(xm$value, suffix[1])[which(!xm$static)],
      paste0(ym$value, suffix[2])[which(!ym$static)]
   ) %>% 
      magrittr::set_names(c(
         paste0(xm$field, "_x")[which(!xm$static)],
         paste0(ym$field, "_y")[which(!ym$static)]
      ))
   toRet <- dplyr::select(toRet, all_of(names(toKeep)))
   colnames(toRet) <- toKeep[colnames(toRet)]
   
   return(toRet)
   
}
