###############################################################################@
#' Read a collection member JSON file
#' 
#' @param txt a JSON string or file
#' 
#' @return A tibble with the description of the collection members of a
#' resource
#' 
#' @export
#' 
read_collection_members <- function(txt){
   
   if(file.exists(txt)){
      raw <- readLines(txt) %>% paste(collapse="\n")
   }else{
      raw <- txt
   }
   
   mbl <- jsonlite::fromJSON(raw, simplifyVector=FALSE)
   if(!"collection" %in% names(mbl)){
      stop("Not a valid collection members JSON file")
   }
   schema <- tkcatEnv$COLLECTIONS %>%
      dplyr::filter(.data$title==mbl$collection) %>%
      dplyr::pull("json")
   if(length(schema)==0){
      stop(sprintf('"%s" is not defined as a collection', mbl$collection))
   }
   if(!jsonvalidate::json_validate(raw, schema, verbose=TRUE, engine="ajv")){
      stop(sprintf("Not valid %s members", mbl$collection))
   }
   mbl$tables <- unique(mbl$tables)
   
   toRet <- c()
   for(i in 1:length(mbl$tables)){
      mb <- mbl$tables[[i]]
      toAdd <- do.call(dplyr::bind_rows, lapply(
         mb$fields,
         dplyr::as_tibble
      ))
      toAdd$field <- names(mb$fields)
      toAdd$table <- mb$name
      toAdd$mid <- i
      toRet <- dplyr::bind_rows(toRet, toAdd)
   }
   
   if(!"type" %in% colnames(toRet)){
      toRet$type <- NA
   }
   toRet$collection <- mbl$collection
   toRet$cid <- mbl$"$id"
   toRet$resource <- mbl$resource
   toRet <- toRet %>%
      dplyr::mutate(type=as.character(.data$type)) %>%
      dplyr::select(
         "collection", "cid", "resource",
         "mid", "table",
         "field", "static", "value", "type"
      )
   return(toRet)
}


###############################################################################@
#' Write a collection member JSON file
#' 
#' @param colMembers A tibble as returned by [read_collection_members()]
#' @param path the JSON file to write. If `NA` (default), the JSON file is
#' not written but returned by the function.
#' @param collection The collection definition (json string).
#' If NULL (default), it is taken from TKCat environment
#' (see [list_local_collections()].
#' 
#' @return The JSON representation of collection members.
#' If a path is provided, then the JSON is also written in it.
#' 
#' @export
#' 
write_collection_members <- function(colMembers, path=NA, collection=NULL){
   colTitle <- unique(colMembers$collection)
   colId <- unique(colMembers$cid)
   resource <- unique(colMembers$resource)
   stopifnot(length(colTitle)==1, length(colId)==1, length(resource)==1)
   if(is.null(collection)){
      collection <- get_local_collection(colTitle)
   }
   toWrite <- list(
      "$schema"=jsonlite::unbox(jsonlite::fromJSON(collection)$"$id"),
      "$id"=jsonlite::unbox(colId),
      collection=jsonlite::unbox(colTitle),
      resource=jsonlite::unbox(resource),
      tables=list()
   )
   for(mid in unique(colMembers$mid)){
      m <- colMembers %>% dplyr::filter(.data$mid==!!mid)
      mToAdd <- list(
         name=jsonlite::unbox(unique(m$table)),
         fields=list()
      )
      for(field in unique(m$field)){
         f <- m %>% dplyr::filter(.data$field==!!field)
         fToAdd <- list(
            "static"=jsonlite::unbox(as.logical(f$static)),
            "value"=jsonlite::unbox(as.character(f$value))
         )
         if(!is.na(f$type)){
            fToAdd$type <- jsonlite::unbox(as.character(f$type))
         }
         mToAdd$fields[[field]] <- fToAdd
      }
      toWrite$tables <- c(toWrite$tables, list(mToAdd))
   }
   toWrite <- jsonlite::toJSON(toWrite, pretty=TRUE)
   if(!jsonvalidate::json_validate(
      toWrite, collection, verbose=TRUE, engine="ajv"
   )){
      stop(
         "Invalid collection members json:",
         "check colMembers and collection parameters"
      )
   }
   if(!is.na(path)){
      writeLines(toWrite, path)
      invisible(toWrite)
   }else{
      return(toWrite)
   }
}

###############################################################################@
#' Show the definition of a collection
#' 
#' This function prints details regarding a collection: title, description
#' and arguments information. These arguments are those that can be
#' used to document collection members within an [MDB] using 
#' the [add_collection_member()] function.
#' 
#' @param collection a json string with the collection definition as returned
#' by [get_local_collection()]
#' @param silent a logical indicating if the definition should be written
#' (TRUE by default) or not.
#' 
#' @return A list with:
#' - collection **title**
#' - collection **description**
#' - a list of **arguments** for defining collection members as a list
#' of elements with:
#'    - the **type** of the argument element
#'    - **allowed** values if any
#'    
#' @examples 
#' get_local_collection("BE") %>% show_collection_def()
#' 
#' @export
#' 
show_collection_def <- function(collection, silent=FALSE){
   collection <- jsonlite::fromJSON(collection)
   p <- collection$properties$tables$items$properties$fields$properties %>% 
      lapply(function(x){
         toRet <- lapply(
            x$properties,
            function(y){
               type <- y$type
               if(is.null(type) && "enum" %in% names(y)){
                  type <- "enum"
                  allowed <- y$enum
               }else{
                  allowed <- NULL
               }
               if(is.null(type)){
                  stop("Unsupported type?")
               }
               type <- ifelse(
                  type=="boolean", "logical",
                  ifelse(
                     type %in% c("string", "enum"),
                     "character",
                     NA
                  )
               )
               return(list(type=type, allowed=allowed))
            }
         )
         return(list(elements=toRet, mandatory=FALSE))
      })
   for(mp in collection$properties$tables$items$properties$fields$required){
      p[[mp]]$mandatory <- TRUE
   }
   
   if(!silent){
      cat(paste0(collection$title, ": ", collection$description), sep="\n")
      cat(
         "Arguments (non-mandatory arguments are between parentheses):",
         sep="\n"
      )
      z <- lapply(names(p), function(pn){
         x <- p[[pn]]
         if(x$mandatory){
            cat(sprintf("   - %s:", pn), sep="\n")
         }else{
            cat(sprintf("   - (%s):", pn), sep="\n")
         }
         lapply(names(x$elements), function(en){
            y <- x$elements[[en]]
            cat(sprintf(
               "      + %s: %s%s",
               en, y$type,
               ifelse(
                  is.null(y$allowed), "",
                  sprintf(" in '%s'", paste(y$allowed, collapse="', '"))
               )
            ), sep="\n")
         })
      })
   }
   
   invisible(list(
      title=collection$title,
      descripiton=collection$description,
      arguments=p
   ))
}
