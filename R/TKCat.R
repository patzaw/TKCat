###############################################################################@
#' TKCat: a catalog of [MDB]
#'
#' @param list a list of [MDB] objects
#' @param ... [MDB] objects used if list is NULL
#'
#' @return a TKCat object
#' 
#' @seealso [scan_fileMDBs]
#' 
#' @export
#'
TKCat <- function(..., list=NULL){
   if(is.null(list)){
      toRet <- list(...)
   }else{
      toRet <- list
   }
   if(any(!unlist(lapply(toRet, is.MDB)))){
      stop("All provided objects should be MDB objects")
   }
   dbnames <- unlist(lapply(toRet, function(x) db_info(x)$name))
   if(any(duplicated(dbnames))){
      stop("MDB objects cannot have the same names")
   }
   names(toRet) <- dbnames
   class(toRet) <- c("TKCat", class(toRet))
   return(toRet)
}


###############################################################################@
#' Scan a catalog of [fileMDB]
#'
#' @param path directory from which all the [fileMDB] should be read
#' @param subdirs the sub directories (relative to path) to take into account.
#' If NULL (default) all the sub directories are considered.
#' @param n_max maximum number of records to read
#' for checks purpose (default: 10). See also [ReDaMoR::confront_data()].
#'
#' @return a TKCat object
#' 
#' @seealso [read_fileMDB]
#' 
#' @export
#'
scan_fileMDBs <- function(path, subdirs=NULL, n_max=10){
   if(is.null(subdirs)){
      files <- list.files(path=path, full.names=TRUE)
   }else{
      files <- file.path(path, subdirs)
   }
   toRet <- list()
   for(f in files){
      toAdd <- suppressWarnings(try(read_fileMDB(
         path=f, n_max=n_max
      ), silent=TRUE))
      if(!inherits(toAdd, "try-error")){
         toRet <- c(toRet, list(toAdd))
      }else{
         warning(paste(basename(f), as.character(toAdd), sep=": "))
      }
   }
   return(TKCat(list=toRet))
}


###############################################################################@
#' Check the object is  a [TKCat] object
#' 
#' @param x any object
#' 
#' @return A single logical: TRUE if x is a [TKCat] object
#' 
#' @export
#'
is.TKCat <- function(x){
   inherits(x, "TKCat")
}


###############################################################################@
#' @export
#'
format.TKCat <- function(x, ...){
   toRet <- sprintf("TKCat gathering %s MDB objects", length(x))
   return(toRet)
}


###############################################################################@
#' @export
#'
print.TKCat <- function(x, ...){
   cat(format(x, ...), "\n")
   invisible()
}

###############################################################################@
#' 
#' @param x a [TKCat] object
#' @param value new [MDB] names
#' 
#' @rdname TKCat
#' 
#' @export
#'
'names<-.TKCat' <- function(x, value){
   stopifnot(
      is.character(value),
      !any(is.na(value)),
      !any(duplicated(value)),
      length(value)==length(x)
   )
   x <- unclass(x)
   for(i in 1:length(x)){
      dbi <- db_info(x[[i]])
      dbi$name <- value[i]
      db_info(x[[i]]) <- dbi
   }
   names(x) <- value
   class(x) <- c("TKCat", class(x))
   return(x)
}

###############################################################################@
#' Rename a [TKCat] object
#'
#' @param .data a [TKCat] object
#' @param ... Use new_name = old_name to rename selected [MDB]
#' 
#' @export
#' 
rename.TKCat <- function(.data, ...){
   loc <- tidyselect::eval_rename(expr(c(...)), .data)
   names <- names(.data)
   names[loc] <- names(loc)
   set_names(.data, names)
}

###############################################################################@
#' 
#' @param x a [TKCat] object
#' @param i index or names of the MDB to take
#'
#' @rdname TKCat
#' 
#' @export
#'
'[.TKCat' <- function(x, i){
   x <- unclass(x)[i]
   class(x) <- c("TKCat", class(x))
   return(x)
}

###############################################################################@
#' @export
#'
'[<-.TKCat' <- function(x, i, value){
   stop("'[<-' is not supported for TKCat")
}

###############################################################################@
#' @export
#'
'[[<-.TKCat' <- function(x, i, value){
   stop("'[[<-' is not supported for TKCat")
}

###############################################################################@
#' @export
#'
'$<-.TKCat' <- function(x, i, value){
   stop("'$<-' is not supported for TKCat")
}


###############################################################################@
#'
#' @param ... [TKCat] objects
#'
#' @rdname TKCat
#' 
#' @export
#'
c.TKCat <- function(...){
   alltkcat <- list(...)
   if(length(alltkcat)==0){
      stop("At least one TKCat should be provided as an input")
   }
   if(any(!unlist(lapply(alltkcat, is.TKCat)))){
      stop("All arguments should be TKCat objects")
   }
   allnames <- unlist(lapply(alltkcat, names))
   if(any(duplicated(allnames))){
      stop("Same names cannot be used in the different TKCat objects")
   }
   toRet <- do.call(c, lapply(alltkcat, unclass))
   class(toRet) <- c("TKCat", class(toRet))
   return(toRet)
}


###############################################################################@
#' 
#' @rdname list_MDBs
#' @method list_MDBs TKCat
#' 
#' @export
#'
list_MDBs.TKCat <- function(x, withInfo=TRUE){
   if(!withInfo){
      return(names(x))
   }
   return(do.call(dplyr::bind_rows, lapply(
      x,
      function(y) dplyr::as_tibble(db_info(y))
   )))
}


###############################################################################@
#' 
#' @rdname get_MDB
#' @method get_MDB TKCat
#' 
#' @export
#'
get_MDB.TKCat <- function(x, dbName, ...){
   stopifnot(dbName %in% names(x))
   return(x[[dbName]])
}

###############################################################################@
#' 
#' @rdname collection_members
#' @method collection_members TKCat
#' 
#' @export
#'
collection_members.TKCat <- function(
   x,
   ...
){
   return(do.call(dplyr::bind_rows, lapply(
      x,
      function(y){
         collection_members(y) %>% 
            dplyr::select("resource", "collection", "table") %>% 
            dplyr::distinct()
      }
   )))
}
