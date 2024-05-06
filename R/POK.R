###############################################################################@
#' Create a piece of knowledge (POK) from an [MDB] and a [KMR]object
#' 
#' @aliases POK
#' 
#' @param mdb a [MDB] object with KM specifications
#' @param kmr a [KMR] object with KM requirements
#' @param tkcat A [TKCat] or [chTKCat] object to make available in
#' helper environment
#' 
#' @return A POK object: a list with 3 slots:
#'    - $mdb: the provided [MDB] object
#'    - $kmr: the provided [KMR] object
#'    - $helpers: a list functions to leverage data from mdb and kmr
#' 
#' @export
#' 
create_POK <- function(mdb, kmr, tkcat=NULL){
   stopifnot(is.MDB(mdb), is.KMR(kmr))
   kn <- db_info(kmr)$name
   if(is.null(tkcat) && is.chMDB(mdb)){
      tkcat <- unclass(mdb)$tkcon
   }
   mdb_helpers <- get_R_helpers(mdb, kmr=kmr, tkcat=tkcat)
   kmr_helpers <- get_R_helpers(kmr, tkcat=tkcat, mdb=mdb)
   ambig <- intersect(names(mdb_helpers), names(kmr_helpers))
   for(fn in ambig){
      names(kmr_helpers) <- sub(
         paste0("^", fn, "$"),
         paste0(kn, "_", fn),
         names(kmr_helpers)
      )
   }
   ambig <- setdiff(ambig, "help")
   if(length(ambig) > 0){
      warning(sprintf(
         "%s '%s' functions have been renamed",
         kn,
         paste(ambig, collapse="', '")
      ))
   }
   toRet <- list(
      mdb=mdb,
      kmr=kmr,
      tkcat=tkcat,
      helpers=c(mdb_helpers, kmr_helpers)
   )
   class(toRet) <- c("POK", class(toRet))
   return(toRet)
}

###############################################################################@
#' Get a [POK] from a [chTKCat] connection or a [TKCat] object
#' 
#' @param x a [chTKCat] or a [TKCat] object 
#' @param mdb [MDB] object with KM specifications or its name in tkcat
#' @param kmr [KMR] object with KM requirements or its name in tkcat
#' 
#' @return A [POK] object
#' 
#' @export
#' 
get_POK <- function(x, mdb, kmr){
   
   stopifnot(
      is.TKCat(x) || is.chTKCat(x),
      is.MDB(mdb) || is.character(mdb),
      is.KMR(kmr) || is.character(kmr)
   )
   
   if(is.character(mdb)){
      mdb <- get_MDB(x, mdb, check=FALSE)
   }
   if(is.character(kmr)){
      kmr <- get_KMR(x, kmr, check=FALSE)
   }
   
   toRet <- create_POK(mdb, kmr)
   
   return(toRet)
   
}

###############################################################################@
#' Check if the object is a [POK] object
#' 
#' @param x any object
#' 
#' @return A single logical: TRUE if x is a [POK] object
#' 
#' @export
#'
is.POK <- function(x){
   inherits(x, "POK")
}


###############################################################################@
#' @export
#' 
format.POK <- function(x, ...){
   xn <- deparse(substitute(x))
   mdbn <- db_info(x$mdb)$name
   kmrn <- db_info(x$kmr)$name
   nm <- 6
   hn <- names(x$helpers)
   hnh <- head(hn, ifelse(length(hn) > nm+3, nm, length(hn)))
   toRet <- paste(
      sprintf("%s POK with %s requirements.", mdbn, kmrn),
      sprintf("This POK comes with %s helpers: ", length(hn)),
      paste(sprintf("   - %s", hnh), collapse="\n"),
      ifelse(length(hnh) < length(hn), "   ...\n", ""),
      paste(
         "You can get further information with",
         sprintf(
            "%s$helpers$help() and %s$helpers$%s_help() functions",
            xn, xn , kmrn
         )
      ),
      sep="\n"
   )
   return(toRet)
}

###############################################################################@
#' @export
#'
print.POK <- function(x, ...){
   cat(format(x, ...), "\n")
}

###############################################################################@
#' List available [POK]
#' 
#' @param x a [chTKCat] or [TKCat] object
#' @param kmr [KMR] object with KM requirements or its name in tkcat
#' 
#' @return The names of available POKs in x with kmr requirements.
#' 
#' @export
#' 
list_POKs <- function(x, kmr){
   stopifnot(
      is.TKCat(x) || is.chTKCat(x),
      is.KMR(kmr) || is.character(kmr)
   )
   if(is.character(kmr)){
      kmrn <- kmr
      kmr <- get_KMR(x, kmrn, check=FALSE)
   }else{
      kmrn <- db_info(kmr)$name
   }
   if(is.chTKCat(x)){
      con <- x$chcon
      toRet <- DBI::dbGetQuery(
         con,
         sprintf(
            "SELECT DISTINCT database FROM system.tables WHERE name IN ('%s')",
            sprintf("___%s-Tables___", kmrn)
         )
      )$database
   }else{
      toRet <- names(x)[unlist(lapply(x, function(mdb){
         sprintf("___%s-Tables___", kmrn) %in% names(mdb)
      }))]
   }
   return(toRet)
}


###############################################################################@
#' 
#' @rdname db_reconnect
#' @method db_reconnect POK
#' 
#' @export
#'
db_reconnect.POK <- function(x, user, password, ntries=3, ...){
   xn <- deparse(substitute(x))
   x <- unclass(x)
   mdb <- x$mdb
   if(is.chMDB(mdb)){
      db_reconnect(mdb, user=user, password=password, ntries=ntries, ...)
   }
   kmr <- x$kmr
   tkcat <- x$tkcat
   if(is.chTKCat(tkcat)){
      db_reconnect(mdb, user=user, password=password, ntries=ntries, ...)
   }
   
   nv <- x
   nv$mdb <- mdb
   nv$kmr <- kmr
   nv$tkcat <- tkcat
   class(nv) <- unique(c("POK", class(nv)))
   
   lenv <- lapply(nv$helpers, environment) %>% unique()
   for(i in 1:length(lenv)){
      lenv[[i]]$THISMDB <- mdb
      lenv[[i]]$THISKMR <- kmr
      lenv[[i]]$THISTKCAT <- tkcat
   }
   
   assign(xn, nv, envir=parent.frame(n=1))
   invisible(nv)
}
