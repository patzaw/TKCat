#' Get a piece of knowledge from a [chTKCat] connection
#' 
#' @param x a [chTKCat] object
#' @param mdb_name the piece of knowlege to get
#' @param corpus_name the corpus to which the piece belongs
#' 
#' @export
#' 
get_pok <- function(x, mdb_name, corpus_name){
   
   mdb <- get_MDB(x, mdb_name, check=FALSE)
   kmr <- get_MDB(x, corpus_name, check=FALSE) %>% 
      as_KMR()
   kn <- db_info(kmr)$name
   mdb_helpers <- get_R_helpers(mdb, kmr=kmr)
   kmr_helpers <- get_R_helpers(kmr, mdb=mdb)
   ambig <- intersect(names(mdb_helpers), names(kmr_helpers))
   for(fn in ambig){
      names(kmr_helpers) <- sub(
         paste0("^", fn, "$"),
         paste0(kn, "_", fn),
         names(kmr_helpers)
      )
   }
   
   toRet <- list(
      mdb=mdb,
      kmr=kmr,
      helpers=c(mdb_helpers, kmr_helpers)
   )
   class(toRet) <- c("POK", class(toRet))
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
   db_reconnect(mdb, user=user, password=password, ntries=ntries, ...)
   kmr <- x$kmr
   db_reconnect(kmr, user=user, password=password, ntries=ntries, ...)
   
   nv <- x
   nv$mdb <- mdb
   nv$kmr <- kmr
   class(nv) <- c("POK", class(nv))
   
   lenv <- lapply(nv$helpers, environment) %>% unique()
   for(i in 1:length(lenv)){
      lenv[[i]]$THISMDB <- mdb
      lenv[[i]]$THISKMR <- kmr
   }
   
   assign(xn, nv, envir=parent.frame(n=1))
   invisible(nv)
}
