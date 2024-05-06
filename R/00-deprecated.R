#' @export
#' 
get_pok <- function(tkcat, mdb_name, corpus_name){
   
   stopifnot(
      is.TKCat(tkcat) || is.chTKCat(tkcat)
   )
   
   kmr <- get_MDB(tkcat, corpus_name, check=FALSE) %>% 
         as_KMR()
   mdb <- get_MDB(tkcat, mdb_name, check=FALSE)
   kn <- db_info(kmr)$name
   mdb_helpers <- get_R_helpers(mdb, kmr=kmr)
   kmr_helpers <- get_R_helpers(kmr, tkcat=tkcat, mdb=mdb)
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
   class(toRet) <- c("dpok", class(toRet))
   return(toRet)
   
}


#' @export
#'
db_reconnect.dpok <- function(x, user, password, ntries=3, ...){
   xn <- deparse(substitute(x))
   x <- unclass(x)
   mdb <- x$mdb
   if(is.chMDB(mdb)){
      db_reconnect(mdb, user=user, password=password, ntries=ntries, ...)
   }
   kmr <- x$kmr
   
   nv <- x
   nv$mdb <- mdb
   nv$kmr <- kmr
   class(nv) <- c("dpok", class(nv))
   
   lenv <- lapply(nv$helpers, environment) %>% unique()
   for(i in 1:length(lenv)){
      lenv[[i]]$THISMDB <- mdb
      lenv[[i]]$THISKMR <- kmr
   }
   
   assign(xn, nv, envir=parent.frame(n=1))
   invisible(nv)
}
