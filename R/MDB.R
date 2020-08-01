###############################################################################@
#' Check if the object is  an MDB object
#' 
#' @param x any object
#' 
#' @return A single logical: TRUE if x is an MDB object.
#' 
#' @export
#'
is.MDB <- function(x){
   inherits(x, "MDB")
}


###############################################################################@
#' @export
#'
names.MDB <- function(x){
   names(data_model(x))
}


###############################################################################@
#' Get the number of tables in an MDB object
#' 
#' @param x an MDB object
#' 
#' @return A single integer value
#' 
#' @export
#'
length.MDB <- function(x){
   length(data_model(x))
}


###############################################################################@
#' @export
#'
format.MDB <- function(x, ...){
   cm <- collection_members(x)
   maintainer <- db_info(x)$maintainer
   return(sprintf(
      paste(
         "%s %s (version %s%s): %s",
         "   - %s tables",
         "",
         "Collections: ",
         "%s",
         "",
         "%s (%s)",
         sep="\n"
      ),
      class(x)[1],
      db_info(x)$name,
      db_info(x)$version,
      ifelse(is.na(maintainer) || maintainer=="", "", paste(",", maintainer)),
      db_info(x)$title,
      length(x),
      paste(
         unlist(lapply(
            unique(cm$collection),
            function(y){
               return(sprintf(
                  "   - %s %s member%s",
                  length(unique(cm$table[which(cm$collection==y)])),
                  y,
                  ifelse(
                     length(unique(cm$table[which(cm$collection==y)]))>1,
                     "s", ""
                  )
               ))
            }
         )),
         collapse="\n"
      ),
      db_info(x)$description,
      db_info(x)$url
   ))
}


###############################################################################@
#' @export
#'
print.MDB <- function(x, ...){
   cat(format(x, ...), "\n")
}

###############################################################################@
#' @export
#'
select.MDB <- function(.data, ...){
   i <- unlist(list(...))
   .data[i]
}

###############################################################################@
#' @export
#'
'[<-.MDB' <- function(x, i, value){
   stop("'[<-' is not supported for .MDB")
}

###############################################################################@
#' @export
#'
'[[<-.MDB' <- function(x, i, value){
   stop("'[[<-' is not supported for .MDB")
}

###############################################################################@
#' @export
#'
'$<-.MDB' <- function(x, i, value){
   stop("'$<-' is not supported for .MDB")
}


###############################################################################@
## Helpers ----
.check_dbInfo <- function(dbInfo){
   mandFields <- c(
      "name", "title", "description", "url",
      "version", "maintainer"
   )
   for(f in mandFields){
      if(
         !is.character(dbInfo[[f]]) || length(dbInfo[[f]])!=1 ||
         is.na(dbInfo[[f]]) || dbInfo[[f]]==""
      ){
         stop(sprintf(
            "%s in dbInfo should be a non-empty character"
         ))
      }
   }
   dbInfo <- as.list(dbInfo[mandFields])
   return(dbInfo)
}
