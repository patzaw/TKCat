###############################################################################@
#' Get a ClickHouse modeled database
#'
#' @param tkcon a chTKCat object
#' @param dbName the name of the database
#'
#' @return A chMDB object
#'
#' @export
#'
chMDB <- function(tkcon, dbName){
   stopifnot(
      is.chTKCat(tkcon),
      dbName %in% listMDBs(tkcon)$name
   )
   toRet <- list(tkcon=tkcon, dbName=dbName)
   class(toRet) <- "chMDB"
   return(toRet)
}

###############################################################################@
#' @export
#' 
is.chMDB <- function(x){
   inherits(x, "chMDB")
}


###############################################################################@
#' @export
#'
dbInfo.chMDB <- function(x, ...){
   xl <- unclass(x)
   adbs <- listMDBs(xl$tkcon)
   toRet <- adbs %>% filter(name==xl$dbName) %>% as.list()
   toRet$records <- sum(unlist(lapply(
      names(x),
      function(y){
         suppressWarnings(dbGetQuery(
            tkcon$chcon,
            sprintf(
               "select count() from `%s`.`%s`",
               xl$dbName, y
            )
         ))[,1]
      }
   )))
   return(toRet)
}

###############################################################################@
#' @export
#'
dataModel.chMDB <- function(x, ...){
   x <- unclass(x)
   dbm <- list(
      tables=dbGetQuery(
         x$tkcon$chcon, sprintf("SELECT * FROM `%s`.`___tables___`", x$dbName)
      ),
      fields=dbGetQuery(
         x$tkcon$chcon, sprintf("SELECT * FROM `%s`.`___fields___`", x$dbName)
      ),
      primaryKeys=dbGetQuery(
         x$tkcon$chcon, sprintf("SELECT * FROM `%s`.`___primaryKeys___`", x$dbName)
      ),
      foreignKeys=dbGetQuery(
         x$tkcon$chcon, sprintf("SELECT * FROM `%s`.`___foreignKeys___`", x$dbName)
      ),
      indexes=dbGetQuery(
         x$tkcon$chcon, sprintf("SELECT * FROM `%s`.`___indexes___`", x$dbName)
      )
   )
   dbm$fields$nullable <- as.logical(dbm$fields$nullable)
   dbm$indexes$unique <- as.logical(dbm$indexes$unique)
   return(fromDBM(dbm))
}

###############################################################################@
#' @export
#'
dataTables.chMDB <- function(x, ...){
   m <- dataModel(x)
   x <- unclass(x)
   toTake <- unlist(list(...))
   if(length(toTake)>0){
      notInDb <- setdiff(toTake, names(m))
      if(length(notInDb)>0){
         stop(
            sprintf("The following tables are not in %s: ", x$dbName),
            paste(notInDb, sep=", ")
         )
      }
   }else{
      toTake <- names(m)
   }
   toRet <- lapply(
      toTake,
      function(tn){
         toRet <- dbGetQuery(
            x$tkcon$chcon,
            sprintf("SELECT * FROM `%s`.`%s`", x$dbName, tn)
         ) %>% as_tibble()
         for(cn in colnames(toRet)){
            toRet[,cn] <- asType(
               toRet %>% pull(get(cn)),
               m[[tn]]$fields$type[which(m[[tn]]$fields$name==cn)]
            )
         }
         return(toRet)
      }
   )
   names(toRet) <- toTake
   return(toRet)
}

###############################################################################@
#' @export
#'
format.chMDB <- function(x){
   xl <- unclass(x)
   dbi <- dbInfo(x)
   return(sprintf(
      paste(
         "chMDB %s (version %s): %s",
         "   - %s tables",
         "   - %s records",
         # "   - %s %s",
         "",
         "%s (%s)",
         sep="\n"
      ),
      dbi$name,
      dbi$version,
      dbi$title,
      length(x),
      format(
         dbi$records,
         big.mark=",",
         scientific=FALSE
      ),
      # round(hs, 1), u,
      dbi$description,
      dbi$url
   ))
}

###############################################################################@
#' @export
#'
print.chMDB <- function(x, ...){
   cat(format(x, ...), "\n")
}


###############################################################################@
#' @export
#'
length.chMDB <- function(x){
   length(dataModel(x))
}

###############################################################################@
#' @export
#'
'[<-.chlMDB' <- function(x, i, value){
   stop("'[<-' is not supported for chMDB")
}

###############################################################################@
#' @export
#'
'[[<-.chMDB' <- function(x, i, value){
   stop("'[[<-' is not supported for chMDB")
}

###############################################################################@
#' @export
#'
'$<-.chMDB' <- function(x, i, value){
   stop("'$<-' is not supported for chMDB")
}

###############################################################################@
#' @export
#'
c.internalMDB <- function(...){
   stop("Cannot concatenate several chMDB")
}


###############################################################################@
#' @export
#'
names.chMDB <- function(x, ...){
   names(dataModel(x))
}


###############################################################################@
#' @export
#'
'names<-.chMDB' <- function(x, ...){
   stop("Cannot change the names of an chMDB object")
}

###############################################################################@
#' @export
#'
plot.chMDB <- function(x, ...){
   plot(dataModel(x))
}

