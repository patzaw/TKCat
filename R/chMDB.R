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
dbInfo.chMDB <- function(x, countRecords=TRUE, ...){
   xl <- unclass(x)
   adbs <- listMDBs(xl$tkcon)
   toRet <- adbs %>% filter(name==xl$dbName) %>% as.list()
   if(countRecords){
      toRet$records <- sum(unlist(lapply(
         names(x),
         function(y){
            suppressWarnings(dbGetQuery(
               tkcon$chcon,
               sprintf(
                  "SELECT count() FROM `%s`.`%s`",
                  xl$dbName, y
               )
            ))[,1]
         }
      )))
   }
   return(toRet)
}

###############################################################################@
#' @export
#'
collectionMembers.chMDB <- function(
   x,
   collections=NULL,
   ...
){
   stopifnot(
      is.null(collections) || is.character(collections)
   )
   toRet <- dbGetQuery(
      conn=unclass(x)$tkcon$chcon,
      statement=
         sprintf(
            "SELECT * FROM default.CollectionMembers WHERE resource='%s' %s",
            dbInfo(x, countRecords=FALSE)$name,
            if(is.null(collections)){
               ""
            }else{
               sprintf(
                  "AND collection IN ('%s')",
                  paste(collections, collapse="', '")
               )
            }
         )
   ) %>%
      as_tibble() %>%
      select(collection, resource, table, field, static, value, type) %>%
      mutate(static=as.logical(static))
   return(toRet)
}

###############################################################################@
setChMDBcollectionMembers <- function(x, value){
   stopifnot(is.chMDB(x))
   if(!is.null(value)){
      stopifnot(
         is.data.frame(value),
         all(
            colnames(value) %in%
               c(
                  "collection", "resource", "table",
                  "field", "static", "value", "type"
               )
         ),
         is.character(value$collection),
         is.character(value$resource),
         is.character(value$table),
         is.character(value$field),
         is.logical(value$static),
         is.character(value$value),
         is.character(value$type),
         all(value$resource==dbInfo(x, countRecords=FALSE)$name),
         all(
            value$collection %in%
               listChTKCatCollections(unlcass(x)$tkcon)$title
         ),
         sum(duplicated(value %>% select(collection, table, field)))==0
      )
      dm <- dataModel(x)
   }
   for(mbt in unique(value$table)){
      notFound <- setdiff(
         value$value[which(value$table==mbt & !value$static)],
         dm[[mbt]]$fields$name
      )
      if(length(notFound)>0){
         stop(
            sprintf(
               "Cannot find the following fields in the %s table: ",
               mbt
            ),
            paste(notFound, collapse=", ")
         )
      }
   }
   dbSendQuery(
      conn=unclass(x)$tkcon$chcon,
      statement=sprintf(
         "ALTER TABLE default.CollectionMembers DELETE WHERE resource='%s'",
         unique(value$resource)
      )
   )
   .dbinsert(
      conn=unclass(x)$tkcon$chcon,
      dbName="default",
      tableName="CollectionMembers",
      value=value
   )
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
         x$tkcon$chcon,
         sprintf("SELECT * FROM `%s`.`___primaryKeys___`", x$dbName)
      ),
      foreignKeys=dbGetQuery(
         x$tkcon$chcon,
         sprintf("SELECT * FROM `%s`.`___foreignKeys___`", x$dbName)
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
   cm <- collectionMembers(x)
   return(sprintf(
      paste(
         "chMDB %s (version %s): %s",
         "   - %s tables",
         "   - %s records",
         # "   - %s %s",
         "",
         "Collections: ",
         "%s",
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
      paste(
         unlist(lapply(
            unique(cm$collection),
            function(y){
               return(sprintf(
                  "   - %s %s members",
                  length(unique(cm$table[which(cm$collection==y)])),
                  y
               ))
            }
         )),
         collapse="\n"
      ),
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
'[.chMDB' <- function(x, i=NULL){
   dbi <- dbInfo(x)
   if(is.null(i)){
      i <- names(x)
      dbi$name <- dbi$name
   }else{
      dbi$name <- sprintf("SUBSET of %s", dbi$name)
   }
   stopifnot(
      is.character(i),
      all(i %in% names(x))
   )
   dm <- dataModel(x)[i, rmForeignKeys=TRUE]
   dt <- dataTables(x, i)
   cm <- collectionMembers(x) %>%
      filter(table %in% i) %>%
      mutate(resource=dbi$name)
   toRet <- internalMDB(
      dataModel=dm,
      dbTables=dt,
      dbInfo=dbi,
      colMembers=cm,
      checkTables=FALSE
   )
   return(toRet)
}

###############################################################################@
#' @export
#'
'[[.chMDB' <- '$.chMDB' <- function(x, i){
   stopifnot(
      is.character(i),
      length(i)==1,
      all(i %in% names(x))
   )
   return(dataTables(x, i)[[1]])
}

###############################################################################@
#' @export
#'
'[<-.chMDB' <- function(x, i, value){
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
c.chMDB <- function(...){
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

