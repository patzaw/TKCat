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
   dbm <- list(
      tables=dbGetQuery(
         tkcon$chcon, sprintf("SELECT * FROM `%s`.`___tables___`", dbName)
      ),
      fields=dbGetQuery(
         tkcon$chcon, sprintf("SELECT * FROM `%s`.`___fields___`", dbName)
      ),
      primaryKeys=dbGetQuery(
         tkcon$chcon,
         sprintf("SELECT * FROM `%s`.`___primaryKeys___`", dbName)
      ),
      foreignKeys=dbGetQuery(
         tkcon$chcon,
         sprintf("SELECT * FROM `%s`.`___foreignKeys___`", dbName)
      ),
      indexes=dbGetQuery(
         tkcon$chcon, sprintf("SELECT * FROM `%s`.`___indexes___`", dbName)
      )
   )
   dbm$fields$nullable <- as.logical(dbm$fields$nullable)
   dbm$fields$unique <- as.logical(dbm$fields$unique)
   dbm$indexes$unique <- as.logical(dbm$indexes$unique)
   m <- ReDaMoR::fromDBM(dbm)
   toRet <- list(tkcon=tkcon, dbName=dbName, dbModel=m)
   class(toRet) <- "chMDB"
   return(toRet)
}

###############################################################################@
#' Check the object is  a [chMDB] object
#' 
#' @param x any object
#' 
#' @return A single logical: TRUE if x is a [chMDB] object
#' 
#' @export
#'
is.chMDB <- function(x){
   inherits(x, "chMDB")
}

###############################################################################@
#' Convert a [chMDB] in a list
#' 
#' @export
#'
as.list.chMDB <- function(x){
   dataTables(x)[names(x)]
}


###############################################################################@
#' Get DB information of a [chMDB] object
#' 
#' @param x a [chMDB] object
#' @param countRecords a single logical. If TRUE (default), the number of
#' records is also returned.
#' 
#' @return A list with the following elements:
#' - **name**: a single character
#' - **title**: a single character
#' - **description**: a single character
#' - **url**: a single character
#' - **version**: a single character
#' - (**table_records**): a numeric giving the number of records per table
#' - (**records**): a single numeric
#' 
#' @export
#'
dbInfo.chMDB <- function(x, countRecords=TRUE){
   xl <- unclass(x)
   adbs <- listMDBs(xl$tkcon)
   toRet <- adbs %>% filter(name==xl$dbName) %>% as.list()
   if(countRecords){
      toRet$table_records <- unlist(lapply(
         names(x),
         function(y){
            as.numeric(suppressWarnings(dbGetQuery(
               xl$tkcon$chcon,
               sprintf(
                  "SELECT count() FROM `%s`.`%s`",
                  xl$dbName, y
               )
            ))[,1])
         }
      ))
      names(toRet$table_records) <- names(x)
      toRet$records <- sum(toRet$table_records)
   }
   toRet$tkcon <- xl$tkcon
   return(toRet)
}

###############################################################################@
#' Get collection members of a [chMDB] object
#' 
#' @param x a [chMDB] object
#' @param collection a character vector indicating the name of the collections
#' to focus on (default: NULL ==> all of them)
#' 
#' @return A [tibble::tibble] with the following columns:
#' - **collection** (character): The name of the collection
#' - **resource** (character): The name of the resource
#' - **cid** (integer): collection member ID by resource
#' - **table** (character): The table recording collection information
#' - **field** (character): The collection field.
#' - **static** (logical): TRUE if the field value is common to all elements.
#' - **value** (character): The name of the table column if static is FALSE
#' or the field value if static is TRUE.
#' - **type** (character): the type of the field.
#' (not necessarily used ==> NA if not)
#' 
#' @export
#'
collectionMembers.chMDB <- function(
   x,
   collections=NULL
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
      select(collection, resource, cid, table, field, static, value, type) %>%
      mutate(static=as.logical(static))
   return(toRet)
}

###############################################################################@
#' Set collection members of a [chMDB] object
#' 
#' @param x a [chMDB] object
#' 
#' @param value A data.frame with the following columns:
#' - **collection** (character): The name of the collection
#' - **resource** (character): The name of the resource
#' - **cid** (integer): collection member ID by resource
#' - **table** (character): The table recording collection information
#' - **field** (character): The collection field.
#' - **static** (logical): TRUE if the field value is common to all elements.
#' - **value** (character): The name of the table column if static is FALSE
#' or the field value if static is TRUE.
#' - **type** (character): the type of the field.
#' (not necessarily used ==> NA if not)
#' 
setChMDBcollectionMembers <- function(x, value){
   stopifnot(is.chMDB(x))
   if(!is.null(value)){
      stopifnot(
         is.data.frame(value),
         all(
            colnames(value) %in%
               c(
                  "collection", "resource", "cid", "table",
                  "field", "static", "value", "type"
               )
         ),
         is.character(value$collection),
         is.character(value$resource),
         is.integer(value$cid),
         is.character(value$table),
         is.character(value$field),
         is.logical(value$static),
         is.character(value$value),
         is.character(value$type),
         all(value$resource==dbInfo(x, countRecords=FALSE)$name),
         all(
            value$collection %in%
               listChTKCatCollections(unclass(x)$tkcon)$title
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
#' Get the [ReDaMoR::RelDataModel] of a [chMDB] object
#' 
#' @param x a [chMDB] object
#' 
#' @return A [ReDaMoR::RelDataModel] object
#' 
#' @export
#'
dataModel.chMDB <- function(x){
   x <- unclass(x)
   return(x[["dbModel"]])
}

###############################################################################@
#' Get data tables from a [chMDB] object
#' 
#' @param x a [chMDB] object
#' @param ... the name of the tables to get (default: all of them)
#' 
#' @return A list of [tibble::tibble]
#' 
#' @export
#'
dataTables.chMDB <- function(x, ...){
   m <- dataModel(x)
   x <- unclass(x)
   toTake <- unlist(list(...))
   if(is.numeric(toTake)){
      toTake <- names(m)[toTake]
   }
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
            toRet[,cn] <- as_type(
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
#' Format a [chMDB] object for printing
#' 
#' @param x a [chMDB] object
#' 
#' @return A single character
#' 
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
#' Get the number of tables in a [chMDB] object
#' 
#' @param x a [chMDB] object
#' 
#' @return A single integer value
#' 
#' @export
#'
length.chMDB <- function(x){
   length(dataModel(x))
}

###############################################################################@
#' Load a subset of tables from a [chMDB] object as an [internalMDB] object
#' 
#' @param i the name of the tables to be taken. If NULL (default), all
#' the tables are loaded
#' 
#' @export
#'
'[.chMDB' <- function(x, i=NULL){
   dbi <- dbInfo(x)
   dbi <- dbi[which(names(dbi)!="records")]
   if(is.null(i)){
      i <- names(x)
      dbi$name <- dbi$name
   }else{
      dbi$name <- sprintf("SUBSET of %s", dbi$name)
   }
   # stopifnot(
   #    is.character(i),
   #    all(i %in% names(x))
   # )
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
      checks=c()
   )
   return(toRet)
}

###############################################################################@
subset_chMDB <- function(x, i){
   stopifnot(
      # is.character(i),
      length(i)==1
      # all(i %in% names(x))
   )
   ## Rstudio hack to avoid DB call when just looking for names
   cc <- grep('.rs.getCompletionsDollar', deparse(sys.calls()), value=FALSE)
   if(length(cc)!=0){
      invisible(NULL)
   }else{
      return(dataTables(x, i)[[1]])
      # dbi <- dbInfo(x)
      # return(tbl(dbi$tkcon$chcon, in_schema(dbi$name, i)))
   }
}
#' @export
'[[.chMDB' <- subset_chMDB
#' @export
'$.chMDB' <- subset_chMDB
rm(subset_chMDB)

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
names.chMDB <- function(x){
   names(dataModel(x))
}


###############################################################################@
#' @export
#'
'names<-.chMDB' <- function(x, ...){
   stop("Cannot change the names of an chMDB object")
}

###############################################################################@
#' Plot the underlying [ReDaMoR::RelDataModel]
#' 
#' @seealso [ReDaMoR::plot.RelDataModel]
#' 
#' @export
#'
plot.chMDB <- function(x){
   plot(dataModel(x))
}

###############################################################################@
#' @export
#'
filter.chMDB <- function(x, ...){
   dots <- enquos(...)
   filter_mdb(x, dots)
}

