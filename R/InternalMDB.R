#' Create an internal database: a set of tables with a data model
#'
#' @param dataModel a RelDataModel object
#' @param dbTable a list of tables
#' @param dbInfo a list of information values regarding the DB
#' @param colMembers the collection members of the MDB. If NULL (default),
#' collection members are not defined.
#' @param checkTables a single logical. If TRUE (default), tables are confronted
#' to the dataModel.
#'
#' @return An internalMDB object
#'
#' @export
#'
internalMDB <- function(
   dataModel, dbTables, dbInfo,
   colMembers=NULL,
   checkTables=TRUE
){

   ## Checks ----
   stopifnot(
      is.RelDataModel(dataModel),
      is.list(dbInfo),
      is.character(dbInfo$name), length(dbInfo$name)==1,
      is.character(dbInfo$title), length(dbInfo$title)==1,
      is.character(dbInfo$description), length(dbInfo$description)==1,
      is.character(dbInfo$url), length(dbInfo$url)==1,
      is.character(dbInfo$version), length(dbInfo$version)==1
   )
   if("___dataModel___" %in% names(dataModel)){
      stop('"___dataModel___" is reserved and cannot be used as a table name')
   }
   if("___dbInfo___" %in% names(dataModel)){
      stop('"___dbInfo___" is reserved and cannot be used as a table name')
   }
   lapply(
      names(dataModel),
      function(x){
         if(!x %in% names(dbTables)){
            stop(sprintf(
               "The %s table is not in dbTables",
               x
            ))
         }
         dbt <- dbTables[[x]]
         if(checkTables){
            checkTable(dbt, dataModel[[x]])
         }
      }
   )
   supTables <- setdiff(
      names(dbTables),
      names(dataModel)
   )
   if(length(supTables)>0){
      stop(
         "The following tables are not described in the data model: ",
         paste(supTables, collapse=", ")
      )
   }


   ## Creating the DB ----
   # toRet <- new.env(hash=TRUE, parent=emptyenv())
   # assign("___dataModel___", dataModel, toRet)
   # for(dn in names(dbTables)){
   #    assign(dn, dbTables[[dn]], toRet)
   # }
   
   toRet <- dbTables[names(dataModel)]
   toRet$"___dataModel___" <- dataModel
   toRet$"___dbInfo___" <- dbInfo
   class(toRet) <- c("internalMDB", class(toRet))
   
   ## Collection members ----
   if(!is.null(colMembers)){
      collectionMembers(toRet) <- colMembers
   }

   ## Returning the DB ----
   return(toRet)

}

###############################################################################@
#' @export
#'
is.internalMDB <- function(x){
   inherits(x, "internalMDB")
}

###############################################################################@
#' Check table according to a data model
#'
#' @param x a data.frame
#' @param tableModel a \code{\link{RelTableModel}} object
#'
#' @export
#'
checkTable <- function(x, tableModel){
   stopifnot(
      is.data.frame(x),
      is.RelTableModel(tableModel)
   )

   ## Missing and supplementary fields ----
   tft <- tableModel$fields
   tf <- tft$name
   misFields <- setdiff(tf, colnames(x))
   if(length(misFields)>0){
      stop(
         sprintf(
            "The following fields were not found in the %s table: ",
            tn
         ),
         paste(misFields, collapse=", ")
      )
   }
   supFields <- setdiff(colnames(x), tf)
   if(length(supFields)>0){
      stop(
         sprintf(
            "The following fields are not defined for the %s table",
            tn
         ),
         " and won't be taken into account: ",
         paste(misFields, collapse=", ")
      )
   }

   ## Field requirements ----
   it <- indexTable(tableModel)
   uq <- c()
   if(!is.null(it)){
      uq <- unique(c(uq, unique(it$field[which(it$unique)])))
   }
   nullable <- tft$name[which(tft$nullable)]
   for(f in tf){
      ft <- tft %>% slice(match(f, name)) %>% pull(type)
      if(!is(x %>% pull(get(f)), ft)){
         stop(sprintf(
            paste(
               "The %s field of the %s table is not of type %s",
               "as defined is the model"
            ),
            f, tableModel$tableName, ft
         ))
      }
      if(f %in% nullable){
         nv <- sum(is.na(x %>% pull(get(f))))
         if(nv>0){
            nvp <- round(nv*100/nrow(x))
            warning(sprintf(
               paste(
                  "There are %s (%s%s) missing values ",
                  "in the %s field of the %s table"
               ),
               nv, nvp, "%", f,
               tableModel$tableName
            ))
         }
      }else{
         if(any(is.na(x %>% pull(get(f))))){
            stop(sprintf(
               "The %s field of the %s table is not nullable",
               f, tableModel$tableName
            ))
         }
      }
      if(f %in% uq){
         dv <- sum(duplicated(x %>% filter(!is.na(get(f))) %>% pull(get(f))))
         if(dv > 0){
            stop(sprintf(
               "The %s field of the %s table must contain unique values",
               f, tableModel$tableName
            ))
         }
      }
   }

}

###############################################################################@
#' @export
#'
dbInfo.internalMDB <- function(x, ...){
   x$"___dbInfo___"[which(names(x$"___dbInfo___")!="collectionMembers")]
}

###############################################################################@
#' @export
#'
collectionMembers.internalMDB <- function(
   x,
   collections=x$"___dbInfo___"$"collectionMembers"$collection,
   ...
){
   toRet <- x$"___dbInfo___"$"collectionMembers"
   toRet <- toRet[which(toRet$collection %in% collections),]
   return(toRet)
}

###############################################################################@
#' @export
#'
'collectionMembers<-.internalMDB' <- function(x, value, ...){
   if(is.null(value)){
      x <- unclass(x)
      x$"___dbInfo___"$"collectionMembers" <- value
      class(x) <- c("internalMDB", class(x))
      return(x)
   }
   
   stopifnot(
      is.data.frame(value),
      is.character(value$collection),
      is.character(value$resource),
      is.character(value$table),
      is.character(value$field),
      is.logical(value$static),
      is.character(value$value),
      all(value$resource==dbInfo(x)$name),
      all(value$collection %in% listLocalCollections()$title),
      sum(duplicated(value %>% select(collection, table, field)))==0
   )
   for(mbt in unique(value$table)){
      notFound <- setdiff(
         value$value[which(value$table==mbt & !value$static)],
         dataModel(x)[[mbt]]$fields$name
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
   x <- unclass(x)
   x$"___dbInfo___"$"collectionMembers" <- value
   class(x) <- c("internalMDB", class(x))
   return(x)
}

###############################################################################@
#' @export
#'
dataModel.internalMDB <- function(x, ...){
   x$"___dataModel___"
}

###############################################################################@
#' @export
#'
dataTables.internalMDB <- function(x, ...){
   m <- dataModel(x)
   x <- unclass(x)
   toTake <- unlist(list(...))
   if(length(toTake)>0){
      notInDb <- setdiff(toTake, names(m))
      if(length(notInDb)>0){
         stop(
            "The following tables are not in the InternalDB: ",
            paste(notInDb, sep=", ")
         )
      }
   }else{
      toTake <- names(m)
   }
   x[toTake]
}

###############################################################################@
#' @export
#'
format.internalMDB <- function(x){
   sunits <- c("B", "KB", "MB", "GB", "TB")
   s <- as.numeric(object.size(x))
   hu <- log2(s)%/%10
   hs <- s/(2^(10*hu))
   u <- sunits[hu+1]
   return(sprintf(
      paste(
         "internalMDB %s (version %s): %s",
         "   - %s tables",
         "   - %s records",
         "   - %s %s",
         "",
         "%s (%s)",
         sep="\n"
      ),
      dbInfo(x)$name,
      dbInfo(x)$version,
      dbInfo(x)$title,
      length(x),
      format(
         sum(unlist(lapply(dataTables(x), nrow))),
         big.mark=",",
         scientific=FALSE
      ),
      round(hs, 1), u,
      dbInfo(x)$description,
      dbInfo(x)$url
   ))
}

###############################################################################@
#' @export
#'
print.internalMDB <- function(x, ...){
   cat(format(x, ...), "\n")
}

###############################################################################@
#' @export
#'
length.internalMDB <- function(x){
   length(dataModel(x))
}

###############################################################################@
#' @export
#'
'[<-.internalMDB' <- function(x, i, value){
   stop("'[<-' is not supported for internalMDB: use 'c' instead")
}

###############################################################################@
#' @export
#'
'[[<-.internalMDB' <- function(x, i, value){
   stop("'[[<-' is not supported for internalMDB: use 'c' instead")
}

###############################################################################@
#' @export
#'
'$<-.internalMDB' <- function(x, i, value){
   stop("'$<-' is not supported for internalMDB: use 'c' instead")
}

###############################################################################@
#' @export
#'
c.internalMDB <- function(..., checkTables=FALSE){
   dm <- do.call(c, lapply(list(...), dataModel))
   d <- do.call(c, lapply(list(...), dataTables))
   return(internalMDB(dm, d, checkTables=checkTables))
}


###############################################################################@
#' @export
#'
names.internalMDB <- function(x, ...){
   setdiff(
      names(unclass(x)),
      c("___dataModel___", "___dbInfo___")
   )
}


###############################################################################@
#' @export
#'
'names<-.internalMDB' <- function(x, value, ...){
   colMb <- collectionMembers(x)
   ovalues <- names(x)
   x <- unclass(x)
   if(!is.null(colMb)){
      ncolMb <- NULL
      for(i in 1:length(ovalues)){
         toAdd <- colMb %>% filter(table==ovalues[i])
         if(nrow(toAdd)>0){
            toAdd$table <- value[i]
            ncolMb <- ncolMb %>% bind_rows(toAdd)
         }
      }
   }
   names(x$"___dataModel___") <- value
   names(x)[which(!names(x) %in% c("___dataModel___", "___dbInfo___"))] <-
      value
   class(x) <- c("internalMDB", class(x))
   collectionMembers(x) <- ncolMb
   return(x)
}

###############################################################################@
#' @export
#'
plot.internalMDB <- function(x, ...){
   plot(dataModel(x))
}
