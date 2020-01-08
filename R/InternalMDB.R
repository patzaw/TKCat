#' Create an internal database: a set of tables with a data model
#'
#' @param dataModel a RelDataModel object
#' @param dbTable a list of tables
#' @param dbInfo a list of information values regarding the DB
#' @param colMembers the collection members of the MDB. If NULL (default),
#' collection members are not defined.
#' @param checks a character vector with the name of optional checks to be
#' done (all of them c("unique", "not nullable", "foreign keys"))
#' @param verbose if TRUE display the data confrontation report
#'
#' @return An internalMDB object
#'
#' @export
#'
internalMDB <- function(
   dataModel,
   dbTables,
   dbInfo,
   colMembers=NULL,
   checks=c("unique", "not nullable", "foreign keys"),
   verbose=FALSE
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
   cr <- ReDaMoR::confront_data(
      dataModel, data=dbTables, checks=checks, verbose=verbose,
      retunData=FALSE
   )
   if(!cr$success){
      stop(ReDaMoR::format_confrontation_report(cr))
   }
   if(verbose){
      cat(format_confrontation_report(cr))
   }
   
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
#' Check the object is  an [internalMDB] object
#' 
#' @param x any object
#' 
#' @return A single logical: TRUE if x is an [internalMDB] object
#' 
#' @export
#'
is.internalMDB <- function(x){
   inherits(x, "internalMDB")
}

###############################################################################@
#' Get DB information of an [internalMDB] object
#' 
#' @param x an [internalMDB] object
#' 
#' @return A list with the following elements:
#' - **name**: a single character
#' - **title**: a single character
#' - **description**: a single character
#' - **url**: a single character
#' - **version**: a single character
#' 
#' @export
#'
dbInfo.internalMDB <- function(x){
   x <- unclass(x)
   x$"___dbInfo___"[
      which(names(x$"___dbInfo___")!="collectionMembers")
   ]
}

###############################################################################@
#' Get collection members of an [internalMDB] object
#' 
#' @param x an [internalMDB] object
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
collectionMembers.internalMDB <- function(
   x,
   collections=x$"___dbInfo___"$"collectionMembers"$collection
){
   x <- unclass(x)
   toRet <- x$"___dbInfo___"$"collectionMembers"
   toRet <- toRet[which(toRet$collection %in% collections),]
   return(toRet)
}

###############################################################################@
#' Set collection members of an [internalMDB] object
#' 
#' @param x an [internalMDB] object
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
#' @export
#'
'collectionMembers<-.internalMDB' <- function(x, value){
   if(is.null(value)){
      x <- unclass(x)
      x$"___dbInfo___"$"collectionMembers" <- value
      class(x) <- c("internalMDB", class(x))
      return(x)
   }
   
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
#' Get the [ReDaMoR::RelDataModel] of an [internalMDB] object
#' 
#' @param x an [internalMDB] object
#' 
#' @return A [ReDaMoR::RelDataModel] object
#' 
#' @export
#'
dataModel.internalMDB <- function(x){
   unclass(x)$"___dataModel___"
}

###############################################################################@
#' Get data tables from an [internalMDB] object
#' 
#' @param x an [internalMDB] object
#' @param ... the name of the tables to get (default: all of them)
#' 
#' @return A list of [tibble::tibble]
#' 
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
#' Format an [internalMDB] object for printing
#' 
#' @param x an [internalMDB] object
#' 
#' @return A single character
#' 
#' @export
#'
format.internalMDB <- function(x){
   sunits <- c("B", "KB", "MB", "GB", "TB")
   s <- as.numeric(object.size(x))
   hu <- log2(s)%/%10
   hs <- s/(2^(10*hu))
   u <- sunits[hu+1]
   cm <- collectionMembers(x)
   return(sprintf(
      paste(
         "internalMDB %s (version %s): %s",
         "   - %s tables",
         "   - %s records",
         "   - %s %s",
         "",
         "Collections: ",
         "%s",
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
#' Get the number of tables in an [internalMDB] object
#' 
#' @param x an [internalMDB] object
#' 
#' @return A single integer value
#' 
#' @export
#'
length.internalMDB <- function(x){
   length(dataModel(x))
}

###############################################################################@
#' @export
#'
'[.internalMDB' <- function(x, i=NULL){
   if(is.null(i)){
      return(x)
   }
   stopifnot(
      is.character(i),
      all(i %in% names(x))
   )
   dbi <- dbInfo(x)
   dbi$name <- sprintf("SUBSET of %s", dbi$name)
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
subset_internalMDB <- function(x, i){
   stopifnot(
      is.character(i),
      length(i)==1,
      all(i %in% names(x))
   )
   ## Rstudio hack to avoid DB call when just looking for names
   cc <- grep('.rs.getCompletionsDollar', deparse(sys.calls()), value=FALSE)
   if(length(cc)!=0){
      invisible(NULL)
   }else{
      return(unclass(x)[[i]])
   }
}
#' @export
'[[.internalMDB' <- subset_internalMDB
#' @export
'$.internalMDB' <- subset_internalMDB
rm(subset_internalMDB)

###############################################################################@
#' @export
#'
'[<-.internalMDB' <- function(x, i, value){
   stop("'[<-' is not supported for internalMDB")
}

###############################################################################@
#' @export
#'
'[[<-.internalMDB' <- function(x, i, value){
   stop("'[[<-' is not supported for internalMDB")
}

###############################################################################@
#' @export
#'
'$<-.internalMDB' <- function(x, i, value){
   stop("'$<-' is not supported for internalMDB")
}

###############################################################################@
#' @export
#'
c.internalMDB <- function(..., checkTables=FALSE){
   stop("'c' is not supported for internalMDB")
}


###############################################################################@
#' @export
#'
names.internalMDB <- function(x){
   setdiff(
      names(unclass(x)),
      c("___dataModel___", "___dbInfo___")
   )
}


###############################################################################@
#' @export
#'
'names<-.internalMDB' <- function(x, value){
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
#' Plot the underlying [ReDaMoR::RelDataModel]
#' 
#' @seealso [ReDaMoR::plot.RelDataModel]
#' 
#' @export
#'
plot.internalMDB <- function(x){
   plot(dataModel(x))
}
