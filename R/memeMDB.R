###############################################################################@
#' Create an MDB (Modeled DataBase) in memory: memeMDB
#'
#' @param dataTables a list of tibbles
#' @param dataModel a [ReDaMoR::RelDataModel] object
#' @param dbInfo a list with DB information:
#' "name", "title", "description", "url",
#' "version", "maintainer".
#' @param collectionMembers the members of collections as provided to the
#' [collection_members<-] function (default: NULL ==> no member).
#' @param checks a character vector with the name of optional checks to be
#' done (all of them c("unique", "not nullable", "foreign keys"))
#' @param verbose if TRUE display the data confrontation report
#' (default: FALSE)
#'
#' @return A memeMDB object
#'
#' @export
#'
memeMDB <- function(
   dataTables,
   dataModel,
   dbInfo,
   collectionMembers=NULL,
   checks=c("unique", "not nullable", "foreign keys"),
   verbose=FALSE
){
   ## DB information ----
   dbInfo <- .check_dbInfo(dbInfo)
   
   ## Confront data tables to the model ----
   cr <- ReDaMoR::confront_data(
      dataModel, data=dataTables, checks=checks, verbose=FALSE,
      returnData=FALSE
   )
   assign("confrontationReport", cr, envir=tkcatEnv)
   if(!cr$success){
      stop(ReDaMoR::format_confrontation_report(cr, title=dbInfo[["name"]]))
   }
   if(verbose){
      cat(ReDaMoR::format_confrontation_report(cr, title=dbInfo[["name"]]))
   }
   
   ## Object ----
   toRet <- list(
      dataTables=dataTables,
      dataModel=dataModel,
      dbInfo=dbInfo
   )
   class(toRet) <- c("memeMDB", "MDB", class(toRet))
   
   ## Collection members ----
   collection_members(toRet) <- collectionMembers
   
   return(toRet)
}


###############################################################################@
#' Check the object is  a [memeMDB] object
#' 
#' @param x any object
#' 
#' @return A single logical: TRUE if x is an [memeMDB] object
#' 
#' @export
#'
is.memeMDB <- function(x){
   inherits(x, "memeMDB")
}


###############################################################################@
#' Convert  any MDB object in a [memeMDB] object
#' 
#' @param x a MDB object
#' @param ... additional parameters for the [memeMDB()] function.
#' 
#' @return A [memeMDB] object
#' 
#' @export
#'
as_memeMDB <- function(x, ...){
   stopifnot(is.MDB(x))
   return(memeMDB(
      dataTables=data_tables(x),
      dataModel=data_model(x),
      dbInfo=db_info(x),
      collectionMembers=collection_members(x),
      ...
   ))
}


###############################################################################@
#' @export
#'
'names<-.memeMDB' <- function(x, value){
   colMb <- collection_members(x)
   ovalues <- names(x)
   x <- unclass(x)
   ncolMb <- NULL
   if(!is.null(colMb)){
      ncolMb <- NULL
      for(i in 1:length(ovalues)){
         toAdd <- colMb %>% dplyr::filter(.data$table==!!ovalues[i])
         if(nrow(toAdd)>0){
            toAdd$table <- value[i]
            ncolMb <- ncolMb %>% dplyr::bind_rows(toAdd)
         }
      }
   }
   names(x$dataModel) <- names(x$dataTables) <-value
   class(x) <- c("memeMDB", class(x))
   collection_members(x) <- ncolMb
   return(x)
}


###############################################################################@
#' @export
#' 
rename.memeMDB <- function(.data, ...){
   loc <- tidyselect::eval_rename(expr(c(...)), .data)
   names <- names(.data)
   names[loc] <- names(loc)
   set_names(.data, names)
}


###############################################################################@
#' @param x a [memeMDB] object
#' @param ... not used
#' 
#' @rdname db_info
#' @method db_info memeMDB
#' 
#' @export
#'
db_info.memeMDB <- function(x, ...){
   y <- unclass(x)
   toRet <- y$dbInfo
   return(toRet)
}

###############################################################################@
#' @param x a [memeMDB] object
#' @param value a list with DB information:
#' "name", "title", "description", "url",
#' "version", "maintainer".
#' 
#' @rdname db_info-set
#' @method db_info<- memeMDB
#' 
#' @export
#'
'db_info<-.memeMDB' <- function(x, value){
   toRet <- unclass(x)
   dbInfo <- .check_dbInfo(value)
   toRet$dbInfo <- dbInfo
   if(!is.null(toRet$collectionMembers)){
      toRet$collectionMembers$resource <- dbInfo$name
   }
   class(toRet) <- c("memeMDB", "MDB", class(toRet))
   return(toRet)
}


###############################################################################@
#' @param x a [memeMDB] object
#' @param ... not used
#' 
#' @rdname data_model
#' @method data_model memeMDB
#' 
#' @export
#'
data_model.memeMDB <- function(x, ...){
   unclass(x)$dataModel
}


###############################################################################@
#' @param x a [memeMDB] object
#' @param ... names of the collections
#' to focus on. By default, all of them are taken.
#' 
#' @rdname collection_members
#' @method collection_members memeMDB
#' 
#' @export
#'
collection_members.memeMDB <- function(
   x,
   ...
){
   x <- unclass(x)
   toRet <- x$"collectionMembers"
   toTake <- unlist(list(...))
   if(length(toTake)>0){
      stopifnot(is.character(toTake))
      toRet <- toRet[which(toRet$collection %in% toTake),]
   }
   return(toRet)
}


###############################################################################@
#' @param x a [memeMDB] object
#' 
#' @param value A data.frame with the following columns:
#' - **collection** (character): The name of the collection
#' - **cid** (character): Collection identifier
#' - **resource** (character): The name of the resource
#' - **mid** (integer): The identifier of the member
#' - **table** (character): The table recording collection information
#' - **field** (character): The collection field.
#' - **static** (logical): TRUE if the field value is common to all elements.
#' - **value** (character): The name of the table column if static is FALSE
#' or the field value if static is TRUE.
#' - **type** (character): the type of the field.
#' (not necessarily used ==> NA if not)
#' 
#' @rdname collection_members-set
#' @method collection_members<- memeMDB
#' 
#' @export
#'
'collection_members<-.memeMDB' <- function(x, value){
   
   if(is.null(value)){
      x <- unclass(x)
      x$"collectionMembers" <- value
      class(x) <- c("memeMDB", "MDB", class(x))
      return(x)
   }
   
   stopifnot(
      is.data.frame(value),
      all(
         colnames(value) %in%
            c(
               "collection", "cid", "resource", "mid", "table",
               "field", "static", "value", "type"
            )
      ),
      is.character(value$collection),
      is.character(value$cid),
      is.character(value$resource),
      is.integer(value$mid),
      is.character(value$table),
      is.character(value$field),
      is.logical(value$static),
      is.character(value$value),
      is.character(value$type),
      all(value$resource==db_info(x)$name),
      all(value$collection %in% list_local_collections()$title),
      sum(duplicated(
         value %>% dplyr::select("collection", "table", "field")
      ))==0
   )
   
   for(collection in unique(value$collection)){
      cv <- dplyr::filter(value, .data$collection==!!collection)
      for(cid in unique(cv$cid)){
         jval <- dplyr::filter(cv, .data$cid==!!cid) %>%
            write_collection_members()
      }
   }
   
   for(mbt in unique(value$table)){
      notFound <- setdiff(
         value$value[which(value$table==mbt & !value$static)],
         data_model(x)[[mbt]]$fields$name
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
   x$"collectionMembers" <- value
   class(x) <- c("memeMDB", "MDB", class(x))
   return(x)
}


###############################################################################@
#' @param x a [memeMDB] object
#' @param ... the name of the tables to get (default: all of them)
#' 
#' @rdname data_tables
#' @method data_tables memeMDB
#' 
#' @export
#'
data_tables.memeMDB <- function(x, ...){
   m <- data_model(x)
   toTake <- tidyselect::eval_select(expr(c(...)), x)
   if(length(toTake)==0){
      toTake <- 1:length(x)
      names(toTake) <- names(x)
   }
   x <- unclass(x)
   toRet <- x$dataTables[toTake]
   names(toRet) <- names(toTake)
   return(toRet)
}


###############################################################################@
#' @param x a [memeMDB]
#' @param ... the name of the tables to consider (default: all of them)
#' 
#' @rdname count_records
#' @method count_records memeMDB
#' 
#' @export
#'
count_records.memeMDB <- function(x, ...){
   lapply(data_tables(x, ...), nrow) %>% unlist()
}


###############################################################################@
#' @export
#'
'[.memeMDB' <- function(x, i){
   if(missing(i)){
      return(x)
   }
   if(length(i)==0){
      dbi <- db_info(x)
      dbi$name <- sprintf("EMPTY %s", dbi$name)
      return(memeMDB(
         dataTables=list(),
         dataModel=RelDataModel(l=list()),
         dbInfo=dbi
      ))
   }
   stopifnot(
      is.character(i) || is.numeric(i),
      all(!is.na(i))
   )
   if(is.numeric(i)){
      stopifnot(all(i %in% 1:length(x)))
      i <- names(x)[i]
   }
   if(is.character(i)){
      stopifnot(all(i %in% names(x)))
   }
   dbi <- db_info(x)
   dbi$name <- sprintf("SUBSET of %s", dbi$name)
   dm <- data_model(x)[i, rmForeignKeys=TRUE]
   dt <- data_tables(x)[i]
   cm <- collection_members(x) %>%
      dplyr::filter(.data$table %in% i) %>%
      dplyr::mutate(resource=dbi$name)
   toRet <- memeMDB(
      dataTables=dt,
      dataModel=dm,
      dbInfo=dbi,
      collectionMembers=cm
   )
   return(toRet)
}


###############################################################################@
#' @export
#'
'[[.memeMDB' <- function(x, i){
   stopifnot(
      length(i)==1
   )
   return(data_tables(x, i)[[1]])
}
#' @export
'$.memeMDB' <- `[[.memeMDB`


###############################################################################@
#' @export
#'
c.memeMDB <- function(...){
   alldb <- list(...)
   if(length(alldb)==0){
      stop("At least one memeMDB should be provided as an input")
   }
   dt <- data_tables(alldb[[1]])
   for(i in 1:length(alldb)){
      if(!is.memeMDB(alldb[[i]])){
         stop("All objects should be memeMDB")
      }
   }
   dbi <- db_info(alldb[[1]])
   dm <- data_model(alldb[[1]])
   cm <- collection_members(alldb[[1]])
   if(length(alldb)>1) for(i in 2:length(alldb)){
      dm <- c(dm, data_model(alldb[[i]]))
      dt <- c(dt, data_tables(alldb[[i]]))
      cm <- dplyr::bind_rows(
         cm,
         collection_members(alldb[[i]]) %>%
            dplyr::mutate(resource=dbi$name)
      )
   }
   memeMDB(
      dataTables=dt,
      dataModel=dm,
      dbInfo=dbi,
      collectionMembers=cm
   )
}


###############################################################################@
#' @param x a [memeMDB]
#' @param path the path where the MDB should be written
#' @param ... not used
#' 
#' @rdname write_MDB
#' @method write_MDB memeMDB
#' 
#' @export
#'
write_MDB.memeMDB <- function(x, path, ...){
   stopifnot(is.character(path), length(path)==1, !is.na(path))
   dbInfo <- db_info(x)
   dbName <- dbInfo$name
   
   ## Initialization ----
   fullPath <- file.path(path, dbName)
   if(file.exists(fullPath)){
      stop(sprintf("%s already exists", fullPath))
   }
   dir.create(fullPath, recursive=TRUE)
   
   ## Description file ----
   rp <- list(
      delim='\t',
      quoted_na=FALSE
   )
   descFile <- file.path(fullPath, "DESCRIPTION.json")
   .writeDescription(c(dbInfo, rp), descFile)
   
   ## Data model ----
   dm <- data_model(x)
   modelPath <- file.path(fullPath, "model")
   dir.create(modelPath)
   jModelPath <- file.path(modelPath, paste0(dbName, ".json"))
   hModelPath <- file.path(modelPath, paste0(dbName, ".html"))
   write_json_data_model(dm, jModelPath)
   plot(dm) %>%
      visNetwork::visSave(hModelPath)
   
   ## Collection members ----
   cm <- collection_members(x)
   if(nrow(cm)>0){
      colPath <- file.path(modelPath, "Collections")
      dir.create(colPath)
      for(collection in unique(cm$collection)){
         cv <- dplyr::filter(cm, .data$collection==!!collection)
         for(cid in unique(cv$cid)){
            dplyr::filter(cv, .data$cid==!!cid) %>%
               write_collection_members(path=file.path(
                  colPath,
                  paste0(collection, "-", cid, ".json")
               ))
         }
      }
   }
   
   ## Data ----
   dataPath <- file.path(fullPath, "data")
   dir.create(dataPath)
   ext <- ".txt.gz"
   dfiles <- file.path(dataPath, paste0(names(x), ext))
   names(dfiles) <- names(x)
   for(tn in names(x)){
      readr::write_tsv(x[[tn]], path=dfiles[tn])
   }
   
   ## Return fileMDB ----
   return(fileMDB(
      dataFiles=dfiles,
      dbInfo=dbInfo,
      dataModel=dm,
      readParameters=rp,
      collectionMembers=cm
   ))
}






