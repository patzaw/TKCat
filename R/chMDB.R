###############################################################################@
#' An [MDB] (Modeled DataBase) relying on ClickHouse: chMDB
#'
#' @param tkcon a [chTKCat] object
#' @param dbTables a named vector of tables in tkcon$chcon with
#' `all(names(dbTables) %in% names(dataModel))`
#' @param dbInfo a list with DB information:
#' **"name"** (only mandatory field), "title", "description", "url",
#' "version", "maintainer".
#' @param dataModel a [ReDaMoR::RelDataModel] object
#' @param collectionMembers the members of collections as provided to the
#' [collection_members<-] function (default: NULL ==> no member).
#' @param n_max maximum number of records to read
#' for checks purpose (default: 10). See also [ReDaMoR::confront_data()].
#' @param verbose if TRUE display the data confrontation report
#'
#' @return A chMDB object
#' 
#' @seealso
#' - MDB methods:
#' [db_info], [data_model], [data_tables], [collection_members],
#' [count_records], [filter_with_tables], [as_fileMDB]
#' - Additional general documentation is related to [MDB].
#' - [filter.chMDB], [slice.chMDB]
#' - [chTKCat], [db_disconnect()], [db_reconnect()]
#'
#' @export
#'
chMDB <- function(
   tkcon,
   dbTables,
   dbInfo,
   dataModel,
   collectionMembers=NULL,
   n_max=10,
   verbose=FALSE
){
   
   ## chTKCat ----
   stopifnot(
      is.chTKCat(tkcon)
   )
   
   ## DB information ----
   dbInfo <- .check_dbInfo(dbInfo)
   
   ## Data model ----
   if(!ReDaMoR::is.RelDataModel(dataModel)){
      stop(
         "dataModel should be a RelDataModel object"
      )
   }
   
   ## DB tables ----
   stopifnot(
      all(names(dbTables) %in% names(dataModel)),
      all(names(dataModel) %in% names(dbTables))
   )
   dbTables_t <- strsplit(dbTables, split="[.]") %>% 
      lapply(function(x) gsub("`", "", x)) %>% 
      do.call(rbind, .) %>% 
      magrittr::set_colnames(c("database", "name")) %>% 
      dplyr::as_tibble()
   chTables <- list_tables(tkcon$chcon, dbNames=unique(dbTables_t$database))
   missTab <- dplyr::anti_join(dbTables_t, chTables, by=c("database", "name"))
   if(nrow(missTab)>0){
      stop(
         "The following tables are not in the clickhouse database: ",
         sprintf("`%s`.`%s`", missTab$database, missTab$name) %>% 
            paste(collapse=", ")
      )
   }
   
   ## Confront data to model ----
   query <- "SELECT * FROM %s"
   stopifnot(is.numeric(n_max), !is.na(n_max), length(n_max)==1)
   if(!is.infinite(n_max)){
      query <- paste(query, sprintf("LIMIT %s", n_max))
   }
   dataTables <- lapply(
      names(dbTables),
      function(tn){
         dbt <- dbTables[tn]
         query <- sprintf(query, dbt)
         toRet <- dplyr::as_tibble(DBI::dbGetQuery(tkcon$chcon, query))
         for(cn in colnames(toRet)){
            toRet[,cn] <- ReDaMoR::as_type(
               dplyr::pull(toRet, !!cn),
               dataModel[[tn]]$fields %>%
                  dplyr::filter(name==!!cn) %>%
                  dplyr::pull(type)
            )
         }
         return(toRet)
      }
   )
   names(dataTables) <- names(dbTables)
   cr <- ReDaMoR::confront_data(
      dataModel, data=dataTables, n_max=n_max, verbose=FALSE,
      returnData=FALSE
   )
   assign("confrontationReport", cr, envir=tkcatEnv)
   if(!cr$success){
      stop(ReDaMoR::format_confrontation_report(cr, title=dbInfo[["name"]]))
   }
   if(verbose){
      cat(ReDaMoR::format_confrontation_report(cr, title=dbInfo[["name"]]))
   }
   
   # ## Object ----
   toRet <- list(
      tkcon=tkcon,
      dbTables=dbTables[names(dataModel)],
      dataModel=dataModel,
      dbInfo=dbInfo
   )
   class(toRet) <- c("chMDB", "MDB", class(toRet))
   
   ## Collection members ----
   collection_members(toRet) <- collectionMembers
   
   return(toRet)
}


###############################################################################@
#' 
#' @rdname db_disconnect
#' @method db_disconnect chMDB
#' 
#' @export
#'
db_disconnect.chMDB <- function(x){
   db_disconnect(unclass(x)$tkcon)
   invisible()
}


###############################################################################@
#' 
#' @rdname db_reconnect
#' @method db_reconnect chMDB
#' 
#' @export
#'
db_reconnect.chMDB <- function(x, user, password, ntries=3){
   xn <- deparse(substitute(x))
   x <- unclass(x)
   tkcon <- x$tkcon
   db_reconnect(tkcon, user=user, password=password, ntries=ntries)
   nv <- x
   nv$tkcon <- tkcon
   class(nv) <- c("chMDB", "MDB", class(nv))
   assign(xn, nv, envir=parent.frame(n=1))
   invisible(nv)
}


###############################################################################@
#' Get a [chMDB] from a [chTKCat] 
#' 
#' @param tkcon a [chTKCat] object
#' @param dbName the name of the database
#' @param n_max maximum number of records to read
#' for checks purpose (default: 10). See also [ReDaMoR::confront_data()].
#' 
#' @export
#' 
get_chMDB <- function(tkcon, dbName, n_max=10){
   stopifnot(
      is.chTKCat(tkcon)
   )
   dbl <- list_chMDBs(tkcon)
   if(!is.data.frame(dbl) || !dbName %in% dbl$name){
      stop(sprintf("%s does not exist in the provided chTKCat", dbName))
   }
   
   ## Data model ----
   dbm <- list(
      tables=DBI::dbGetQuery(
         tkcon$chcon, sprintf("SELECT * FROM `%s`.`___Tables___`", dbName)
      ),
      fields=DBI::dbGetQuery(
         tkcon$chcon, sprintf("SELECT * FROM `%s`.`___Fields___`", dbName)
      ),
      primaryKeys=DBI::dbGetQuery(
         tkcon$chcon,
         sprintf("SELECT * FROM `%s`.`___PrimaryKeys___`", dbName)
      ),
      foreignKeys=DBI::dbGetQuery(
         tkcon$chcon,
         sprintf("SELECT * FROM `%s`.`___ForeignKeys___`", dbName)
      ),
      indexes=DBI::dbGetQuery(
         tkcon$chcon, sprintf("SELECT * FROM `%s`.`___Indexes___`", dbName)
      )
   )
   dbm$fields$nullable <- as.logical(dbm$fields$nullable)
   dbm$fields$unique <- as.logical(dbm$fields$unique)
   dbm$indexes$unique <- as.logical(dbm$indexes$unique)
   dataModel <- ReDaMoR::fromDBM(dbm)
   
   ## DB information ----
   dbInfo <- as.list(DBI::dbGetQuery(
      tkcon$chcon, sprintf("SELECT * FROM `%s`.`___MDB___`", dbName)
   ))
   
   ## DB tables ----
   dbTables <- sprintf(
      "`%s`.`%s`", dbInfo$name, names(dataModel)
   )
   names(dbTables) <- names(dataModel)
   
   ## Collection members ----
   collectionMembers <- DBI::dbGetQuery(
      conn=tkcon$chcon,
      statement=sprintf(
         "SELECT * FROM `%s`.`___CollectionMembers___`",
         dbName
      )
   ) %>%
      as_tibble() %>%
      mutate(
         resource=dbName,
         static=as.logical(static)
      ) %>% 
      select(collection, cid, resource, mid, table, field, static, value, type)
   attr(collectionMembers, "data.type") <- NULL
   
   return(chMDB(
      tkcon=tkcon,
      dbTables=dbTables,
      dbInfo=dbInfo,
      dataModel=dataModel,
      collectionMembers=collectionMembers,
      n_max=n_max,
      verbose=TRUE
   ))
}


###############################################################################@
#' Check if the object is a [chMDB] object
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
#' 
#' @param x a [chMDB] object
#' @param value new table names
#' 
#' @rdname chMDB
#' 
#' @export
#'
'names<-.chMDB' <- function(x, value){
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
   names(x$dataModel) <- names(x$dbTables) <-value
   class(x) <- c("chMDB", class(x))
   collection_members(x) <- ncolMb
   return(x)
}


###############################################################################@
#' Rename tables of a [chMDB] object
#'
#' @param .data a [chMDB] object
#' @param ... Use new_name = old_name to rename selected tables
#' 
#' @export
#' 
rename.chMDB <- function(.data, ...){
   loc <- tidyselect::eval_rename(expr(c(...)), .data)
   names <- names(.data)
   names[loc] <- names(loc)
   set_names(.data, names)
}


###############################################################################@
#' 
#' @rdname db_info
#' @method db_info chMDB
#' 
#' @export
#'
db_info.chMDB <- function(x, ...){
   return(unclass(x)$dbInfo)
}


###############################################################################@
#' 
#' @rdname db_info
#' @method db_info<- chMDB
#' 
#' @export
#'
'db_info<-.chMDB' <- function(x, value){
   toRet <- unclass(x)
   dbInfo <- .check_dbInfo(value)
   toRet$dbInfo <- dbInfo
   if(!is.null(toRet$collectionMembers)){
      toRet$collectionMembers$resource <- dbInfo$name
   }
   class(toRet) <- c("chMDB", "MDB", class(toRet))
   return(toRet)
}


###############################################################################@
#' 
#' @rdname data_model
#' @method data_model chMDB
#' 
#' @export
#'
data_model.chMDB <- function(x, ...){
   unclass(x)$dataModel
}


###############################################################################@
#' 
#' @rdname collection_members
#' @method collection_members chMDB
#' 
#' @export
#'
collection_members.chMDB <- function(
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
#'
#' @rdname collection_members
#' @method collection_members<- chMDB
#' 
#' @export
#'
'collection_members<-.chMDB' <- function(x, value){
   
   if(is.null(value)){
      x <- unclass(x)
      x$"collectionMembers" <- value
      class(x) <- c("chMDB", "MDB", class(x))
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
         dplyr::select(value, "collection", "mid", "table", "field")
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
   class(x) <- c("chMDB", "MDB", class(x))
   return(x)
}


###############################################################################@
#' 
#' @rdname data_tables
#' @method data_tables chMDB
#' 
#' @export
#'
data_tables.chMDB <- function(x, ...){
   m <- data_model(x)
   toTake <- tidyselect::eval_select(expr(c(...)), x)
   if(length(toTake)==0){
      toTake <- 1:length(x)
      names(toTake) <- names(x)
   }
   toTake <- x$dbTables[toTake]
   dbt <- db_tables(x)
   toRet <- lapply(
      toTake,
      function(y){
         DBI::dbGetQuery(
            dbt$tkcon$chcon,
            sprintf("SELECT * from %s", y)
         )
      }
   )
   return(toRet)
}


###############################################################################@
#' 
#' @rdname count_records
#' @method count_records chMDB
#' 
#' @export
#'
count_records.chMDB <- function(x, ...){
   toTake <- tidyselect::eval_select(expr(c(...)), x)
   if(length(toTake)==0){
      toTake <- 1:length(x)
      names(toTake) <- names(x)
   }
   dbt <- db_tables(x)
   dbt$dbTables <- dbt$dbTables[toTake]
   dbTables_t <- strsplit(dbt$dbTables, split="[.]") %>% 
      lapply(function(x) gsub("`", "", x)) %>% 
      do.call(rbind, .) %>% 
      magrittr::set_colnames(c("database", "name")) %>% 
      dplyr::as_tibble()
   chTables <- list_tables(
      dbt$tkcon$chcon, dbNames=unique(dbTables_t$database)
   )
   dbTables_t <- dplyr::left_join(
      dbTables_t, chTables, by=c("database", "name")
   )
   toRet <- dbTables_t$total_rows
   names(toRet) <- names(toTake)
   return(toRet)
}


###############################################################################@
#' Get the DB tables from a [chMDB] object
#' 
#' @param x a [chMDB] object
#' 
#' @return a list with a chTKCat object (tkcon) and
#' a named vector of DB table names (dbTables).
#' 
#' @export
#'
db_tables <- function(x){
   stopifnot(is.chMDB(x))
   x <- unclass(x)
   toTake <- c("tkcon", "dbTables")
   stopifnot(all(toTake %in% names(x)))
   return(x[toTake])
}



































