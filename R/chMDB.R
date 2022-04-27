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
#' @param check logical: if TRUE (default) the data are confronted to the
#' data model
#' @param n_max maximum number of records to read
#' for checks purpose (default: 10). If 0, the data are not checked.
#' See also [ReDaMoR::confront_data()].
#' @param verbose if TRUE display the data confrontation report
#'
#' @return A chMDB object
#' 
#' @seealso
#' - MDB methods:
#' [db_info], [data_model], [data_tables], [collection_members],
#' [count_records], [dims], [filter_with_tables], [as_fileMDB]
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
   check=TRUE,
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
   if(check){
      if(length(dbTables)>0){
         dbTables_t <- do.call(rbind, lapply(
            strsplit(dbTables, split="`[.]`"),
            function(x) gsub("`", "", x)
         )) %>%
            magrittr::set_colnames(c("database", "name")) %>% 
            dplyr::as_tibble()
         chTables <- list_tables(
            tkcon$chcon,
            dbNames=unique(dbTables_t$database)
         )
         missTab <- dplyr::anti_join(
            dbTables_t, chTables, by=c("database", "name")
         )
         if(nrow(missTab)>0){
            stop(
               "The following tables are not in the clickhouse database: ",
               sprintf("`%s`.`%s`", missTab$database, missTab$name) %>% 
                  paste(collapse=", ")
            )
         }
      }
   }
   
   ## Confront data to model ----
   if(check){
      stopifnot(is.numeric(n_max), !is.na(n_max), length(n_max)==1)
      if(n_max>0){
         tmpk <- list(
            tkcon=tkcon,
            dbTables=dbTables[names(dataModel)],
            dataModel=dataModel,
            dbInfo=dbInfo
         )
         class(tmpk) <- c("chMDB", "MDB", class(tmpk))
         dataTables <- heads(tmpk, n=n_max)
         names(dataTables) <- names(dbTables)
         cr <- ReDaMoR::confront_data(
            dataModel, data=dataTables, n_max=n_max, verbose=FALSE,
            returnData=FALSE
         )
         assign("confrontationReport", cr, envir=tkcatEnv)
         if(!cr$success){
            cat(ReDaMoR::format_confrontation_report(
               cr, title=dbInfo[["name"]]
            ))
            stop("Data do not fit the data model")
         }
         if(verbose){
            cat(ReDaMoR::format_confrontation_report(
               cr, title=dbInfo[["name"]]
            ))
         }
      }
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
db_reconnect.chMDB <- function(x, user, password, ntries=3, ...){
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
#' 
#' @rdname get_hosts
#' @method get_hosts chMDB
#' 
#' @export
#'
get_hosts.chMDB <- function(x, ...){
   get_hosts(unclass(x)$tkcon)
}


###############################################################################@
#' 
#' @param autoalias Change this parameter only if you know what you're doing.
#' if TRUE, make relevant alias to query the chMDB
#' using the table names from the data model. If FALSE, the user must know the
#' table instance name in the remote database. By default, autoalias is set
#' to TRUE when using a non-current instance of the database.
#' @param ... Additional parameters for [dbGetQuery()] function.
#' For the ClickHouseHTTP DBI, `format` can be set to "Arrow" (default) or
#' "TabSeparatedWithNamesAndTypes"
#' (see [ClickHouseHTTP::dbSendQuery,ClickHouseHTTPConnection,character-method])
#' 
#' @rdname get_query
#' @method get_query chMDB
#' 
#' @export
#'
get_query.chMDB <- function(x, query, autoalias=!is_current_chMDB(x), ...){
   con <- unclass(x)$tkcon$chcon
   n <- unclass(x)$dbInfo$name
   DBI::dbSendQuery(con, sprintf("USE `%s`", n))
   on.exit(DBI::dbSendQuery(con, "USE default"))
   if(is.na(autoalias)){
      autoalias <- FALSE
   }
   if(autoalias){
      dbt <- db_tables(x)$dbTables
      alias <- paste(
         "WITH",
         paste(
            sprintf(
               '`%s` AS (SELECT * FROM %s)',
               names(dbt), dbt
            ),
            collapse=", "
         ),
         sep=" "
      )
      query <- paste(alias, query, sep=" ")
   }
   DBI::dbGetQuery(con, query, ...) %>%
      dplyr::as_tibble()
}


###############################################################################@
#'
#' @param timestamp the timestamp of the instance to get.
#' Default=NA: get the current version.
#' @param check logical: if TRUE (default) the data are confronted to the
#' data model
#' @param n_max maximum number of records to read
#' for checks purpose (default: 10). See also [ReDaMoR::confront_data()].
#' 
#' @rdname get_MDB
#' @method get_MDB chTKCat
#' 
#' @export
#'
get_MDB.chTKCat <- function(x, dbName, timestamp=NA, check=TRUE, n_max=10, ...){
   timestamp <- as.POSIXct(timestamp)
   stopifnot(
      is.chTKCat(x),
      is.na(timestamp) || inherits(timestamp, "POSIXct"), length(timestamp)==1
   )
   dbl <- list_MDBs(x)
   if(!is.data.frame(dbl) || !dbName %in% dbl$name){
      stop(sprintf(
         "%s does not exist in the provided chTKCat",
         dbName
      ))
   }
   if(dplyr::filter(dbl, .data$name==!!dbName)$access=="none"){
      stop(sprintf("You don't have permission to access %s", dbName))
   }
   
   tst <- list_chMDB_timestamps(x, dbName)
   if(is.null(tst) || nrow(tst)==0){
      tstToComplete <- TRUE
      if(!is.na(timestamp)){
         stop("This MDB is not timestamped: timestamp must be NA")
      }else{
         if(!dplyr::filter(dbl, .data$name==!!dbName)$populated){
            stop(sprintf("The %s is not populated yet", dbName))
         }
         tst <- dplyr::tibble(
            table=setdiff(names(CHMDB_DATA_MODEL), MGT_TABLES)
         ) %>% 
            dplyr::mutate(
               instance=.data$table
            )
      }
   }else{
      tstToComplete <- FALSE
      if(is.na(timestamp)){
         if(is.na(attr(tst, "current"))){
            stop("There is no current instance of the MDB: provide a timestamp")
         }else{
            timestamp <- attr(tst, "current")
         }
      }
      if(!timestamp %in% tst$timestamp){
         stop("The selected timestamp does not exist")
      }
      tst <- tst %>% 
         dplyr::filter(.data$timestamp==!!timestamp)
   }
   
   ## Data model ----
   tsSelect <- function(table){
      sprintf(
         "SELECT * FROM `%s`.`%s`",
         dbName,
         tst$instance[which(tst$table==table)]
      )
   }
   dbm <- list(
      tables=get_query(
         x, tsSelect("___Tables___"),
         format="Arrow"
      ),
      fields=get_query(
         x, tsSelect("___Fields___"),
         format="Arrow"
      ),
      primaryKeys=get_query(
         x, tsSelect("___PrimaryKeys___"),
         format="TabSeparatedWithNamesAndTypes"
      ),
      foreignKeys=get_query(
         x, tsSelect("___ForeignKeys___"),
         format="TabSeparatedWithNamesAndTypes"
      ),
      indexes=get_query(
         x, tsSelect("___Indexes___"),
         format="TabSeparatedWithNamesAndTypes"
      )
   )
   dbm$fields$nullable <- as.logical(dbm$fields$nullable)
   dbm$fields$unique <- as.logical(dbm$fields$unique)
   dbm$indexes$unique <- as.logical(dbm$indexes$unique)
   dataModel <- ReDaMoR::fromDBM(dbm)
   
   ## DB information ----
   dbInfo <- as.list(get_query(
      x, tsSelect("___MDB___"),
      format="Arrow"
   ))
   dbInfo <- c(dbInfo, list("timestamp"=as.POSIXct(timestamp)))

   ## DB tables ----
   if(tstToComplete){
      tst <- rbind(
         tst,
         dplyr::tibble(table=names(dataModel), instance=names(dataModel))
      )
   }
   dbTables <- sprintf(
      "`%s`.`%s`", dbName, tst$instance
   )
   names(dbTables) <- tst$table
   dbTables <- dbTables[names(dataModel)]
   
   ## Collection members ----
   collectionMembers <- get_query(
      x, tsSelect("___CollectionMembers___"),
      format="TabSeparatedWithNamesAndTypes"
   ) %>%
      dplyr::mutate(
         resource=dbName,
         static=as.logical(.data$static)
      ) %>% 
      dplyr::select(
         "collection", "cid", "resource", "mid", "table", "field",
         "static", "value", "type"
      )
   attr(collectionMembers, "data.type") <- NULL
   
   return(chMDB(
      tkcon=x,
      dbTables=dbTables,
      dbInfo=dbInfo,
      dataModel=dataModel,
      collectionMembers=collectionMembers,
      check=check,
      n_max=n_max,
      verbose=FALSE
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
#' Check if the [chMDB] object refers to the current instance of the MDB
#' 
#' @param x a [chMDB] object
#' 
#' @return A single logical:
#' TRUE if x refers to the current instance of the MDB.
#' 
#' @export
#'
is_current_chMDB <- function(x){
   stopifnot(is.chMDB(x))
   ots <- db_info(x)$timestamp
   dts <- list_chMDB_timestamps(unclass(x)$tkcon, db_info(x)$name) %>% 
      attr("current")
   if(is.na(ots)){
      if(is.null(dts) || is.na(dts)){
         return(TRUE)
      }else{
         warning(
            "This object is not timestamped but timestamps exist in",
            " the chTKCat database"
         )
         return(NA)
      }
   }else{
      if(is.null(dts) || is.na(dts)){
         warning("The database in chTKCat is not timestamped")
         return(NA)
      }
      return(dts==ots)
   }
}


###############################################################################@
#' Push an [MDB] object in a ClickHouse database
#'
#' @param x an [MDB] object
#' @param tkcon a [chTKCat] object
#' @param timestamp a single POSIXct value as a timestamp for
#' the chMDB instance.
#' The default value is the current system time.
#' If this value is smaller or equal to the chMDB current value, an error is
#' thrown. If NA, the current instance is overwritten
#' (if the overwrite parameter is set to TRUE) without changing
#' the existing timestamp.
#' @param overwrite a logical indicating if existing data should be overwritten
#' (default: FALSE)
#' @param by the size of the batch: number of records to write
#' together (default: 10^5)
#' 
#' @return A [chMDB] object.
#' 
#' @export
#'
as_chMDB <- function(x, tkcon, timestamp=Sys.time(), overwrite=FALSE, by=10^5){
   
   timestamp <- as.POSIXct(timestamp)
   stopifnot(
      is.MDB(x),
      is.chTKCat(tkcon),
      is.na(timestamp[1]) || inherits(timestamp, "POSIXct"),
      length(timestamp)==1,
      is.logical(overwrite), length(overwrite)==1, !is.na(overwrite)
   )
   con <- tkcon$chcon
   dbInfo <- db_info(x)
   dbInfo <- dbInfo[setdiff(names(dbInfo), "timestamp")]
   dbName <- dbInfo$name
   dataModel <- data_model(x)
   collectionMembers <- collection_members(x)
   
   ## Check existence and availability ----
   mdbl <- list_MDBs(tkcon, withInfo=TRUE)
   if(!dbName %in% mdbl$name){
      stop(
         sprintf("%s does not exist in the chTKCat.", dbName),
         "Create it first."
      )
   }
   
   ## Rules of update ----
   tst <- list_chMDB_timestamps(tkcon, dbName)
   maxTs <- suppressWarnings(max(tst$timestamp))
   if(!is.na(timestamp) && !is.null(maxTs) && timestamp <= maxTs){
      stop("Timestamp should be more recent than those already recorded")
   }
   makeEmpty <- makeArchive <- setTS <- FALSE
   if(!dbName %in% mdbl$name[which(mdbl$populated)]){
      ### Not Populated ----
      if(is.null(tst) || nrow(tst)==0){
         #### No ETS ----
         if(is.na(timestamp)){
            ###### No TS ----
            ## ==> Fill
         }else{
            ###### TS ----
            ## ==> Fill
            setTS <- TRUE
         }
      }else{
         #### ETS ----
         if(is.na(timestamp)){
            ###### No TS ----
            stop("This MDB is timestamped: a timestamp must be provided")
         }else{
            ###### TS ----
            ## ==> Fill
            setTS <- TRUE
         }
      }
   }else{
      ### Populated ----
      if(is.null(tst) || nrow(tst)==0){
         #### No ETS ----
         if(overwrite){
            ##### Overwrite ----
            if(is.na(timestamp)){
               ###### No TS ----
               makeEmpty <- TRUE
               ## ==> Fill
            }else{
               ###### TS ----
               makeEmpty <- TRUE
               ## ==> Fill
               setTS <- TRUE
            }
         }else{
            ##### Not overwrite ----
            if(is.na(timestamp)){
               ###### No TS ----
               stop(
                  sprintf("%s is already used and filled.", dbName),
                  " Set overwrite to TRUE if you want to replace the content.",
                  " Or provide a timestamp to push a new instance."
               )
            }else{
               ###### TS ----
               makeArchive <- TRUE
               ## ==> Fill
               setTS <- TRUE
            }
         }
      }else{
         #### ETS ----
         if(overwrite){
            ##### Overwrite ----
            if(is.na(timestamp)){
               ###### No TS ----
               timestamp <- attr(tst, "current")
               makeEmpty <- TRUE
               ## ==> Fill
               setTS <- TRUE
            }else{
               ###### TS ----
               makeEmpty <- TRUE
               ## ==> Fill
               setTS <- TRUE
            }
         }else{
            ##### Not overwrite ----
            if(is.na(timestamp)){
               ###### No TS ----
               stop(
                  sprintf("%s is already used and filled.", dbName),
                  " Set overwrite to TRUE if you want to replace the content.",
                  " Or provide a timestamp to push a new instance."
               )
            }else{
               ###### TS ----
               makeArchive <- TRUE
               ## ==> Fill
               setTS <- TRUE
            }
         }
      }
   }
   
   ## Identify tables already recorded in CH ----
   ## and that don't need to be rewritten
   if(is.chMDB(x) | is.metaMDB(x)){
      if(get_hosts(tkcon) %in% get_hosts(x)){
         chTables <- db_tables(x, host=get_hosts(tkcon))$dbTables
         if(is.null(tst) || nrow(tst)==0){
            availableTables <- list_tables(tkcon$chcon, dbName) %>% 
               dplyr::select("table"="name", "instance"="name")
         }else{
            availableTables <- tst %>% dplyr::select("table", "instance")
         }
         availableTables$path <- sprintf(
            "`%s`.`%s`", dbName, availableTables$instance
         )
         availableTables$inCh <- as.character(chTables[availableTables$table])
         toKeep <- availableTables$table[which(
            availableTables$table==availableTables$instance &
            availableTables$path==availableTables$inCh
         )] %>% 
            as.character()
      }else{
         toKeep <- character()
      }
   }else{
      toKeep <- character()
   }

   ## Apply rules before filling ----
   if(makeEmpty){
      message("Make empty")
      .empty_chMDB(tkcon, dbName, .toKeep=toKeep)
   }
   if(makeArchive){
      message("Make archive")
      .archive_chMDB(tkcon, dbName, .toKeep=toKeep)
   }
   
   ## FILL ----
   message("Fill")
   
   ### Add relevant collections ----
   if(!is.null(collectionMembers) && nrow(collectionMembers)>0){
      toAdd <- unique(collectionMembers$collection)
      toAdd <- setdiff(
         toAdd, dplyr::pull(list_chTKCat_collections(tkcon), "title")
      )
      for(col in toAdd){
         add_chTKCat_collection(tkcon, col)
      }
   }
   
   ### Write DB information ----
   ch_insert(
      con=con, dbName=dbName, tableName="___MDB___",
      value=dplyr::as_tibble(dbInfo)
   )
   
   ### Write data model ----
   er <- try({
      dbm <- ReDaMoR::toDBM(dataModel)
      ch_insert(
         con=con,
         dbName=dbName,
         tableName="___Tables___",
         value=dbm$tables
      )
      ch_insert(
         con=con,
         dbName=dbName,
         tableName="___Fields___",
         value=dbm$fields
      )
      ch_insert(
         con=con,
         dbName=dbName,
         tableName="___PrimaryKeys___",
         value=dbm$primaryKeys
      )
      ch_insert(
         con=con,
         dbName=dbName,
         tableName="___ForeignKeys___",
         value=dbm$foreignKeys
      )
      ch_insert(
         con=con,
         dbName=dbName,
         tableName="___Indexes___",
         value=dbm$indexes
      )
   }, silent=FALSE)
   if(inherits(er, "try-error")){
      empty_chMDB(tkcon, dbName)
      stop(as.character(er))
   }
   
   ### Write collection members ----
   if(!is.null(collectionMembers)){
      er <- try({
         ch_insert(
            con=con, dbName=dbName, tableName="___CollectionMembers___",
            value=dplyr::select(
               collectionMembers,
               dplyr::all_of(
                  CHMDB_DATA_MODEL$"___CollectionMembers___"$fields$name
               )
            )
         )
      }, silent=FALSE)
      if(inherits(er, "try-error")){
         empty_chMDB(tkcon, dbName)
         stop(as.character(er))
      }
   }
   
   ### Write data ----
   er <- try({
      toWrite <- setdiff(names(x), toKeep)
      mergeTrees_from_RelDataModel(
         con=con, dbName=dbName,
         dbm=dataModel[toWrite, rmForeignKeys=TRUE]
      )
      .write_chTables(x[toWrite], con, dbName, by=by)
   }, silent=FALSE)
   if(inherits(er, "try-error")){
      empty_chMDB(tkcon, dbName)
      stop(as.character(er))
   }
   
   ## Set TS if required ----
   if(setTS){
      message("Set timestamp")
      set_chMDB_timestamp(tkcon, dbName, timestamp)
   }
   
   ## Update grants ----
   update_chMDB_grants(tkcon, dbName)
   
   ## Return the chMDB object ----
   return(get_MDB(x=tkcon, dbName=dbName))
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
   stopifnot(
      length(x)==length(value),
      sum(duplicated(value))==0
   )
   if(length(x)==0){
      return(x)
   }
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
   names(x$dataModel) <- names(x$dbTables) <- value
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
#' @rdname chMDB
#' 
#' @export
#' 
rename.chMDB <- function(.data, ...){
   loc <- tidyselect::eval_rename(rlang::expr(c(...)), .data)
   names <- names(.data)
   names[loc] <- names(loc)
   magrittr::set_names(.data, names)
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
data_tables.chMDB <- function(x, ..., skip=0, n_max=Inf){
   stopifnot(
      is.numeric(skip), length(skip)==1, skip>=0, is.finite(skip),
      is.numeric(n_max), length(n_max)==1, n_max>0
   )
   if(length(x)==0){
      return(list())
   }
   # if(is.infinite(n_max)){
   #    n_max <- max(count_records(x, ...))
   # }
   m <- data_model(x)
   toTake <- tidyselect::eval_select(rlang::expr(c(...)), x)
   if(length(toTake)==0){
      toTake <- 1:length(x)
      names(toTake) <- names(x)
   }
   dbt <- db_tables(x)
   toTake <- dbt$dbTables[toTake]
   toRet <- lapply(
      names(toTake),
      function(tn){
         d <- .get_ch_mtable(
            x,
            tablePath=toTake[tn], tableModel=m[[tn]],
            skip=skip, n_max=n_max
         )
         return(d)
      }
   )
   toRet <- lapply(toRet, function(d){
      if(nrow(d)==0){
         d
      }else{
         d[1:nrow(d),]
      }
   })
   names(toRet) <- names(toTake)
   return(toRet)
}


###############################################################################@
#' 
#' @rdname heads
#' @method heads chMDB
#' 
#' @export
#'
heads.chMDB <- function(x, ..., n=6L){
   stopifnot(
      is.numeric(n), length(n)==1, n>0
   )
   if(length(x)==0){
      return(list())
   }
   m <- data_model(x)
   toTake <- tidyselect::eval_select(rlang::expr(c(...)), x)
   if(length(toTake)==0){
      toTake <- 1:length(x)
      names(toTake) <- names(x)
   }
   dbt <- db_tables(x)
   toTake <- dbt$dbTables[toTake]
   toRet <- lapply(
      names(toTake),
      function(tn){
         .head_ch_mtable(
            x,
            tablePath=toTake[tn], tableModel=m[[tn]],
            n=n
         )
      }
   )
   names(toRet) <- names(toTake)
   return(toRet)
}


###############################################################################@
#' 
#' @rdname dims
#' @method dims chMDB
#' 
#' @export
#'
dims.chMDB <- function(x, ...){
   if(length(x)==0){
      return(dplyr::tibble(
         name=character(),
         format=character(),
         ncol=numeric(),
         nrow=numeric(),
         records=numeric(),
         transposed=logical()
      ))
   }
   toTake <- tidyselect::eval_select(rlang::expr(c(...)), x)
   if(length(toTake)==0){
      toTake <- 1:length(x)
      names(toTake) <- names(x)
   }
   toTake <- names(toTake)
   dbt <- db_tables(x)
   toTake <- dbt$dbTables[toTake]
   m <- data_model(x)
   
   toRet <- do.call(dplyr::bind_rows, lapply(
      names(toTake),
      function(tn){
         .dim_ch_mtable(
            x,
            tablePath=toTake[tn], tableModel=m[[tn]]
         )
      }
   )) %>%
      dplyr::mutate(name=names(toTake)) %>% 
      dplyr::relocate("name")
   
   return(toRet)
   
}


###############################################################################@
#' Get the DB tables from a [chMDB] or [metaMDB] object
#' 
#' @param x a [chMDB] or a [metaMDB] object
#' @param host the name of host (as returned by `[get_hosts]`) to focus on.
#' Only used with [metaMDB] objects.
#' 
#' @return a list with a chTKCat object (tkcon) and
#' a named vector of DB table names (dbTables).
#' 
#' @export
#'
db_tables <- function(x, host){
   stopifnot(is.chMDB(x) || is.metaMDB(x))
   if(is.chMDB(x)){
      x <- unclass(x)
      toTake <- c("tkcon", "dbTables")
      stopifnot(all(toTake %in% names(x)))
      return(x[toTake])
   }
   if(is.metaMDB(x)){
      hosts <- get_hosts(x)
      if(missing(host)){
         host <- hosts[1]
         if(length(hosts)>1){
            warning(
               sprintf("No host provided ==> taking the first one: %s", host)
            )
         }
      }
      mdbs <- MDBs(x)
      toRet <- lapply(mdbs, function(y){
         if(is.metaMDB(y)){
            toRet <- db_tables(y, host)
         }else{
            if(is.chMDB(y) && get_hosts(y)==host){
               toRet <- db_tables(y)
            }else{
               toRet <- NULL
            }
         }
         return(toRet)
      })
      toRet <- toRet[which(lengths(toRet)>0)] %>% magrittr::set_names(NULL)
      if(length(toRet) > 0){
         toRet <- list(
            tkcon=toRet[[1]]$tkcon,
            dbTables=do.call(c, lapply(toRet, function(z) z$dbTables))
         )
      }
      return(toRet)
   }
}


###############################################################################@
#' 
#' @param x a [chMDB] object
#' @param i index or names of the tables to take
#'
#' @rdname chMDB
#' 
#' @export
#'
'[.chMDB' <- function(x, i){
   if(missing(i)){
      return(x)
   }
   if(length(i)==0){
      dbi <- db_info(x)
      return(chMDB(
         tkcon=db_tables(x)$tkcon,
         dbTables=as.character(),
         dbInfo=dbi,
         dataModel=ReDaMoR::RelDataModel(l=list()),
         check=FALSE
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
   dm <- data_model(x)[i, rmForeignKeys=TRUE]
   dbt <- db_tables(x)
   tkcon <- dbt$tkcon
   dbt <- dbt$dbTables[i]
   cm <- collection_members(x)
   if(!is.null(cm)){
      cm <- cm %>%
         dplyr::filter(.data$table %in% !!i) %>%
         dplyr::mutate(resource=!!dbi$name)
   }
   toRet <- chMDB(
      tkcon=tkcon,
      dbTables=dbt,
      dbInfo=dbi,
      dataModel=dm,
      collectionMembers=cm,
      check=FALSE
   )
   return(toRet)
}


###############################################################################@
#' 
#' @param x a [chMDB] object
#' @param i the index or the name of the tables to take
#' 
#' @export
#'
#' @rdname chMDB
#'
'[[.chMDB' <- function(x, i){
   stopifnot(
      length(i)==1
   )
   ## Rstudio hack to avoid DB call when just looking for names
   cc <- grep('.rs.getCompletionsDollar', deparse(sys.calls()), value=FALSE)
   if(length(cc)!=0){
      invisible(NULL)
   }else{
      cc <- c(
         grep('.rs.valueContents', deparse(sys.calls()), value=FALSE),
         grep('.rs.explorer.inspectObject', deparse(sys.calls()), value=FALSE)
      )
      if(length(cc)!=0){
         invisible()
      }else{
         return(data_tables(x, dplyr::all_of(i))[[1]])
      }
   }
}
#' @rdname fileMDB
#' @export
'$.chMDB' <- `[[.chMDB`


###############################################################################@
#' 
#' 
#' @rdname as_fileMDB
#' @method as_fileMDB chMDB
#' 
#' @export
#'
as_fileMDB.chMDB <- function(
   x, path,
   readParameters=list(delim="\t", na="<NA>"),
   htmlModel=TRUE,
   compress=TRUE,
   by=10^5,
   ...
){
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
   rp <- .check_read_params(readParameters)
   descFile <- file.path(fullPath, "DESCRIPTION.json")
   .writeDescription(c(dbInfo, rp), descFile)
   
   ## Data model ----
   dm <- data_model(x)
   modelPath <- file.path(fullPath, "model")
   dir.create(modelPath)
   jModelPath <- file.path(modelPath, paste0(dbName, ".json"))
   hModelPath <- file.path(modelPath, paste0(dbName, ".html"))
   ReDaMoR::write_json_data_model(dm, jModelPath)
   if(htmlModel){
      plot(dm) %>%
         visNetwork::visSave(hModelPath)
   }
   
   ## Collection members ----
   cm <- collection_members(x)
   if(!is.null(cm) && nrow(cm)>0){
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
   ext <- ifelse(compress, ".txt.gz", ".txt")
   dfiles <- file.path(dataPath, paste0(names(x), ext))
   names(dfiles) <- names(x)
   for(tn in names(x)){
      ddim <- dims(x, dplyr::all_of(tn))
      if(ddim$format=="MatrixMarket"){
         dbti <- db_tables(x)$dbTables[tn]
         tdb <- sub("^`", "", sub("`[.]`.*$", "", dbti))
         
         tquery <- sprintf("SELECT * FROM %s", dbti)
         qr <- get_query(
            x, tquery, autoalias=FALSE,
            format="TabSeparatedWithNamesAndTypes"
         )
         
         rquery <- sprintf(
            "SELECT i, name FROM %s ORDER BY i",
            sprintf('`%s`.`%s`', tdb, qr$table[which(qr$info=="rows")])
         )
         rowNames <- get_query(
            x, rquery, autoalias=FALSE,
            format="Arrow"
         )
         
         cquery <- sprintf(
            "SELECT j, name FROM %s ORDER BY j",
            sprintf('`%s`.`%s`', tdb, qr$table[which(qr$info=="columns")])
         )
         colNames <- get_query(
            x, cquery, autoalias=FALSE,
            format="Arrow"
         )
         
         chTables <- list_tables(
            unclass(x)$tkcon$chcon, dbNames=tdb
         )
         nv <- chTables %>% 
            dplyr::filter(
               .data$database==tdb &
                  .data$name==qr$table[which(qr$info=="values")]
            ) %>% 
            dplyr::pull("total_rows")
         
         readr::write_delim(
            dplyr::tibble(h=c(
               "%%MatrixMarket matrix coordinate real general",
               paste0("%%Rownames: ", paste(rowNames$name, collapse="\t")),
               paste0("%%Colnames: ", paste(colNames$name, collapse="\t")),
               paste(nrow(rowNames), nrow(colNames), nv, sep=" ")
            )),
            file=dfiles[tn],
            delim="\t",
            quote="none",
            col_names=FALSE
         )
         
         vtquery <- sprintf(
            "SELECT i, j, x FROM %s ORDER BY j, i",
            sprintf('`%s`.`%s`', tdb, qr$table[which(qr$info=="values")])
         )
         r <- 0
         vquery <- paste(
            vtquery,
            sprintf("LIMIT %s, %s", r, by)
         )
         toWrite <- get_query(
            x, vquery, autoalias=FALSE,
            format="Arrow"
         )
         while(!is.null(toWrite) && nrow(toWrite)>0){
            readr::write_delim(
               toWrite, file=dfiles[tn],
               delim="\t",
               quote="none",
               col_names=FALSE,
               append=TRUE
            )
            r <- r + nrow(toWrite)
            message(sprintf("%s rows written over %s", r, nv))
            vquery <- paste(
               vtquery,
               sprintf("LIMIT %s, %s", r, by)
            )
            toWrite <- get_query(
               x, vquery, autoalias=FALSE,
               format="Arrow"
            )
         }

      }else{
      
         r <- 0
         toWrite <- data_tables(x, dplyr::all_of(tn), skip=r, n_max=by)[[1]]
         while(!is.null(toWrite) && nrow(toWrite)>0){
            if(is.matrix(toWrite)){
               toWrite <- dplyr::as_tibble(toWrite, rownames="___ROWNAMES___")
            }
            readr::write_delim(
               toWrite, file=dfiles[tn],
               delim=rp$delim,
               na=rp$na,
               quote="all", escape="double",
               append=file.exists(dfiles[tn])
            )
            r <- r + nrow(toWrite)
            message(sprintf("%s rows written over %s", r, ddim$nrow))
            toWrite <- data_tables(x, dplyr::all_of(tn), skip=r, n_max=by)[[1]]
         }
      
      }
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


###############################################################################@
#' Filter a [chMDB] object and return a [memoMDB]
#' 
#' @param .data a [chMDB] object
#' @param ... each argument should have the name of one of the tables of the
#' [chMDB] object and contain a simple logical expression involving
#' the names of the corresponding table.
#' @param by the size of the batch: number of records to filter
#' together (default: 10^5)
#' @param .preserve not used
#' 
#' @return a [memoMDB] object
#' 
#' @export
#'
filter.chMDB <- function(.data, ..., by=10^5, .preserve=FALSE){
   
   x <- .data
   dm <- data_model(x)
   
   ## Apply rules
   toRet <- list()
   dots <- rlang::enquos(...)
   for(tn in names(dots)){
      if(!tn %in% names(x)){
         stop(sprintf("%s table does not exist", tn))
      }
      if(ReDaMoR::is.MatrixModel(data_model(x)[[tn]])){
         stop("Cannot filter a matrix: start from another table")         
      }
      ft <- c()
      toAdd <- data_tables(x, dplyr::all_of(tn), skip=0, n_max=by)[[1]]
      r <- nrow(toAdd)
      while(nrow(toAdd)>0){
         ft <- dplyr::bind_rows(ft, dplyr::filter(toAdd, !!dots[[tn]]))
         toAdd <- data_tables(x, dplyr::all_of(tn), skip=r, n_max=by)[[1]]
         r <- r + nrow(toAdd)
      }
      toRet[[tn]] <- ft
   }
   
   ## Filter with tables
   return(filter_with_tables(x, toRet, checkTables=FALSE))
}


###############################################################################@
#' Subset a [chMDB] object according to row position in one table
#' and return a [memoMDB]
#' 
#' @param .data a [chMDB] object
#' @param ... a single argument. The name of this argument should be a table
#' name of x and the value of this argument should be vector of integers
#' corresponding to row indexes.
#' @param by the size of the batch: number of records to slice
#' together (default: 10^5)
#' @param .preserve not used
#' 
#' @return a [memoMDB] object
#' 
#' @export
#'
slice.chMDB <- function(.data, ..., by=10^5, .preserve=FALSE){
   
   x <- .data
   
   ## Apply rules
   toRet <- list()
   dots <- list(...)
   if(length(dots)>1){
      stop("Only one argument should be supplied in '...'")
   }
   tn <- names(dots)
   if(!tn %in% names(x)){
      stop(sprintf("%s table does not exist", tn))
   }
   if(ReDaMoR::is.MatrixModel(data_model(x)[[tn]])){
      stop("Cannot slice a matrix: start from another table")         
   }
   i <- dots[[tn]]
   
   ft <- data_tables(x, dplyr::all_of(tn), skip=0, n_max=1)[[1]] %>% 
      dplyr::slice(2)
   s <- 0
   j <- i-s
   j <- j[j>0 & j <=by]
   if(length(j)>0){
      ft <- dplyr::bind_rows(
         ft,
         data_tables(x, dplyr::all_of(tn), skip=s, n_max=by)[[1]] %>% 
            dplyr::slice(j)
      )
   }
   r <- s+by
   while(r <= count_records(x, dplyr::all_of(tn))){
      s <- r
      j <- i-s
      j <- j[j>0 & j <=by]
      if(length(j)>0){
         ft <- dplyr::bind_rows(
            ft,
            data_tables(x, dplyr::all_of(tn), skip=s, n_max=by)[[1]] %>% 
               dplyr::slice(j)
         )
      }
      r <- s+by
   }
   toRet[[tn]] <- ft
   
   ## Filter with tables
   return(filter_with_tables(x, toRet, checkTables=FALSE))
   
}


###############################################################################@
#' 
#' @param by the size of the batch: number of records to process
#' together (default: 10000)
#' 
#' @rdname filter_with_tables
#' @method filter_with_tables chMDB
#' 
#' @export
#'
filter_with_tables.chMDB <- function(x, tables, checkTables=TRUE, by=10^5, ...){
   
   ## Check the tables ----
   if(checkTables){
      for(tn in names(tables)){
         cr <- ReDaMoR::confront_table_data(data_model(x)[[tn]], tables[[tn]])
         if(!cr$success){
            stop(sprintf("The %s table does not fit the data model"), tn)
         }
      }
   }
   
   ## Identify foreign keys ----
   dm <- data_model(x)
   fk <- ReDaMoR::get_foreign_keys(dm)
   
   ## Filter by contamination ----
   tables <- .ch_filtByConta(tables, x, fk, by=by)
   dm <- dm[names(tables), rmForeignKeys=TRUE]
   tables <- .norm_data(tables,  dm)
   
   ## Collection members ----
   cm <- collection_members(x)
   if(!is.null(cm)){
      cm <- cm %>% dplyr::filter(.data$table %in% names(tables))
   }
   
   ## Results ----
   return(memoMDB(
      dataTables=tables,
      dataModel=dm,
      dbInfo=db_info(x),
      collectionMembers=cm,
      check=FALSE
   ))
   
}


###############################################################################@
#' 
#' @rdname filter_mdb_matrix
#' @method filter_mdb_matrix chMDB
#' 
#' @export
#'
filter_mdb_matrix.chMDB <- function(x, tableName, ...){
   
   ## Checks ----
   stopifnot(
      is.chMDB(x),
      tableName %in% names(x)
   )
   tableModel <- data_model(x)[[tableName]]
   stopifnot(ReDaMoR::is.MatrixModel(tableModel))
   iFilter <- list(...)
   stopifnot(
      length(names(iFilter)) > 0, length(iFilter) <= 2, 
      !any(duplicated(names(iFilter))),
      all(names(iFilter) %in% tableModel$fields$name)
   )
   vfield <- tableModel$fields %>%
      dplyr::filter(!.data$type %in% c("row", "column")) %>% 
      dplyr::pull("name") %>% 
      intersect(names(iFilter))
   if(length(vfield)>0){
      stop("Cannot filter a matrix on values; only on row or column names")
   }
   
   ## Table info ----
   dbti <- db_tables(x)$dbTables[tableName]
   dbn <- sub("^`", "", sub("`[.]`.*$", "", dbti))
   qr <- get_query(
      x,
      sprintf("SELECT * FROM %s", dbti),
      autoalias=FALSE,
      format="TabSeparatedWithNamesAndTypes"
   )
   
   if(.is_chMM(qr)){
      
      ## Sparse matrix ----
      
      rt <- qr$table[which(qr$info=="rows")]
      ct <- qr$table[which(qr$info=="columns")]
      vt <- qr$table[which(qr$info=="values")]
      
      fr <- fc <- NULL
      for(f in names(iFilter)){
         ft <- tableModel$fields %>%
            dplyr::filter(.data$name==!!f) %>%
            dplyr::pull("type")
         if(ft=="row"){
            frn <- iFilter[[f]]
            if(length(frn)==0){
               fr <- dplyr::tibble(i=integer(0), name=character(0))
            }else{
               fr <- get_query(
                  x,
                  sprintf(
                     paste(
                        "SELECT i, name FROM `%s`.`%s` WHERE name in ('%s')",
                        "ORDER BY i"
                     ),
                     dbn, rt, paste(frn, collapse="', '")
                  ),
                  autoalias=FALSE,
                  format="Arrow"
               )
            }
         }
         if(ft=="column"){
            fcn <- iFilter[[f]]
            if(length(fcn)==0){
               fc <- dplyr::tibble(j=integer(0), name=character(0))
            }else{
               fc <- get_query(
                  x,
                  sprintf(
                     paste(
                        "SELECT j, name FROM `%s`.`%s` WHERE name in ('%s')",
                        "ORDER BY j"
                     ),
                     dbn, ct, paste(fcn, collapse="', '")
                  ),
                  autoalias=FALSE,
                  format="Arrow"
               )
            }
         }
      }
      
      if(is.null(fr)){
         fri <- get_query(
            x,
            sprintf(
               "SELECT i, name FROM `%s`.`%s` ORDER BY i",
               dbn, rt
            ),
            autoalias=FALSE,
            format="Arrow"
         )
         frn <- fri$name
      }else(
         fri <- fr
      )
      if(is.null(fc)){
         fcj <- get_query(
            x,
            sprintf(
               "SELECT j, name FROM `%s`.`%s` ORDER BY j",
               dbn, ct
            ),
            autoalias=FALSE,
            format="Arrow"
         )
         fcn <- fcj$name
      }else{
         fcj <- fc
      }
      
      ### No value ----
      if(!is.null(fr) && nrow(fr)==0){
         if(!is.null(fc) && nrow(fc)==0){
            return(Matrix::drop0(matrix(numeric(0), nrow=0, ncol=0)))
         }else{
            toRet <- matrix(
               numeric(0), nrow=0, ncol=nrow(fcj),
               dimnames=list(character(0), fcj$name)
            )
            return(Matrix::drop0(
               toRet[, intersect(fcn, colnames(toRet)), drop=FALSE]
            ))
         }
      }else{
         if(!is.null(fc) && nrow(fc)==0){
            toRet <- matrix(
               numeric(0), nrow=nrow(fri), ncol=0,
               dimnames=list(fri$name, character(0))
            )
            return(Matrix::drop0(
               toRet[intersect(frn, rownames(toRet)), , drop=FALSE]
            ))
         }
      }
      
      ### Value query ----
      query <- sprintf(
         "SELECT i, j, x FROM `%s`.`%s` WHERE %s AND %s ORDER BY j, i",
         dbn, vt,
         ifelse(
            is.null(fr), "1",
            sprintf('i in (%s)', paste(fr$i, collapse=", "))
         ),
         ifelse(
            is.null(fc), "1",
            sprintf('j in (%s)', paste(fc$j, collapse=", "))
         )
      )
      toRet <- get_query(x, query, autoalias=FALSE, format="Arrow")
      mi <- max(fri$i)
      mj <- max(fcj$j)
      if(!mi %in% toRet$i || !mj %in% toRet$j){
         toRet <- rbind(
            toRet,
            dplyr::tibble(i=mi, j=mj, x=0)
         )
      }
      fri2 <- rbind(
         fri,
         dplyr::tibble(
            i=setdiff(1:mi, fri$i),
            name="x"
         )
      ) %>% arrange(.data$i)
      fcj2 <- rbind(
         fcj,
         dplyr::tibble(
            j=setdiff(1:mj, fcj$j),
            name="x"
         )
      ) %>% arrange(.data$j)
      toRet <- Matrix::sparseMatrix(
         i=toRet$i, j=toRet$j, x=toRet$x,
         dimnames=list(fri2$name, fcj2$name)
      )[fri$i, fcj$j, drop=FALSE]
      return(Matrix::drop0(toRet[
         intersect(frn, rownames(toRet)),
         intersect(fcn, colnames(toRet)),
         drop=FALSE
      ]))
      
      
   }else{
      
      ## Matrix ----
   
      mtables <- qr$table
      vtype <- setdiff(
         tableModel$fields$type,
         c("row", "column")
      )
      dimcol <- get_query(
         x,
         sprintf(
            paste(
               "SELECT name FROM system.columns",
               " WHERE database='%s' AND table='%s'"
            ),
            dbn, mtables[1]
         ),
         autoalias=FALSE,
         format="TabSeparatedWithNamesAndTypes"
      )
      dimcol <- intersect(
         dimcol$name, c("___COLNAMES___", "___ROWNAMES___")
      )
      stopifnot(length(dimcol)==1)
      chFields <- get_query(
         x,
         sprintf(
            paste(
               "SELECT database, table, name FROM system.columns",
               " WHERE database='%s' AND table IN ('%s')"
            ),
            dbn,
            paste(unique(mtables), collapse="', '")
         ),
         autoalias=FALSE,
         format="TabSeparatedWithNamesAndTypes"
      ) %>% 
         dplyr::filter(.data$name!=dimcol) %>% 
         dplyr::arrange(.data$name)
      
      ## Create clause and selected fields ----
      clause <- ""
      sel <- NA
      frc <- c()
      for(f in names(iFilter)){
         ft <- tableModel$fields %>%
            dplyr::filter(.data$name==!!f) %>%
            dplyr::pull("type")
         if(ft=="row"){
            fr <- iFilter[[f]]
            frc <- c(frc, "r")
            if(dimcol=="___ROWNAMES___"){
               clause <- sprintf(
                  "WHERE `___ROWNAMES___` IN ('%s')",
                  paste(iFilter[[f]],  collapse="', '")
               )
            }else{
               sel <- iFilter[[f]]
            }
         }
         if(ft=="column"){
            fc <- iFilter[[f]]
            frc <- c(frc, "c")
            if(dimcol=="___COLNAMES___"){
               clause <- sprintf(
                  "WHERE `___COLNAMES___` IN ('%s')",
                  paste(iFilter[[f]],  collapse="', '")
               )
            }else{
               sel <- iFilter[[f]]
            }
         }
      }
      frc <- paste(sort(frc), collapse="")
      queryTemplate <- sprintf(
         "SELECT %s FROM `%s`.`%s`",
         "%s", dbn, "%s"
      )
      
      ## Build the query ----
      i <- 1
      if(length(sel)==1 && is.na(sel[1])){
         tquery <- paste(
            sprintf(
               queryTemplate, "*", mtables[[i]]
            ),
            clause
         )
      }else{
         totake <- chFields %>%
            dplyr::filter(.data$table==!!mtables[[i]]) %>%
            dplyr::pull("name") %>% 
            intersect(sel)
         totake <- c(dimcol, totake) %>% 
            paste(collapse="`, `")
         totake <- paste0("`", totake, "`")
         tquery <- paste(
            sprintf(
               queryTemplate,
               totake,
               mtables[[i]]
            ),
            clause
         )
      }
      query <- tquery
      if(length(sel) >0 && length(mtables)>1) for(i in 2:length(mtables)){
         if(is.na(sel[1])){
            tquery <- paste(
               sprintf(
                  queryTemplate, "*", mtables[[i]]
               ),
               clause
            )
         }else{
            totake <- chFields %>%
               dplyr::filter(.data$table==!!mtables[[i]]) %>%
               dplyr::pull("name") %>% 
               intersect(sel)
            totake <- c(dimcol, totake) %>% 
               paste(collapse="`, `")
            totake <- paste0("`", totake, "`")
            tquery <- paste(
               sprintf(
                  queryTemplate,
                  totake,
                  mtables[[i]]
               ),
               clause
            )
         }
         query <- paste0(
            "SELECT * FROM (",
            query,
            ") FULL JOIN (",
            tquery,
            ') USING `',
            dimcol,
            '`'
         )
      }
   
      ## Get the results ----
      toRet <- get_query(
         x, query, autoalias=FALSE,
         format="TabSeparatedWithNamesAndTypes"
      )
      dimname <- toRet[[dimcol]]
      toRet <- toRet[, -which(colnames(toRet)==dimcol), drop=FALSE] %>% 
         as.matrix() %>% 
         magrittr::set_rownames(dimname)
      if(!vtype %in% c("Date", "POSIXct")){
         toRet <- toRet %>% magrittr::set_class(vtype)
      }
      if(frc=="r"){
         toRet <- toRet[intersect(fr, rownames(toRet)),, drop=FALSE]
      }
      if(frc=="c"){
         toRet <- toRet[,intersect(fc, colnames(toRet)), drop=FALSE]
      }
      if(frc=="cr"){
         toRet <- toRet[
            intersect(fr, rownames(toRet)),
            intersect(fc, colnames(toRet)),
            drop=FALSE
         ]
      }
      
   }
   
   return(toRet)
   
}


###############################################################################@
## Helpers ----
.write_chTables.chMDB <- function(x, con, dbName, by=10^5, ...){
   dm <- data_model(x)
   for(tn in names(x)){
      if(ReDaMoR::is.MatrixModel(dm[[tn]])){
         
         ddim <- dims(x, dplyr::all_of(tn))
         if(ddim$format=="MatrixMarket"){
            
            ## Sparse matrix ----
            
            dbti <- db_tables(x)$dbTables[tn]
            tdb <- sub("^`", "", sub("`[.]`.*$", "", dbti))
            
            tquery <- sprintf("SELECT * FROM %s", dbti)
            qr <- get_query(
               x, tquery, autoalias=FALSE,
               format="TabSeparatedWithNamesAndTypes"
            )
            
            ## Columns and rows
            rquery <- sprintf(
               "SELECT i, name FROM %s ORDER BY i",
               sprintf('`%s`.`%s`', tdb, qr$table[which(qr$info=="rows")])
            )
            rowNames <- get_query(
               x, rquery, autoalias=FALSE,
               format="Arrow"
            )
            
            cquery <- sprintf(
               "SELECT j, name FROM %s ORDER BY j",
               sprintf('`%s`.`%s`', tdb, qr$table[which(qr$info=="columns")])
            )
            colNames <- get_query(
               x, cquery, autoalias=FALSE,
               format="Arrow"
            )
            
            write_MergeTree(
               con=con,
               dbName=dbName,
               tableName=qr$table[which(qr$info=="rows")],
               value=rowNames,
               rtypes=c("i"="integer", name="character"),
               nullable=NULL,
               sortKey="i"
            )
            write_MergeTree(
               con=con,
               dbName=dbName,
               tableName=qr$table[which(qr$info=="columns")],
               value=colNames,
               rtypes=c("j"="integer", name="character"),
               nullable=NULL,
               sortKey="j"
            )
            
            ## Values
            valTable <- qr$table[which(qr$info=="values")]
            write_MergeTree(
               con=con,
               dbName=dbName,
               tableName=valTable,
               value=dplyr::tibble(
                  i=integer(),
                  j=integer(),
                  x=numeric()
               ),
               rtypes=c("i"="integer", "j"="integer", "x"="numeric"),
               nullable=NULL,
               sortKey=c("i", "j")
            )
            chTables <- list_tables(
               unclass(x)$tkcon$chcon, dbNames=tdb
            )
            nv <- chTables %>% 
               dplyr::filter(
                  .data$database==tdb &
                     .data$name==valTable
               ) %>% 
               dplyr::pull("total_rows")
            
            vtquery <- sprintf(
               "SELECT i, j, x FROM %s ORDER BY j, i",
               sprintf('`%s`.`%s`', tdb, valTable)
            )
            r <- 0
            vquery <- paste(
               vtquery,
               sprintf("LIMIT %s, %s", r, by)
            )
            toWrite <- get_query(
               x, vquery, autoalias=FALSE,
               format="Arrow"
            )
            while(!is.null(toWrite) && nrow(toWrite)>0){
               ch_insert(
                  con=con, dbName=dbName, tableName=valTable, value=toWrite
               )
               r <- r + nrow(toWrite)
               message(sprintf("%s rows written over %s", r, nv))
               vquery <- paste(
                  vtquery,
                  sprintf("LIMIT %s, %s", r, by)
               )
               toWrite <- get_query(
                  x, vquery, autoalias=FALSE,
                  format="Arrow"
               )
            }
            
            ## Reference table
            ch_insert(
               con=con, dbName=dbName, tableName=tn,
               value=qr
            )
            
         }else{
            
            ## Matrix ----
         
            nullable <- dm[[tn]]$fields %>% 
               dplyr::filter(!.data$type %in% c("column", "row")) %>% 
               dplyr::pull("nullable")
            vtype <- setdiff(dm[[tn]]$fields$type, c("column", "row"))
            tnpath <- db_tables(x)$dbTables[[tn]]
            tndb <- sub("^`", "", sub("`[.]`.*$", "", tnpath))
            stl <- get_query(
               x,
               sprintf(
                  "SELECT * FROM %s",
                  tnpath
               ),
               autoalias=FALSE,
               format="TabSeparatedWithNamesAndTypes"
            )
            ch_insert(con=con, dbName=dbName, tableName=tn, value=stl)
            for(stn in stl$table){
               toWrite <- get_query(
                  x,
                  sprintf("SELECT * FROM `%s`.`%s` LIMIT 0, %s", tndb, stn, by),
                  autoalias=FALSE,
                  format="TabSeparatedWithNamesAndTypes"
               )
               nulcol <- NULL
               if(nullable){
                  nulcol <- colnames(toWrite)[-1]
               }
               write_MergeTree(
                  con=con,
                  dbName=dbName,
                  tableName=stn,
                  value=toWrite,
                  rtypes=c("character", rep(vtype, ncol(toWrite)-1)) %>% 
                     magrittr::set_names(colnames(toWrite)),
                  nullable=nulcol,
                  sortKey=colnames(toWrite)[1]
               )
               r <- nrow(toWrite)
               toWrite <- get_query(
                  x,
                  sprintf(
                     "SELECT * FROM `%s`.`%s` LIMIT %s, %s",
                     tndb, stn, r, by
                  ),
                  autoalias=FALSE,
                  format="TabSeparatedWithNamesAndTypes"
               )
               while(nrow(toWrite)>0){
                  ch_insert(
                     con=con, dbName=dbName, tableName=stn, value=toWrite
                  )
                  r <- r + nrow(toWrite)
                  toWrite <- get_query(
                     x,
                     sprintf(
                        "SELECT * FROM `%s`.`%s` LIMIT %s, %s",
                        tndb, stn, r, by
                     ),
                     autoalias=FALSE,
                     format="TabSeparatedWithNamesAndTypes"
                  )
               }
            }
         }
      }else{{
         
         ## Table ----
         
         toWrite <- data_tables(x, dplyr::all_of(tn), skip=0, n_max=by)[[1]]
         r <- nrow(toWrite)
         while(nrow(toWrite)>0){
            ch_insert(con=con, dbName=dbName, tableName=tn, value=toWrite)
            toWrite <- data_tables(x, dplyr::all_of(tn), skip=r, n_max=by)[[1]]
            r <- r + nrow(toWrite)
         }
      }}
   }
}

.is_chMM <- function(d){
   identical(sort(colnames(d)), sort(c("info", "table"))) &&
      identical(sort(d$info), sort(c("columns", "rows", "values")))
}

.get_ch_mtable <- function(
   x,
   tablePath,
   tableModel,
   skip=0, n_max=Inf
){
   
   getsubtab <- skip > 0 || !is.infinite(n_max)
   if(is.infinite(n_max)){
      n_max <- "18446744073709551615"
   }
   dbti <- tablePath
   tdb <- sub("^`", "", sub("`[.]`.*$", "", dbti))
   if(ReDaMoR::is.MatrixModel(tableModel)){
      query <- "SELECT * FROM %s"
      tquery <- sprintf(query, dbti)
      qr <- get_query(
         x, tquery, autoalias=FALSE,
         format="TabSeparatedWithNamesAndTypes"
      )
      
      if(.is_chMM(qr)){
         
         ## Sparse matrix ----
         if(getsubtab){
            warning(
               "Be careful when using skip and n_max parameters: ",
               "Subsetting a sparse matrix from clickhouse may return ",
               "inconsistent 0 values"
            )
         }
         
         vquery <- sprintf(
            "SELECT i, j, x FROM %s ORDER BY j, i",
            sprintf('`%s`.`%s`', tdb, qr$table[which(qr$info=="values")])
         )
         vquery <- paste(
            vquery,
            sprintf("LIMIT %s, %s", skip, n_max)
         )
         values <- get_query(
            x, vquery, autoalias=FALSE,
            format="Arrow"
         )
         
         rToTake <- max(values$i)
         rquery <- sprintf(
            "SELECT i, name FROM %s WHERE i <= %s ORDER BY i",
            sprintf('`%s`.`%s`', tdb, qr$table[which(qr$info=="rows")]),
            rToTake
         )
         rowNames <- get_query(
            x, rquery, autoalias=FALSE,
            format="Arrow"
         )
         
         cToTake <- max(values$j)
         cquery <- sprintf(
            "SELECT j, name FROM %s WHERE j <= %s ORDER BY j",
            sprintf('`%s`.`%s`', tdb, qr$table[which(qr$info=="columns")]),
            cToTake
         )
         colNames <- get_query(
            x, cquery, autoalias=FALSE,
            format="Arrow"
         )
         
         toRet <- Matrix::sparseMatrix(
            i=as.integer(values$i), j=as.integer(values$j), x=values$x,
            dimnames=list(rowNames$name, colNames$name)
         )
         
      }else{
      
         ## Matrix ----
         
         toRet <- list()
         vtype <- setdiff(
            tableModel$fields$type,
            c("row", "column")
         )
         dimcol <- get_query(
            x,
            sprintf(
               paste(
                  "SELECT name FROM system.columns",
                  " WHERE database='%s' AND table='%s'"
               ),
               tdb, qr$table[1]
            ),
            autoalias=FALSE,
            format="TabSeparatedWithNamesAndTypes"
         )
         dimcol <- intersect(
            dimcol$name, c("___COLNAMES___", "___ROWNAMES___")
         )
         stopifnot(length(dimcol)==1)
         chFields <- get_query(
            x,
            sprintf(
               paste(
                  "SELECT database, table, name FROM system.columns",
                  " WHERE database='%s' AND table IN ('%s')"
               ),
               tdb,
               paste(unique(qr$table), collapse="', '")
            ),
            autoalias=FALSE,
            format="TabSeparatedWithNamesAndTypes"
         ) %>% 
            dplyr::filter(.data$name!=dimcol) %>% 
            dplyr::arrange(.data$name)
         if(dimcol=="___ROWNAMES___"){
            for(st in unique(chFields$table)){
               stquery <- sprintf(
                  paste(query, 'ORDER BY %s'),
                  sprintf('`%s`.`%s`', tdb, st),
                  dimcol
               )
               stquery <- paste(
                  stquery,
                  sprintf("LIMIT %s, %s", skip, n_max)
               )
               stqr <- get_query(
                  x, stquery, autoalias=FALSE,
                  format="TabSeparatedWithNamesAndTypes"
               )
               dimname <- stqr[[dimcol]]
               stopifnot(
                  !any(duplicated(dimname)),
                  ncol(stqr) > 1
               )
               toAdd <- stqr[, -1, drop=FALSE] %>% 
                  as.matrix() %>% 
                  magrittr::set_rownames(dimname)
               toRet <- c(toRet, list(toAdd))
            }
            toRet <- do.call(cbind, toRet)
            if(!vtype %in% c("Date", "POSIXct")){
               toRet <- magrittr::set_class(toRet, vtype)
            }
            toRet <- toRet[, sort(colnames(toRet))]
         }else{
            if(skip >= nrow(chFields)){
               return(NULL)
            }
            chFields <- chFields[(skip + 1):nrow(chFields),]
            chFields <- chFields[1:min(nrow(chFields), n_max),]
            for(st in unique(chFields$table)){
               stquery <- sprintf(
                  'SELECT `%s` FROM %s ORDER BY %s',
                  paste(
                     c(
                        dimcol,
                        chFields$name[which(chFields$table==st)]
                     ),
                     collapse='`, `'
                  ),
                  sprintf('`%s`.`%s`', tdb, st),
                  dimcol
               )
               stqr <- get_query(
                  x, stquery, autoalias=FALSE,
                  format="TabSeparatedWithNamesAndTypes"
               )
               dimname <- stqr[[dimcol]]
               stopifnot(
                  !any(duplicated(dimname)),
                  ncol(stqr) > 1
               )
               toAdd <- stqr[, -1, drop=FALSE] %>% 
                  as.matrix() %>% 
                  magrittr::set_rownames(dimname)
               toRet <- c(toRet, list(toAdd))
            }
            toRet <- do.call(cbind, toRet) %>%
               t()
            if(!vtype %in% c("Date", "POSIXct")){
               toRet <- magrittr::set_class(toRet, vtype)
            }
            toRet <- toRet[sort(rownames(toRet)),]
         }
      }
   }else{{
      
      ## Table ----
      
      query <- "SELECT * FROM %s ORDER BY %s LIMIT %s, %s"
      toRet <- get_query(
         x,
         sprintf(
            query,
            tablePath,
            .get_tm_sortKey(tableModel, quoted=TRUE),
            skip, n_max
         ),
         autoalias=FALSE,
         format=ifelse(
            "base64" %in% tableModel$fields$type | nrow(tableModel$fields) < 50,
            "Arrow",
            "TabSeparatedWithNamesAndTypes"
         )
      )
      attr(toRet, "data.type") <- NULL
      cc <- unlist(lapply(toRet, function(x) class(x)[1]))
      fcc <- dplyr::mutate(tableModel$fields, cc=cc[.data$name])
      wc <- dplyr::filter(fcc, .data$type!=.data$cc) %>% dplyr::pull("name")
      for (cn in wc) {
         toRet[, cn] <- ReDaMoR::as_type(
            dplyr::pull(toRet, !!cn),
            dplyr::filter(tableModel$fields, .data$name==!!cn) %>%
               dplyr::pull("type")
         )
      }
   }}
   return(toRet)
}

.dim_ch_mtable <- function(x, tablePath, tableModel){
   
   tp <- gsub("`", "", strsplit(tablePath, split="`[.]`")[[1]])
   tdb <- tp[1]
   tn <- tp[2]
   chTables <- list_tables(
      unclass(x)$tkcon$chcon, dbNames=tdb
   )
   chFields <- get_query(
      x,
      sprintf(
         paste(
            "SELECT database, table, name FROM system.columns",
            " WHERE database='%s'"
         ),
         tdb
      ),
      autoalias=FALSE,
      format="TabSeparatedWithNamesAndTypes"
   )
   
   if(ReDaMoR::is.MatrixModel(tableModel)){
      dbmt <- tablePath
      query <- "SELECT * FROM %s"
      tquery <- sprintf(query, dbmt)
      qr <- get_query(
         x, tquery, autoalias=FALSE,
         format="TabSeparatedWithNamesAndTypes"
      )
      
      if(.is_chMM(qr)){
         
         ## Sparse matrix ----
         
         nr <- chTables %>% 
            dplyr::filter(
               .data$database==tdb &
                  .data$name==qr$table[which(qr$info=="rows")]
            ) %>% 
            dplyr::pull("total_rows")
         nc <- chTables %>% 
            dplyr::filter(
               .data$database==tdb &
                  .data$name==qr$table[which(qr$info=="columns")]
            ) %>% 
            dplyr::pull("total_rows")
         toRet <- dplyr::tibble(
            format="MatrixMarket",
            ncol=nc,
            nrow=nr,
            records=as.numeric(nc) * as.numeric(nr),
            transposed=FALSE
         )
         
      }else{
         
         ## Matrix ----
      
         nr <- chTables %>% 
            dplyr::filter(
               .data$database==tdb & .data$name==qr$table[1]
            ) %>% 
            dplyr::pull("total_rows")
         tfields <- chFields %>% 
            dplyr::filter(
               .data$database==tdb,
               .data$table %in% qr$table
            )
         if("___ROWNAMES___" %in% tfields$name){
            if("___COLNAMES___" %in% tfields$name){
               stop(paste0(tn, ": Ambiguous matrix format"))
            }else{
               transposed=FALSE
            }
         }else{
            if("___COLNAMES___" %in% tfields$name){
               transposed=TRUE
            }else{
               stop(paste0(tn, ": Wrong matrix format"))
            }
         }
         nc <- nrow(tfields) - nrow(qr)
         
         toRet <- dplyr::tibble(
            format="matrix",
            ncol=ifelse(transposed, nr, nc),
            nrow=ifelse(transposed, nc, nr),
            records=as.numeric(nc) * as.numeric(nr),
            transposed=transposed
         )
      
      }
      
   }else{{
      
      ## Table ----
      
      toRet <- dplyr::tibble(
         format="table",
         ncol=length(tableModel),
         nrow=dplyr::filter(
            chTables,
            .data$database==tdb,
            .data$name==tn
         ) %>% dplyr::pull("total_rows")
      ) %>%
         dplyr::mutate(
            records=.data$nrow,
            transposed=FALSE
         )
   }}
   
   return(toRet)

}

.head_ch_mtable <- function(
   x,
   tablePath,
   tableModel,
   n=6L
){
   if(ReDaMoR::is.MatrixModel(tableModel)){
      dd <- .dim_ch_mtable(x, tablePath, tableModel)
      
      if(is.infinite(n)){
         nc <- dd$ncol
         nr <- dd$nrow
      }else{
         ncol <- dd$ncol
         mn <- min(floor(sqrt(n)), ncol)
         for(nc in mn:floor(mn/2)){
            if(n %% nc == 0){
               break()
            }
         }
         if(n %% nc == 0){
            nr <- n %/% nc
         }else{
            nr <- nc <- mn
         }
         if(nc * nr != n){
            warning(sprintf("Returning %s records", nc * nr))
         }
      }
      
      dbti <- tablePath
      query <- "SELECT * FROM %s"
      tdb <- sub("^`", "", sub("`[.]`.*$", "", dbti))
      tquery <- sprintf(query, dbti)
      qr <- get_query(
         x, tquery, autoalias=FALSE,
         format="TabSeparatedWithNamesAndTypes"
      )
      
      if(.is_chMM(qr)){
         
         ## Sparse matrix ----
         
         vquery <- sprintf(
            "SELECT i, j, x FROM %s WHERE i <= %s AND j <= %s ORDER BY j, i",
            sprintf('`%s`.`%s`', tdb, qr$table[which(qr$info=="values")]),
            nr, nc
         )
         values <- get_query(
            x, vquery, autoalias=FALSE,
            format="Arrow"
         )
         ## Sparse ==> all values may be missing
         if(nrow(values)==0){
            values <- dplyr::tibble(
               i=as.integer(nr),
               j=as.integer(nc),
               x=as.numeric(0)
            )
         }

         rquery <- sprintf(
            "SELECT i, name FROM %s WHERE i <= %s ORDER BY i",
            sprintf('`%s`.`%s`', tdb, qr$table[which(qr$info=="rows")]),
            nr
         )
         rowNames <- get_query(
            x, rquery, autoalias=FALSE,
            format="Arrow"
         )
         
         cquery <- sprintf(
            "SELECT j, name FROM %s WHERE j <= %s ORDER BY j",
            sprintf('`%s`.`%s`', tdb, qr$table[which(qr$info=="columns")]),
            nc
         )
         colNames <- get_query(
            x, cquery, autoalias=FALSE,
            format="Arrow"
         )
         
         mi <- max(rowNames$i)
         mj <- max(colNames$j)
         if(!mi %in% values$i || !mj %in% values$j){
            values <- rbind(
               values,
               dplyr::tibble(i=mi, j=mj, x=0)
            )
         }
         
         toRet <- Matrix::sparseMatrix(
            i=as.integer(values$i), j=as.integer(values$j), x=values$x,
            dimnames=list(rowNames$name, colNames$name)
         )
         
         return(Matrix::drop0(toRet))
         
      }else{
         
         ## Matrix ----
      
         vtype <- setdiff(
            tableModel$fields$type,
            c("row", "column")
         )
         dimcol <- ifelse(dd$transposed, "___COLNAMES___", "___ROWNAMES___")
         chFields <- get_query(
            x,
            sprintf(
               paste(
                  "SELECT database, table, name FROM system.columns",
                  " WHERE database='%s' AND table IN ('%s')"
               ),
               tdb,
               paste(unique(qr$table), collapse="', '")
            ),
            autoalias=FALSE,
            format="TabSeparatedWithNamesAndTypes"
         ) %>% 
            dplyr::filter(.data$name!=dimcol) %>% 
            dplyr::arrange(.data$name)
         if(!dd$transposed){
            chFields <- chFields[1:nc,]
            lim <- nr
         }else{
            chFields <- chFields[1:nr,]
            lim <- nc
         }
         toRet <- c()
         for(st in unique(chFields$table)){
            stquery <- sprintf(
               'SELECT `%s` FROM %s ORDER BY %s LIMIT %s',
               paste(
                  c(
                     dimcol,
                     chFields$name[which(chFields$table==st)]
                  ),
                  collapse='`, `'
               ),
               sprintf('`%s`.`%s`', tdb, st),
               dimcol,
               lim
            )
            stqr <- get_query(
               x, stquery, autoalias=FALSE,
               format="TabSeparatedWithNamesAndTypes"
            )
            dimname <- stqr[[dimcol]]
            stopifnot(
               !any(duplicated(dimname)),
               ncol(stqr) > 1
            )
            toAdd <- stqr[, -1, drop=FALSE] %>% 
               as.matrix()
            if(!vtype %in% c("Date", "POSIXct")){
               toAdd <- toAdd %>% magrittr::set_class(vtype)
            }
            toAdd <- toAdd %>% magrittr::set_rownames(dimname)
            toRet <- cbind(toRet, toAdd)
         }
         
         if(!dd$transposed){
            return(toRet[, sort(colnames(toRet))])
         }else{
            return(t(toRet[, sort(colnames(toRet))]))
         }
      
      }
      
   }else{{
      ## Table ----
      return(.get_ch_mtable(
         x, tablePath=tablePath, tableModel=tableModel, n_max=n
      ))
   }}
}

.ch_filtByConta <- function(d, fdb, fk, by=10^5){
   nfk <- fk
   dm <- data_model(fdb)
   .contaminate <- function(tn){
      
      ## Backward ----
      fkf <- fk %>% dplyr::filter(.data$from==!!tn & .data$fmin>0)
      fkt <- fk %>% dplyr::filter(.data$to==!!tn & .data$tmin>0)
      nfk <<- nfk %>%
         dplyr::anti_join(
            dplyr::select(fkf, "from", "to"), by=c("from", "to")
         ) %>% 
         dplyr::anti_join(
            dplyr::select(fkt, "from", "to"), by=c("from", "to")
         )
      fkl <- dplyr::bind_rows(
         fkf,
         fkt %>% dplyr::rename("from"="to", "ff"="tf", "to"="from", "tf"="ff")
      ) %>% 
         dplyr::distinct()
      if(nrow(fkl)>0){
         for(i in 1:nrow(fkl)){
            ntn <- fkl$to[i]
            if(ntn %in% names(d)){
               nv <- .mdjoin(
                  d1=d[[ntn]], d2=d[[tn]],
                  by=magrittr::set_names(
                     fkl$ff[[i]], fkl$tf[[i]]
                  ),
                  tm1=dm[[ntn]], tm2=dm[[tn]]
               )
            }else{
               if(ReDaMoR::is.MatrixModel(dm[[ntn]])){
                  dbti <- db_tables(fdb)$dbTables[[ntn]]
                  tdb <- sub("^`", "", sub("`[.]`.*$", "", dbti))
                  qr <- get_query(
                     fdb,
                     sprintf("SELECT * from %s", dbti),
                     autoalias=FALSE,
                     format="TabSeparatedWithNamesAndTypes"
                  )
               }else{
                  qr <- NULL
               }
               if(!is.null(qr) && .is_chMM(qr)){
                  fv <- lapply(1:length(fkl$tf[[i]]), function(j){
                     ntm <- dm[[ntn]]
                     ntf <- fkl$tf[[i]][[j]]
                     tm <- dm[[tn]]
                     tf <- fkl$ff[[i]][[j]]
                     if(inherits(d[[tn]], c("matrix", "Matrix"))){
                        if(
                           tm$fields$type[which(tm$fields$name==tf)] ==
                           "row"
                        ){
                           tv <- rownames(d[[tn]])
                        }else{
                           if(
                              tm$fields$type[which(tm$fields$name==ntf)] ==
                              "column"
                           ){
                              tv <- colnames(d[[tn]])
                           }else{
                              stop(sprintf(
                                 paste(
                                    "The field type of %s in the %s MM",
                                    "should be row or column"
                                 ),
                                 tf, tn
                              ))
                           }
                        }
                     }else{
                        tv <- d[[tn]][[tf]]
                     }
                     toRet <- list(tv)
                     names(toRet) <- ntf
                     return(toRet)
                  })
                  fv <- do.call(c, fv)
                  nv <- do.call(
                     filter_mdb_matrix,
                     c(
                        list(x=fdb, tableName=ntn),
                        fv
                     )
                  )
               }else{
                  nv <- c()
                  toAdd <- data_tables(
                     fdb, dplyr::all_of(ntn), skip=0, n_max=by
                  )[[1]]
                  r <- nrow(toAdd)
                  while(nrow(toAdd)>0){
                     toAdd <- .mdjoin(
                        d1=toAdd, d2=d[[tn]],
                        by=magrittr::set_names(
                           fkl$ff[[i]], fkl$tf[[i]]
                        ),
                        tm1=dm[[ntn]], tm2=dm[[tn]]
                     )
                     nv <- rbind(nv, toAdd)
                     toAdd <- data_tables(
                        fdb, dplyr::all_of(ntn), skip=r, n_max=by
                     )[[1]]
                     r <- r + nrow(toAdd)
                  }
               }
            }
            d[[ntn]] <<- nv
         }
      }
      
      ## Forward ----
      fkf <- fk %>% dplyr::filter(.data$from==!!tn & .data$fmin==0)
      fkt <- fk %>% dplyr::filter(.data$to==!!tn & .data$tmin==0)
      nfk <<- nfk %>%
         dplyr::anti_join(
            dplyr::select(fkf, "from", "to"), by=c("from", "to")
         ) %>% 
         dplyr::anti_join(dplyr::select(fkt, "from", "to"), by=c("from", "to"))
      fkl <- dplyr::bind_rows(
         fkf,
         fkt %>% dplyr::rename("from"="to", "ff"="tf", "to"="from", "tf"="ff")
      ) %>% 
         dplyr::distinct()
      if(nrow(fkl)>0){
         for(i in 1:nrow(fkl)){
            ntn <- fkl$to[i]

            if(ReDaMoR::is.MatrixModel(dm[[ntn]])){
               
               fv <- lapply(1:length(fkl$tf[[i]]), function(j){
                  ntm <- dm[[ntn]]
                  ntf <- fkl$tf[[i]][[j]]
                  tm <- dm[[tn]]
                  tf <- fkl$ff[[i]][[j]]
                  if(inherits(d[[tn]], c("matrix", "Matrix"))){
                     if(
                        tm$fields$type[which(tm$fields$name==tf)] ==
                        "row"
                     ){
                        tv <- rownames(d[[tn]])
                     }else{
                        if(
                           tm$fields$type[which(tm$fields$name==ntf)] ==
                           "column"
                        ){
                           tv <- colnames(d[[tn]])
                        }else{
                           stop(sprintf(
                              paste(
                                 "The field type of %s in the %s MM",
                                 "should be row or column"
                              ),
                              tf, tn
                           ))
                        }
                     }
                  }else{
                     tv <- d[[tn]][[tf]]
                  }
                  if(
                     ntm$fields$type[which(ntm$fields$name==ntf)] ==
                     "row"
                  ){
                     tv <- union(tv, rownames(d[[ntn]]))
                  }else{
                     tv <- union(tv, colnames(d[[ntn]]))
                  }
                  toRet <- list(tv)
                  names(toRet) <- ntf
                  return(toRet)
               })
               fv <- do.call(c, fv)
               
               d[[ntn]] <<- do.call(
                  filter_mdb_matrix,
                  c(
                     list(x=fdb, tableName=ntn),
                     fv
                  )
               )
               
            }else{            
            
               vta <- c()
               toAdd <- data_tables(
                  fdb, dplyr::all_of(ntn), skip=0, n_max=by
               )[[1]]
               r <- nrow(toAdd)
               while(nrow(toAdd)>0){
                  toAdd <- .mdjoin(
                     d1=toAdd, d2=d[[tn]],
                     by=magrittr::set_names(
                        fkl$ff[[i]], fkl$tf[[i]]
                     ),
                     tm1=dm[[ntn]], tm2=dm[[tn]]
                  )
                  vta <- rbind(vta, toAdd)
                  toAdd <- data_tables(
                     fdb, dplyr::all_of(ntn), skip=r, n_max=by
                  )[[1]]
                  r <- r + nrow(toAdd)
               }
               d[[ntn]] <<- rbind(
                  d[[ntn]],
                  vta
               ) %>%
                  dplyr::distinct()
               
            }
         }
      }
   }
   for(tn in names(d)){
      if(!is.null(fk)){
         .contaminate(tn)
      }
   }
   if(!is.null(fk) && nrow(fk) > nrow(nfk)){
      d <- .ch_filtByConta(d, fdb, nfk)
   }
   return(d)
}

