###############################################################################@
#'
#' @export
#'
db_disconnect.ClickhouseConnection <- function(x){
   RClickhouse::dbDisconnect(x)
   invisible()
}


###############################################################################@
#'
#' @rdname db_reconnect
#' @method db_reconnect ClickhouseConnection
#' 
#' @param settings list of
#' [Clickhouse settings](https://clickhouse.com/docs/en/operations/settings/settings/)
#'
#' @export
#'
db_reconnect.ClickhouseConnection <- function(
   x, user, password, ntries=3, settings=list(), ...
){
   xn <- deparse(substitute(x))
   if(missing(user)){
      user <- x@user
   }
   suppressWarnings(try(RClickhouse::dbDisconnect(x), silent=TRUE))
   if(missing(password)){
      nv <- try(RClickhouse::dbConnect(
         drv=RClickhouse::clickhouse(),
         host=x@host,
         port=x@port,
         user=user
      ), silent=TRUE)
      n <- 0
      while(inherits(nv, "try-error") & n < ntries){
         password <- getPass::getPass(
            msg=paste0(user, " password on ", x@host, ":", x@port)
         )
         if(is.null(password)){
            stop("Canceled by the user")
         }
         nv <- try(RClickhouse::dbConnect(
            drv=RClickhouse::clickhouse(),
            host=x@host,
            port=x@port,
            user=user, password=password
         ), silent=TRUE)
         n <- n+1
      }
      if(inherits(nv, "try-error")){
         stop(as.character(nv))
      }
   }else{
      nv <- RClickhouse::dbConnect(
         drv=RClickhouse::clickhouse(),
         host=x@host,
         port=x@port,
         user=user, password=password
      )
   }
   for(s in names(settings)){
      RClickhouse::dbSendQuery(nv, sprintf("SET %s='%s'", s, settings[[s]]))
   }
   assign(xn, nv, envir=parent.frame(n=1))
}


###############################################################################@
#' 
#' @rdname get_hosts
#' @method get_hosts ClickhouseConnection
#' 
#' @export
#'
get_hosts.ClickhouseConnection <- function(x, ...){
   paste(x@host, x@port, sep=":")
}


###############################################################################@
#' List tables in a clickhouse database
#' 
#' @param con the clickhouse connection
#' @param dbNames the name of databases to focus on (default NULL ==> all)
#' 
#' @return A tibble with the following columns:
#' - **database**: the name of the database
#' - **name**: the name of the table
#' - **total_rows**: the number of rows in the table
#' - **total_bytes**: the size of the table
#' 
#' @export
#' 
list_tables <- function(
   con, dbNames=NULL
){
   stopifnot(
      inherits(con, "ClickhouseConnection"),
      length(dbNames)==0 || is.character(dbNames) & all(!is.na(dbNames))
   )
   query <- paste(
      "SELECT database, name, total_rows, total_bytes",
      "FROM system.tables"
   )
   if(length(dbNames)>0){
      query <- paste(
         query,
         sprintf("WHERE database IN ('%s')", paste(dbNames, collapse="', '"))
      )
   }
   toRet <- dplyr::as_tibble(DBI::dbGetQuery(con, query)) %>% 
      dplyr::mutate(
         total_rows=as.numeric(.data$total_rows),
         total_bytes=as.numeric(.data$total_bytes)
      )
   return(toRet)
}


###############################################################################@
#' Write a Clickhouse
#' [MergeTree](https://clickhouse.com/docs/en/engines/table-engines/mergetree-family/mergetree/)
#' table
#' 
#' @param con the clickhouse connection
#' @param dbName the name of the database
#' @param tableName the name of the table
#' @param value the table to import
#' @param rtypes a named character vector giving the R type of each and every
#' columns. If NULL (default), types are guessed from value.
#' @param nullable a character vector indicating the name of the columns
#' which are nullable (default: NULL)
#' @param sortKey a character vector indicating the name of the columns
#' used in the sort key. If NULL (default), all the non-nullable columns
#' are used in the key.
#' 
#' @return No return value, called for side effects
#' 
#' @export
#' 
write_MergeTree <- function(
   con,
   dbName,
   tableName,
   value,
   rtypes=NULL,
   nullable=NULL,
   sortKey=NULL
){
   stopifnot(
      inherits(con, "ClickhouseConnection"),
      is.character(dbName), length(dbName)==1, !is.na(dbName),
      is.character(tableName), length(tableName)==1, !is.na(tableName),
      is.data.frame(value),
      all(nullable %in% colnames(value)),
      all(sortKey %in% colnames(value))
   )
   
   if(is.null(rtypes)){
      rtypes <- c()
      for(cn in colnames(value)){
         rtypes[cn] <- class(dplyr::pull(value, !!cn))[1]
      }
   }
   stopifnot(
      all(names(rtypes) %in% colnames(value)),
      all(colnames(value) %in% names(rtypes))
   )
   
   if(length(sortKey)==0){
      sortKey <- setdiff(colnames(value), nullable)[1]
   }else{
      sortKey <- setdiff(sortKey, nullable)
   }
   
   chtypes <- ReDaMoR::conv_type_ref(rtypes, to="ClickHouse")
   names(chtypes) <- names(rtypes)
   
   tst <- paste(
      sprintf("CREATE TABLE `%s`.`%s` (", dbName, tableName),
      paste(unlist(lapply(
         colnames(value),
         function(cn){
            toRet <- sprintf(
               "`%s` %s",
               cn,
               ifelse(
                  cn %in% nullable & chtypes[cn]!="Array(String)",
                  sprintf("Nullable(%s)", chtypes[cn]),
                  chtypes[cn]
               )
            )
            return(toRet)
         }
      )), collapse=",\n"),
      ") ENGINE = MergeTree()"
   )
   if(length(sortKey) > 0){
      tst <- paste(
         tst,
         sprintf(
            "ORDER BY (`%s`)",
            paste(sortKey, collapse="`, `")
         )
      )
   }else{
      tst <- paste(
         tst,
         "ORDER BY tuple()"
      )
   }
   
   RClickhouse::dbSendQuery(con, tst)
   
   ch_insert(con, dbName, tableName, value)
   
   invisible()
   
}

###############################################################################@
#' Insert records by batches in a Clickhouse table
#' 
#' @param con the clickhouse connection
#' @param dbName the name of the database
#' @param tableName the name of the table
#' @param value the table to import
#' @param by the size of the batch: number of records to import
#' together (default: 10^6)
#' 
#' @return No return value, called for side effects
#' 
#' @export
#' 
ch_insert <- function(
   con,
   dbName,
   tableName,
   value,
   by=10^6
){
   
   stopifnot(
      inherits(con, "ClickhouseConnection"),
      is.character(dbName), length(dbName)==1,
      is.character(tableName), length(tableName)==1,
      is.data.frame(value),
      tableName %in% list_tables(con, dbName)$name
   )
   
   qname <- DBI::SQL(paste(
      RClickhouse::dbQuoteIdentifier(con, dbName),
      RClickhouse::dbQuoteIdentifier(con, tableName),
      sep="."
   ))
   RClickhouse::dbSendQuery(con, sprintf("USE `%s`", dbName))
   on.exit(RClickhouse::dbSendQuery(con, "USE default"))
   
   if(nrow(value)>0){
      fo <- DBI::dbGetQuery(
         con, sprintf("SELECT * FROM %s LIMIT 1", qname)
      ) %>% 
         colnames()
      if(!all(colnames(value) %in% fo)){
         stop(
            "Some fields in value are not available in the table: ",
            paste(setdiff(colnames(value), fo), collapse=", ")
         )
      }
      value <- dplyr::select(value, dplyr::all_of(fo))
      s <- by*(0:(nrow(value)%/%by))
      e <- c(s[-1], nrow(value))
      s <- s+1
      s <- s[which(!duplicated(e))]
      e <- e[which(!duplicated(e))]
      for(i in 1:length(s)){
         em <- try(
            DBI::dbAppendTable(
               conn=con,
               name=tableName, #qname,
               value=dplyr::slice(value, s[i]:e[i]),
               row.names=FALSE
               # append=TRUE
            ),
            silent=TRUE
         )
         if(inherits(em, "try-error")){
            print(qname)
            stop(em)
         }
      }
   }
   invisible()
}

###############################################################################@
#' Create ClickHouse MergeTree tables from a [ReDaMoR::RelDataModel]
#' 
#' @param con the clickhouse connection
#' @param dbName the name of the database in which the tables should be written
#' @param dbm a [ReDaMoR::RelDataModel] object
#' 
#' @return No return value, called for side effects
#' 
#' @export
#' 
#' 
mergeTrees_from_RelDataModel <- function(
   con, dbName, dbm
){
   stopifnot(
      inherits(con, "ClickhouseConnection"),
      is.character(dbName), length(dbName)==1, !is.na(dbName),
      ReDaMoR::is.RelDataModel(dbm)
   )
   for(tn in names(dbm)){
      mergeTree_from_RelTableModel(con, dbName, dbm[[tn]])
   }
   invisible()
}

###############################################################################@
#' Create a ClickHouse MergeTree table from a [ReDaMoR::RelTableModel]
#' 
#' @param con the clickhouse connection
#' @param dbName the name of the database in which the table should be written
#' @param tm a [ReDaMoR::RelTableModel] object
#' 
#' @return No return value, called for side effects
#' 
#' @export
#' 
mergeTree_from_RelTableModel <- function(
   con, dbName, tm
){
   stopifnot(
      inherits(con, "ClickhouseConnection"),
      is.character(dbName), length(dbName)==1, !is.na(dbName),
      ReDaMoR::is.RelTableModel(tm)
   )
   if(ReDaMoR::is.MatrixModel(tm)){
      write_MergeTree(
         con=con,
         dbName=dbName,
         tableName=tm$tableName,
         value=dplyr::tibble(
            table=character()
         ),
         rtypes=c("table"="character"),
         nullable=NULL,
         sortKey="table"
      )
   }else{
      rtypes <- tm$fields$type
      names(rtypes) <- tm$fields$name
      value <- dplyr::tibble()
      for(i in 1:nrow(tm$fields)){
         toAdd <- integer()
         class(toAdd) <- tm$fields$type[i]
         value[,tm$fields$name[i]] <- toAdd
      }
      write_MergeTree(
         con=con,
         dbName=dbName,
         tableName=tm$tableName,
         value=value,
         rtypes=rtypes,
         nullable=tm$fields %>%
            dplyr::filter(.data$nullable) %>%
            dplyr::pull("name"),
         sortKey=.get_tm_sortKey(tm)
      ) 
   }
   invisible()
}


###############################################################################@
## Helpers ----

.get_tm_sortKey <- function(
   tm, # a [ReDaMoR::RelTableModel] object
   quoted=FALSE # if TRUE, returns a single character value CH compatible
                # if FALSE, returns a vector of character
){
   # By default: sort by primary key
   if(length(tm$primaryKey)>0){
      toRet <- tm$primaryKey
   }else{
      it <- ReDaMoR::index_table(tm)
      if(!is.null(it) && nrow(it)>0){
         uit <- dplyr::filter(it, .data$uniqueIndex)
         
         # If no primary key, sort by the first unique index
         if(nrow(uit)>0){
            toRet <- dplyr::filter(uit, .data$index==min(uit$index)) %>% 
               dplyr::pull("field")
         }else{
            
            # If no unique index, sort by index and the remaining columns
            toRet <- unique(c(it$field, tm$fields$name))
         }
      }else{
         
         # If no index, sort by all columns
         toRet <- tm$fields$name
      }
   }
   if(quoted){
      toRet <- paste(sprintf("`%s`", toRet), collapse=", ")
   }
   return(toRet)
}

###############################################################################@
## ClickHouse statements for DB access ----
CH_DB_STATEMENTS <- c(
   "ALTER", "CREATE DATABASE", "CREATE DICTIONARY", "CREATE TABLE",
   "CREATE VIEW", "DROP", "INSERT", "OPTIMIZE", "SELECT",
   "SHOW DICTIONARIES", "SYSTEM FETCHES", "SYSTEM FLUSH DISTRIBUTED",
   "SYSTEM MERGES", "SYSTEM MOVES", "SYSTEM REPLICATION QUEUES",
   "SYSTEM RESTART REPLICA", "SYSTEM SENDS", "SYSTEM SYNC REPLICA",
   "SYSTEM TTL MERGES", "TRUNCATE", "dictGet"
)

###############################################################################@
## Reserved databases (not available for the user) ----
CH_RESERVED_DB <- c(
   "default",
   "system",
   "_temporary_and_external_tables"
)

###############################################################################@
## Maximum number of column allowed in a ClikHouse table ----
CH_MAX_COL <- 1000

###############################################################################@
## Maximum length of base64 data ----
CH_DOC_CHUNK <- 10^6
