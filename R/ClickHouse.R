###############################################################################@
#' Reconnect to a ClickHouse database
#' 
#' @param x a ClickhouseConnection object
#' @param user user name. If not provided, it's taken from x
#' @param password user password. If not provided, first the function
#' tries to connect without any password.If it fails, the function asks the
#' user to provide a password.
#' @param ntries the number of times the user can enter a wrong password
#' 
#' @importFrom RClickhouse dbConnect clickhouse dbDisconnect
#' @importFrom getPass getPass
#' 
#' @export
#' 
ch_reconnect <- function(x, user, password, ntries=3){
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
         password <- getPass::getPass(msg=paste(user, "password"))
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
   assign(xn, nv, envir=parent.frame(n=1))
}


###############################################################################@
#' Write a Clickhouse
#' [MergeTree](https://clickhouse.yandex/docs/en/operations/table_engines/mergetree/)
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
#' @importFrom ReDaMoR conv_type_ref
#' @importFrom RClickhouse dbSendQuery
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
         rtypes[cn] <- class(pull(value, !!cn))[1]
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
                  cn %in% nullable,
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
   }
   
   RClickhouse::dbSendQuery(con, tst)
   
   ch_insert(con, dbName, tableName, value)
   
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
#' @importFrom RClickhouse dbQuoteIdentifier
#' @importFrom DBI dbGetQuery
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
      is.data.frame(value)
   )
   
   qname <- SQL(paste(
      RClickhouse::dbQuoteIdentifier(con, dbName),
      RClickhouse::dbQuoteIdentifier(con, tableName),
      sep="."
   ))
   
   stopifnot(
      tableName %in% DBI::dbGetQuery(
         con,
         sprintf("SELECT name FROM system.tables WHERE database='%s'", dbName)
      )$name,
      is.data.frame(value)
   )
   
   if(nrow(value)>0){
      classes <- unlist(lapply(value, function(v){
         class(v)[[1]]
      }))
      for (c in names(classes[classes=="character"])) {
         value[[c]] <- .Internal(setEncoding(value[[c]], "UTF-8"))
      }
      for (c in names(classes[classes=="factor"])) {
         levels(value[[c]]) <- .Internal(setEncoding(
            levels(value[[c]]), "UTF-8"
         ))
      }
      s <- by*(0:(nrow(value)%/%by))
      e <- c(s[-1], nrow(value))
      s <- s+1
      s <- s[which(!duplicated(e))]
      e <- e[which(!duplicated(e))]
      for(i in 1:length(s)){
         em <- try(
            RClickhouse:::insert(con@ptr, qname, value[s[i]:e[i],,drop=FALSE]),
            silent=TRUE
         )
         if(inherits(em, "try-error")){
            print(qname)
            stop(em)
         }
      }
   }
}

###############################################################################@
#' Create ClickHouse MergeTree tables from a [ReDaMoR::RelDataModel]
#' 
#' @param con the clickhouse connection
#' @param dbName the name of the database in which the tables should be written
#' @param dbm a [ReDaMoR::RelDataModel] object
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
      is.RelDataModel(dbm)
   )
   for(tn in names(dbm)){
      mergeTree_from_RelTableModel(con, dbName, dbm[[tn]])
   }
}

###############################################################################@
#' Create a ClickHouse MergeTree table from a [ReDaMoR::RelTableModel]
#' 
#' @param con the clickhouse connection
#' @param dbName the name of the database in which the table should be written
#' @param tm a [ReDaMoR::RelTableModel] object
#' 
#' @export
#' 
mergeTree_from_RelTableModel <- function(
   con, dbName, tm
){
   stopifnot(
      inherits(con, "ClickhouseConnection"),
      is.character(dbName), length(dbName)==1, !is.na(dbName),
      is.RelTableModel(tm)
   )
   rtypes <- tm$fields$type
   names(rtypes) <- tm$fields$name
   value <- tibble()
   for(i in 1:nrow(tm$fields)){
      toAdd <- character()
      class(toAdd) <- tm$fields$type[i]
      value[,tm$fields$name[i]] <- toAdd
   }
   write_MergeTree(
      con=con,
      dbName=dbName,
      tableName=tm$tableName,
      value=value,
      rtypes=rtypes,
      nullable=tm$fields %>% filter(nullable) %>% pull(name),
      sortKey=tm$primaryKey
   )
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
