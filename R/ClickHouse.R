###############################################################################@
#'
#' @export
#'
db_disconnect.DBIConnection <- function(x){
   DBI::dbDisconnect(x)
   invisible()
}


###############################################################################@
#' 
#' @rdname get_hosts
#' @method get_hosts DBIConnection
#' 
#' @export
#'
get_hosts.DBIConnection <- function(x, ...){
   paste(x@host, x@port, sep=":")
}


###############################################################################@
#' 
#' @param dbNames the name of databases to focus on (default NULL ==> all)
#'
#' @rdname list_tables
#' @method list_tables DBIConnection
#' 
#' @export
#'
list_tables.DBIConnection <- function(
   x, dbNames=NULL, ...
){
   stopifnot(
      length(dbNames)==0 || is.character(dbNames) & all(!is.na(dbNames))
   )
   if(length(dbNames)>0){
      fquery <- sprintf(
         "WHERE database IN ('%s')",
         paste(dbNames, collapse="', '")
      )
   }else{
      fquery <- ""
   }
   query <- paste(
      "SELECT * FROM",
      "(",
      "SELECT database, name as table, total_rows, total_bytes",
      "FROM system.tables",
      fquery,
      ") AS q1",
      "LEFT JOIN",
      "(",
      "SELECT database, table, count() as total_columns",
      ", sum(name='___COLNAMES___') as transposed",
      "FROM system.columns",
      "GROUP BY database, table",
      ") AS q2",
      "USING database, table"
   )
   toRet <- dplyr::as_tibble(DBI::dbGetQuery(
      x, query, format="TabSeparatedWithNamesAndTypes"
   )) %>% 
      dplyr::rename("name"="table") %>% 
      dplyr::mutate(
         total_rows=as.numeric(.data$total_rows),
         total_bytes=as.numeric(.data$total_bytes),
         total_columns=as.numeric(.data$total_columns),
         transposed=as.logical(.data$transposed)
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
#' @param lowCardinality a character vector indicating the name of the columns
#' with low cardinality (default: NULL)
#' @param sortKey a character vector indicating the name of the columns
#' used in the sort key. If NULL (default), all the non-nullable columns
#' are used in the key.
#' @param indexes a data.frame with 3 columns:
#' - idx: index name,
#' - field: field name,
#' - type: 'bloom_filter(0.01)', 'minmax'...
#' (see https://clickhouse.com/docs/optimize/skipping-indexes)
#' - granularity: index granularity
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
   lowCardinality=NULL,
   sortKey=NULL,
   indexes=NULL
){
   stopifnot(
      inherits(con, "DBIConnection"),
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
   
   chtypes <- ReDaMoR::conv_type_ref(rtypes, to="ClickHouse")
  

   chtypes <- ifelse(
      names(rtypes) %in% nullable & chtypes!="Array(String)",
      sprintf("Nullable(%s)", chtypes),
      chtypes
   )
  
  chtypes <- ifelse(
   names(rtypes) %in% lowCardinality,
   sprintf("LowCardinality(%s)", chtypes),
   chtypes
  )
   names(chtypes) <- names(rtypes)
   
  
  if(!is.null(indexes) && nrow(indexes) > 0){
    idxq <- paste(
         sprintf(
            ',\nINDEX %s (%s) TYPE %s GRANULARITY %s',
            indexes$idx, indexes$field, indexes$type, indexes$granularity
         ),
         collapse = ""
      )
   }else{
    idxq <- ""
  }
  
   tst <- paste0(
      sprintf("CREATE TABLE `%s`.`%s` (\n", dbName, tableName),
      paste(unlist(lapply(
         colnames(value),
         function(cn){
            toRet <- sprintf(
               "`%s` %s",
               cn,
               chtypes[cn]
            )
            return(toRet)
         }
      )), collapse=",\n"),
      idxq,
      "\n) ENGINE = MergeTree()"
   )
   if(length(sortKey) > 0){
      tst <- paste(
         tst,
         sprintf(
            "ORDER BY (`%s`)",
            paste(sortKey, collapse="`, `")
         ),
         sep = "\n"
      )
   }else{
      tst <- paste(
         tst,
         "ORDER BY tuple()",
         sep= "\n"
      )
   }
   
   DBI::dbSendQuery(con, tst)
   
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
      inherits(con, "DBIConnection"),
      is.character(dbName), length(dbName)==1,
      is.character(tableName), length(tableName)==1,
      is.data.frame(value),
      tableName %in% list_tables(con, dbName)$name
   )
   
   qname <- DBI::SQL(paste(
      DBI::dbQuoteIdentifier(con, dbName),
      DBI::dbQuoteIdentifier(con, tableName),
      sep="."
   ))
   
   if(!is.na(con@session)){
      DBI::dbSendQuery(con, sprintf("USE `%s`", dbName))
      on.exit(DBI::dbSendQuery(con, "USE default"))
   }
   
   if(nrow(value)>0){
      fo <- DBI::dbGetQuery(
         con, sprintf("SELECT * FROM %s LIMIT 0", qname),
         format="TabSeparatedWithNamesAndTypes"
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
               database = dbName,
               value=dplyr::slice(value, s[!!i]:e[!!i]),
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
      inherits(con, "DBIConnection"),
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
      inherits(con, "DBIConnection"),
      is.character(dbName), length(dbName)==1, !is.na(dbName),
      ReDaMoR::is.RelTableModel(tm)
   )
   if(ReDaMoR::is.MatrixModel(tm)){
      write_MergeTree(
         con=con,
         dbName=dbName,
         tableName=tm$tableName,
         value=dplyr::tibble(
            table=character(),
            info=character()
         ),
         rtypes=c("table"="character", "info"="character"),
         nullable=NULL,
         sortKey="table"
      )
   }else{
      rtypes <- tm$fields$type
      names(rtypes) <- tm$fields$name
     nullable <- tm$fields %>%
            dplyr::filter(.data$nullable) %>%
            dplyr::pull("name")
     lowCardinality <- tm$fields %>%
       dplyr::filter(
         .data$type %in% c("character"),
         stringr::str_detect(.data$comment, stringr::fixed('{ch_LowCardinality}'))
      ) %>%
       dplyr::pull("name")
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
         nullable=nullable,
         lowCardinality = lowCardinality,
         sortKey=.get_tm_sortKey(tm),
         indexes=.get_tm_indexes(tm)
      ) 
   }
   invisible()
}


###############################################################################@
## Helpers ----

.get_tm_sortKey <- function(
   tm, # a [ReDaMoR::RelTableModel] object
   quoted=FALSE, # if TRUE, returns a single character value CH compatible
                 # if FALSE, returns a vector of character
   nsc=5         # Maximum number of columns to use for sorting
){
   # # By default: sort by primary key
   # if(length(tm$primaryKey)>0){
   #    toRet <- tm$primaryKey
   # }else{
   #    it <- ReDaMoR::index_table(tm)
   #    if(!is.null(it) && nrow(it)>0){
   #       uit <- dplyr::filter(it, .data$uniqueIndex)
         
   #       # If no primary key, sort by the first unique index
   #       if(nrow(uit)>0){
   #          toRet <- dplyr::filter(uit, .data$index==min(uit$index)) %>% 
   #             dplyr::pull("field")
   #       }else{
            
   #          # If no unique index, sort by index and the remaining columns
   #          toRet <- unique(c(it$field, tm$fields$name))
   #       }
   #    }else{
         
   #       # If no index, sort by nsc first columns
   #       toRet <- tm$fields$name[1:min(nsc, nrow(tm$fields))]
   #    }
   # }
  
   nullable <- tm$fields %>%
      dplyr::filter(.data$nullable) %>%
      dplyr::pull("name")
  
  toRet <- c()
  
  indexes <- ReDaMoR::index_table(tm)
  if(length(toRet) == 0 && 1 %in% indexes$index){
    toRet <- indexes %>%
      dplyr::filter(.data$index == 1) %>%
      dplyr::pull("field")
  }
  if(length(toRet) == 0 && 0 %in% indexes$index){
    toRet <- indexes %>%
      dplyr::filter(.data$index == 0) %>%
      dplyr::pull("field")
  }

  foreign_keys <- ReDaMoR::get_foreign_keys(tm)
  if(length(toRet) == 0 && !is.null(foreign_keys) && nrow(foreign_keys) > 0){
    toRet <- unique(unlist(foreign_keys$ff))
  }

   if(length(toRet) == 0){
    toRet <- tm$fields %>%
      dplyr::filter(!.data$nullable) %>%
      dplyr::pull("name") %>%
      head(1)
  }
  
  toRet <- setdiff(toRet, c(NA, nullable))

  if(length(toRet) > nsc){
    toRet <- toRet[1:nsc]
  }
  
   if(quoted){
      toRet <- paste(sprintf("`%s`", toRet), collapse=", ")
   }
   return(toRet)
}

.get_tm_indexes <- function(
   tm # a [ReDaMoR::RelTableModel] object
){
  indexes <- ReDaMoR::index_table(tm)
  if(all(indexes$index <= 1)){
    return(NULL)
  }

  toRet <- c()
  ifields <- indexes %>%
    dplyr::filter(.data$index > 1) %>%
    dplyr::arrange(.data$index) %>%
    dplyr::distinct(.data$index, .keep_all = TRUE) %>%
    dplyr::pull("field") %>%
    unique()
  for(f in ifields){
    if(tm$fields$type[which(tm$fields$name==f)] == "character"){
      toRet <- dplyr::bind_rows(
         toRet,
         dplyr::tibble(
            idx = paste0("idx_", f),
            field = f,
            type = "bloom_filter(0.01)",
            granularity = 1
         )
      )
    }
    if(tm$fields$type[which(tm$fields$name==f)] %in% c("numeric", "integer")){
      toRet <- dplyr::bind_rows(
         toRet,
         dplyr::tibble(
            idx = paste0("idx_", f),
            field = f,
            type = "minmax",
            granularity = 1
         )
      )
    }
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
  