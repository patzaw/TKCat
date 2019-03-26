addMDBToChTKCat <- function(
   tkcon,
   imdb
){
   
   check_chTKCat(tkcon)
   stopifnot(is.internalMDB(imdb))
   
   dbInfo <- dbInfo(imdb)
   
   ## Existing DB ----
   curDB <- dbGetQuery(
      tkcon$chcon,
      sprintf(
         "SELECT * FROM default.MDB WHERE name='%s'",
         dbInfo$name
      )
   )
   if(nrow(curDB)>0){
      print(tkcon)
      stop(sprintf(
         "The %s database is already in this chTKCat",
         dbInfo$name
      ))
   }
   
   ## Create the database ----
   dbSendQuery(
      tkcon$chcon,
      sprintf(
         "CREATE DATABASE `%s`",
         dbInfo$name
      )
   )
   
   ## Add the data model ----
   rdbm <- dataModel(imdb)
   dbm <- toDBM(rdbm)
   writeMergeTree(
      con=tkcon$chcon,
      dbName=dbInfo$name,
      tableName="___tables___",
      value=dbm$tables,
      rtypes=dbm$tables %>%
         summarise_all(function(x)class(x)[1]) %>% unlist(),
      nullable=c("x", "y", "color", "comment"),
      indexes=c("name")
   )
   writeMergeTree(
      con=tkcon$chcon,
      dbName=dbInfo$name,
      tableName="___fields___",
      value=dbm$fields,
      rtypes=dbm$fields %>%
         summarise_all(function(x)class(x)[1]) %>% unlist(),
      nullable=NULL,
      indexes=c("table", "name")
   )
   writeMergeTree(
      con=tkcon$chcon,
      dbName=dbInfo$name,
      tableName="___primaryKeys___",
      value=dbm$primaryKeys,
      rtypes=dbm$primaryKeys %>%
         summarise_all(function(x)class(x)[1]) %>% unlist(),
      nullable=NULL,
      indexes=c("table", "field")
   )
   writeMergeTree(
      con=tkcon$chcon,
      dbName=dbInfo$name,
      tableName="___foreignKeys___",
      value=dbm$foreignKeys,
      rtypes=dbm$foreignKeys %>%
         summarise_all(function(x)class(x)[1]) %>% unlist(),
      nullable=NULL,
      indexes=c("table", "field", "refTable", "refField")
   )
   writeMergeTree(
      con=tkcon$chcon,
      dbName=dbInfo$name,
      tableName="___indexes___",
      value=dbm$indexes,
      rtypes=dbm$indexes %>%
         summarise_all(function(x)class(x)[1]) %>% unlist(),
      nullable=NULL,
      indexes=c("table", "field")
   )
   
   ## Add the data ----
   for(tn in names(rdbm)){
      rtypes <- rdbm[[tn]]$fields$type
      names(rtypes) <- rdbm[[tn]]$fields$name
      writeMergeTree(
         con=tkcon$chcon,
         dbName=dbInfo$name,
         tableName=tn,
         value=imdb[[tn]],
         rtypes=rtypes,
         nullable=rdbm[[tn]]$fields$name[which(rdbm[[tn]]$fields$nullable)],
         indexes=unique(indexTable(rdbm[[tn]])$field)
      )
   }
   
   ## Add database to the listing ----
   .dbinsert(
      tkcon$chcon,
      dbName="default", tableName="MDB",
      value=as_tibble(dbInfo) %>%
         select(colnames(curDB))
   )
   
}

writeMergeTree <- function(
   conn,
   dbName,
   tableName,
   value,
   rtypes,
   nullable=NULL,
   indexes=NULL
){
   stopifnot(
      inherits(conn, "ClickhouseConnection"),
      is.character(dbName), length(dbName)==1,
      is.character(tableName), length(tableName)==1,
      is.data.frame(value),
      all(names(rtypes) %in% colnames(value)),
      all(colnames(value) %in% names(rtypes)),
      all(nullable %in% colnames(value)),
      all(indexes %in% colnames(value))
      # !any(indexes %in% nullable)
   )
   
   if(length(indexes)==0){
      indexes <- setdiff(colnames(value), nullable)[1]
   }else{
      indexes <- setdiff(indexes, nullable)
   }
   
   chtypes <- typeRefConv(rtypes, to="ClickHouse")
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
   if(length(indexes) > 0){
      tst <- paste(
         tst,
         sprintf(
            "ORDER BY (`%s`)",
            paste(indexes, collapse="`, `")
         )
      )
   }
   
   dbSendQuery(
      conn=conn,
      statement=tst
   )
   
   ct <- dbGetQuery(
      conn=conn,
      statement=sprintf("SELECT * FROM `%s`.`%s`", dbName, tableName)
   )
   
   .dbinsert(conn, dbName, tableName, value)
   
}

.dbinsert <- function(
   conn,
   dbName,
   tableName,
   value,
   by=10^6
){
   
   stopifnot(
      is.character(dbName), length(dbName)==1,
      is.character(tableName), length(tableName)==1
   )
   
   qname <- SQL(paste(
      dbQuoteIdentifier(conn, dbName),
      dbQuoteIdentifier(conn, tableName),
      sep="."
   ))
   
   stopifnot(
      tableName %in% dbGetQuery(
         conn,
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
         levels(value[[c]]) <- .Internal(setEncoding(levels(value[[c]]), "UTF-8"))
      }
      s <- by*(0:(nrow(value)%/%by))
      e <- c(s[-1], nrow(value))
      s <- s+1
      s <- s[which(!duplicated(e))]
      e <- e[which(!duplicated(e))]
      for(i in 1:length(s)){
         em <- try(
            RClickhouse:::insert(conn@ptr, qname, value[s[i]:e[i],]),
            silent=TRUE
         )
         if(inherits(em, "try-error")){
            print(qname)
            stop(em)
         }
      }
   }
}

rmMDBFromChTKCat <- function(
   tkcon,
   name
){
   try(dbSendQuery(tkcon$chcon, sprintf("drop database `%s`", name)))
   try(dbSendQuery(
      tkcon$chcon,
      sprintf("alter table default.MDB DELETE where name='%s'", name)
   ))
}
