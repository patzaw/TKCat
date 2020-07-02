###############################################################################@
#' Add an MDB (modeled database) to a [chTKCat] database
#' 
#' @param tkcon a [chTKCat] object
#' @param imdb an [internalMDB] object
#' 
add_chMDB <- function(
   tkcon,
   imdb
){
   
   check_chTKCat(tkcon)
   stopifnot(is.internalMDB(imdb))
   
   dbInfo <- dbInfo(imdb, countRecords=FALSE)
   
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
   
   ## Collections ----
   cm <- collectionMembers(imdb)
   if(!is.null(cm) && nrow(cm)>0){
      rcol <- unique(cm$collection)
      locCol <- tkcatEnv$COLLECTIONS
      missingCol <- setdiff(rcol, locCol$title)
      if(length(missingCol)>0){
         stop(
            "The following collection are not available locally: ",
            paste(missing, sep=", ")
         )
      }
      chCol <- dbGetQuery(
         conn=tkcon$chcon,
         statement="SELECT * FROM default.Collections"
      )
      alreadyIn <- intersect(rcol, chCol$title)
      for(ct in alreadyIn){
         if(
            locCol %>% filter(title==ct) %>% pull(json) != 
            chCol %>% filter(title==ct) %>% pull(json)
         ){
            stop(sprintf("Incompatible collection definition for %s", ct))
         }
      }
      newCol <- setdiff(rcol, chCol$title)
      for(ct in newCol){
         addChTKCatCollection(
            tkcon=tkcon,
            json=ct
         )
      }
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
   er <- try({
      rdbm <- dataModel(imdb)
      dbm <- ReDaMoR::toDBM(rdbm)
      write_MergeTree(
         con=tkcon$chcon,
         dbName=dbInfo$name,
         tableName="___tables___",
         value=dbm$tables,
         rtypes=dbm$tables %>%
            summarise_all(function(x)class(x)[1]) %>% unlist(),
         nullable=c("x", "y", "color", "comment"),
         indexes=c("name")
      )
      write_MergeTree(
         con=tkcon$chcon,
         dbName=dbInfo$name,
         tableName="___fields___",
         value=dbm$fields,
         rtypes=dbm$fields %>%
            summarise_all(function(x)class(x)[1]) %>% unlist(),
         nullable=NULL,
         indexes=c("table", "name")
      )
      write_MergeTree(
         con=tkcon$chcon,
         dbName=dbInfo$name,
         tableName="___primaryKeys___",
         value=dbm$primaryKeys,
         rtypes=dbm$primaryKeys %>%
            summarise_all(function(x)class(x)[1]) %>% unlist(),
         nullable=NULL,
         indexes=c("table", "field")
      )
      write_MergeTree(
         con=tkcon$chcon,
         dbName=dbInfo$name,
         tableName="___foreignKeys___",
         value=dbm$foreignKeys,
         rtypes=dbm$foreignKeys %>%
            summarise_all(function(x)class(x)[1]) %>% unlist(),
         nullable=NULL,
         indexes=c("table", "field", "refTable", "refField")
      )
      write_MergeTree(
         con=tkcon$chcon,
         dbName=dbInfo$name,
         tableName="___indexes___",
         value=dbm$indexes,
         rtypes=dbm$indexes %>%
            summarise_all(function(x)class(x)[1]) %>% unlist(),
         nullable=NULL,
         indexes=c("table", "field")
      )
   }, silent=TRUE)
   if(inherits(er, "try-error")){
      dbSendQuery(
         tkcon$chcon,
         sprintf(
            "DROP DATABASE `%s`",
            dbInfo$name
         )
      )
      stop(as.character(er))
   }
   
   ## Add the data ----
   er <- try({
      for(tn in names(rdbm)){
         rtypes <- rdbm[[tn]]$fields$type
         names(rtypes) <- rdbm[[tn]]$fields$name
         write_MergeTree(
            con=tkcon$chcon,
            dbName=dbInfo$name,
            tableName=tn,
            value=imdb[[tn]],
            rtypes=rtypes,
            nullable=rdbm[[tn]]$fields$name[which(rdbm[[tn]]$fields$nullable)],
            indexes=unique(index_table(rdbm[[tn]])$field)
         )
      }
   }, silent=TRUE)
   if(inherits(er, "try-error")){
      dbSendQuery(
         tkcon$chcon,
         sprintf(
            "DROP DATABASE `%s`",
            dbInfo$name
         )
      )
      stop(as.character(er))
   }
   
   ## Add database to the listing ----
   er <- try({
      ch_insert(
         tkcon$chcon,
         dbName="default", tableName="MDB",
         value=as_tibble(dbInfo) %>%
            select(colnames(curDB))
      )
   }, silent=TRUE)
   if(inherits(er, "try-error")){
      dbSendQuery(
         tkcon$chcon,
         sprintf(
            "DROP DATABASE `%s`",
            dbInfo$name
         )
      )
      stop(as.character(er))
   }
   
   ## Collection members ----
   toRet <- chMDB(tkcon, dbInfo$name)
   cm <- collectionMembers(imdb)
   if(!is.null(cm) && nrow(cm)>0){
      setChMDBcollectionMembers(toRet, cm)
   }
   
   return(toRet)
   
}

###############################################################################@
#' Remove an MDB (modeled database) from a [chTKCat] database
#' 
#' @param tkcon a [chTKCat] object
#' @param name the name of the MDB to remove
#' 
rmMDBFromChTKCat <- function(
   tkcon,
   name
){
   stopifnot(
      is.chTKCat(tkcon),
      is.character(name),
      length(name)==1
   )
   try(dbSendQuery(tkcon$chcon, sprintf("drop database `%s`", name)))
   try(dbSendQuery(
      tkcon$chcon,
      sprintf(
         "alter table default.CollectionMembers DELETE where resource='%s'",
         name
      )
   ))
   try(dbSendQuery(
      tkcon$chcon,
      sprintf("alter table default.MDB DELETE where name='%s'", name)
   ))
}
