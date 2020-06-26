#' Connect to a ClickHouse TKCat instance
#'
#' @param host a character string specifying the host heberging the 
#' database (default: localhost)
#' @param port an integer specifying the port on which the 
#' database is listening (default: 9101)
#' @param http an integer specifying the HTTP port of the 
#' ClickHouse database (default: 9111). Used for documentation only.
#' @param user user name
#' @param password user password
#'
#' @return a chTKCat object
#'
#' @seealso [check_chTKCat()], [disconnect_chTKCat()]
#'
#' @importFrom RClickhouse clickhouse dbConnect
#' @export
#'
chTKCat <- function(
   host="localhost",
   port=9101L,
   http=9111L,
   user="default",
   password=""
){
   
   chcon <- RClickhouse::dbConnect(
      drv=RClickhouse::clickhouse(),
      host=host,
      port=port,
      user=user, password=password
   )
   
   toRet <- list(
      chcon=chcon,
      http=http
   )
   class(toRet) <- "chTKCat"
   
   toRet <- check_chTKCat(toRet, verbose=TRUE)
   
   return(toRet)
   
}

###############################################################################@
#' Check the object is  a [chTKCat] object
#' 
#' @param x any object
#' 
#' @return A single logical: TRUE if x is a [chTKCat] object
#' 
#' @export
#'
is.chTKCat <- function(x){
   inherits(x, "chTKCat")
}

###############################################################################@
#' Format an [chTKCat] object for printing
#' 
#' @param x an [chTKCat] object
#' 
#' @return A single character
#' 
#' @export
#'
format.chTKCat <- function(x){
   toRet <- sprintf(
      "chTKCat on %s:%s", x$chcon@host, x$chcon@port
   )
   if(x$init){
      toRet <- paste(
         toRet,
         sprintf("   - Instance: %s", x$instance),
         sprintf("   - Version: %s", x$version),
         sprintf("   - User: %s", x$chcon@user),
         sep="\n"
      )
   }
   return(toRet)
}

###############################################################################@
#' @export
#'
print.chTKCat <- function(x, ...){
   cat(format(x, ...), "\n")
}

###############################################################################@
#' Disconnect from a [chTKCat] instance
#'
#' @param x a [chTKCat] object
#'
#' @importFrom RClickhouse dbDisconnect
#' @export
#'
disconnect_chTKCat <- function(x){
   stopifnot(inherits(x, "chTKCat"))
   RClickhouse::dbDisconnect(x[["chcon"]])
}

###############################################################################@
#' Initialize a chTKCat database
#' 
#' The initialization can only be done locally (host=localhost)
#'
#' @param x a [chTKCat] object
#' @param instance instance name of the database
#' @param version version name of the database
#' @param contact maintainer of the chTKCat instance
#' @param path path to ClickHouse folder
#' @param primaryUser user name of the primary administrator of the database
#' @param password password for the primary administrator of the database
#'
#' @return a [chTKCat]
#' 
#' @importFrom RClickhouse dbSendQuery
#' @importFrom DBI dbWriteTable dbGetQuery
#'
init_chTKCat <- function(
   x, instance, version, contact, path,
   primaryUser, password, userfile=NULL
){
   
   ## Check that ClickHouse is empty and ready for initialization ----
   check_chTKCat(x)
   if(x$chcon@host!="localhost"){
      stop("Initialisation can only be done for localhost instances")
   }
   defaultTables <- DBI::dbGetQuery(
      x$chcon,
      "select name from system.tables where database='default'"
   )$name
   if("System" %in% defaultTables){
      stop("chTKCat already initialized")
   }
   
   ## users.xml file ----
   if(!is.null(userfile)){
      stopifnot(file.exists(userfile))
   }else{
      userfile <- system.file(
         "ClickHouse/users.xml",
         package = packageName()
      )
   }
   
   ## Create default tables ----
   ## > System < -----
   DBI::dbWriteTable(
      conn=x$chcon,
      name="System",
      value=tibble(
         name="chTKCat",
         instance=instance,
         version=version,
         contact=contact,
         path=path
      ),
      append=FALSE,
      overwrite=FALSE,
      row.names=FALSE
   )
   ## > MDBs < -----
   RClickhouse::dbSendQuery(
      conn=x$chcon,
      statement=paste(
         "CREATE TABLE default.MDB (",
         "name String,",
         "title String,",
         "description String,",
         "url String,",
         "version String,",
         "maintainer String,",
         "public UInt8",
         ")",
         "ENGINE = MergeTree() ORDER BY (name)"
      )
   )
   ## > Users, MDBUser < -----
   RClickhouse::dbSendQuery(
      conn=x$chcon,
      statement=paste(
         "CREATE TABLE default.Users (",
         "login String,",
         "name String,",
         "contact String",
         ")",
         "ENGINE = MergeTree() ORDER BY (login)"
      )
   )
   RClickhouse::dbSendQuery(
      conn=x$chcon,
      statement=paste(
         "CREATE TABLE default.MDBUsers (",
         "user String,",
         "mdb String,",
         "admin UInt8",
         ")",
         "ENGINE = MergeTree() ORDER BY (user, mdb)"
      )
   )
   ## > Tables < ----
   RClickhouse::dbSendQuery(
      conn=x$chcon,
      statement=paste(
         "CREATE TABLE default.Tables (",
         "mdb String,",
         "name String,",
         "x Nullable(Float64),",
         "y Nullable(Float64),",
         "color Nullable(String),",
         "comment Nullable(String)",
         ")",
         "ENGINE = MergeTree() ORDER BY (mdb, name)"
      )
   )
   ## > Fields < ----
   RClickhouse::dbSendQuery(
      conn=x$chcon,
      statement=paste(
         "CREATE TABLE default.Fields (",
         "mdb String,",
         "table String,",
         "name String,",
         "type String,",
         "nullable UInt8,",
         "unique UInt8,",
         "comment String,",
         "fieldOrder Int32",
         ")",
         "ENGINE = MergeTree() ORDER BY (mdb, table, name)"
      )
   )
   ## > PrimaryKeys < ----
   RClickhouse::dbSendQuery(
      conn=x$chcon,
      statement=paste(
         "CREATE TABLE default.PrimaryKeys (",
         "mdb String,",
         "table String,",
         "field String",
         ")",
         "ENGINE = MergeTree() ORDER BY (mdb, table, field)"
      )
   )
   ## > Indexes < ----
   RClickhouse::dbSendQuery(
      conn=x$chcon,
      statement=paste(
         "CREATE TABLE default.Indexes (",
         "mdb String,",
         "table String,",
         "idx Int32,",
         "field String,",
         "unique UInt8",
         ")",
         "ENGINE = MergeTree() ORDER BY (mdb, table, idx, field)"
      )
   )
   ## > ForeignKeys < ----
   RClickhouse::dbSendQuery(
      conn=x$chcon,
      statement=paste(
         "CREATE TABLE default.ForeignKeys (",
         "mdb String,",
         "table String,",
         "fki Int32,",
         "field String,",
         "refTable String,",
         "refField String,",
         "fmin Int32,",
         "fmax Int32,",
         "tmin Int32,",
         "tmax Int32,",
         ")",
         "ENGINE = MergeTree()",
         "ORDER BY (mdb, table, fki, field, refTable, refField)"
      )
   )
   ## > Collections, CollectionMembers < ----
   RClickhouse::dbSendQuery(
      conn=x$chcon,
      statement=paste(
         "CREATE TABLE default.Collections (",
         "title String,",
         "description String,",
         "json String",
         ")",
         "ENGINE = MergeTree() ORDER BY (title)"
      )
   )
   RClickhouse::dbSendQuery(
      conn=x$chcon,
      statement=paste(
         "CREATE TABLE default.CollectionMembers (",
         "collection String,",
         "mdb String,",
         "table String,",
         "cid Int32,",
         "field String,",
         "static UInt8,",
         "value String,",
         "type Nullable(String)",
         ")",
         "ENGINE = MergeTree() ORDER BY (collection, mdb, table, cid, field)"
      )
   )
   x$init <- TRUE
   
   ## Create admin user ----
   RClickhouse::dbSendQuery(x$chcon , "CREATE ROLE admin")
   RClickhouse::dbSendQuery(
      x$chcon, "GRANT ALL ON *.* TO admin WITH GRANT OPTION"
   )
   create_chTKCat_user(x$chcon, user=primaryUser, password=password, admin=TRUE)
   file.copy(userfile, file.path(path, "conf", "users.xml"))
   disconnect_chTKCat(x)
   x <- chTKCat(
      host=x$chcon@host,
      port=x$chcon@port,
      http=x$http,
      user=primaryUser,
      password=password
   )
   create_chTKCat_user(x$chcon, user="default", admin=FALSE)
   
   ## Finalize ----
   return(check_chTKCat(x))
}

###############################################################################@
#' Check a [chTKCat] object
#'
#' @param x a [chTKCat] object
#' @param verbose a logical indicating if information messages should be
#' displayed.
#'
#' @return Invisible result: [chTKCat] object
#'
#' @importFrom DBI dbGetQuery dbReadTable
#' @export
#'
check_chTKCat <- function(x, verbose=FALSE){
   stopifnot(is.chTKCat(x))
   toRet <- x
   defaultTables <- dbGetQuery(
      x$chcon,
      "select name from system.tables where database='default'"
   )$name
   chMDBs <- setdiff(
      dbGetQuery(x$chcon, "select name from system.databases")$name,
      PCKRESERVED
   )
   if(length(defaultTables)==0 & length(chMDBs)==0){
      toRet$init <- FALSE
      if(verbose){
         message("Empty chTKcat. Ready to be initialized")
      }
   }else{
      if(!"System" %in% defaultTables){
         stop('Non-empty Clickhouse database and not a chTKCat')
      }
      dbSys <- dbGetQuery(
         x$chcon,
         "select * from default.System"
      )
      if(
         any(!c("name", "instance", "version") %in% colnames(dbSys)) ||
         nrow(dbSys) != 1
      ){
         stop("Wrong System table in connected DB")
      }
      if(dbSys$name != "chTKCat"){
         stop("Not a chTKCat")
      }
      toRet$init <- TRUE
      toRet$instance <- dbSys$instance
      toRet$version <- dbSys$version
   }
   for(i in intersect(names(x), names(toRet))){
      if(!identical(x[[i]], toRet[[i]])){
         print(toRet)
         stop(sprintf('Incoherent "%s" information', i))
      }
   }
   return(toRet)
}

###############################################################################@
#' Import a collection in a [chTKCat] database
#' 
#' @param tkcon a [chTKCat] object
#' @param json a single charcter indicating the collection to import. Can be:
#' - a path to a file
#' - the name of a local collection (see [listLocalCollections()])
#' - the json text defining the collection
#' 
#' @importFrom jsonlite fromJSON
#' @importFrom jsonvalidate json_validate
#' 
addChTKCatCollection <- function(tkcon, json, overwrite=FALSE){
   stopifnot(
      is.chTKCat(tkcon),
      is.character(json),
      length(json)==1
   )
   if(file.exists(json)){
      raw <- readLines(f) %>% paste(collapse="\n")
   }else if(json %in% listLocalCollections()$title){
      env <- environment()
      raw <- tkcatEnv$COLLECTIONS %>%
         filter(title==get("json", env)) %>%
         pull(json)
   }else{
      raw <- json
   }
   if(!json_validate(raw, tkcatEnv$COL_SCHEMA, verbose=TRUE)){
      stop("Not a valid collection")
   }
   def <- fromJSON(raw)
   ctitle <- def$properties$collection$enum 
   if(
      ctitle %in% listChTKCatCollections(tkcon)$title &&
      !overwrite
   ){
      stop(
         sprintf(
            'A "%s" has already been imported.',
            ctitle
         ),
         " Set overwrite to TRUE if you want to replace it."
      )
   }
   dbSendQuery(
      conn=tkcon$chcon,
      statement=sprintf(
         "ALTER TABLE default.Collections DELETE WHERE title='%s'",
         ctitle
      )
   )
   toWrite<- tibble(
      title=ctitle,
      description=def$description,
      json=raw
   )
   .dbinsert(
      conn=tkcon$chcon,
      dbName="default",
      tableName="Collections",
      value=toWrite
   )
   invisible(NULL)
}

###############################################################################@
#' List collections available in a [chTKCat]
#' 
#' @param tkcon a [chTKCat] object
#' 
#' @export
#' 
listChTKCatCollections <- function(tkcon){
   stopifnot(is.chTKCat(tkcon))
   as_tibble(dbGetQuery(
      conn=tkcon$chcon,
      statement="SELECT title, description FROM default.Collections"
   ))
}

###############################################################################@
#' List available database in a [chTKCat]
#' 
#' @param tkcon a [chTKCat] object
#' 
#' @export
#' 
listMDBs <- function(tkcon){
   check_chTKCat(tkcon, verbose=TRUE)
   dbGetQuery(tkcon$chcon, "SELECT * FROM default.MDB") %>% as_tibble()
}



###############################################################################@
#' Get collection members of a [chTKCat] object
#' 
#' @param x a [chTKCat] object
#' @param collection a character vector indicating the name of the collections
#' to focus on (default: NULL ==> all of them)
#' 
#' @return See [collectionMembers.chMDB()]
#' 
#' @export
#'
collectionMembers.chTKCat <- function(
   x,
   collections=NULL
){
   stopifnot(
      is.null(collections) || is.character(collections)
   )
   toRet <- dbGetQuery(
      conn=x$chcon,
      statement=
         sprintf(
            "SELECT * FROM default.CollectionMembers %s",
            if(is.null(collections)){
               ""
            }else{
               sprintf(
                  "WHERE collection IN ('%s')",
                  paste(collections, collapse="', '")
               )
            }
         )
   ) %>%
      as_tibble() %>%
      select(collection, resource, cid, table, field, static, value, type) %>%
      mutate(static=as.logical(static))
   return(toRet)
}
