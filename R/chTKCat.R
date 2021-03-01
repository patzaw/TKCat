###############################################################################@
#### chTKCat object ####
###############################################################################@


###############################################################################@
#' Connect to a ClickHouse TKCat instance
#'
#' @param host a character string specifying the host heberging the 
#' database (default: localhost)
#' @param port an integer specifying the port on which the 
#' database is listening (default: 9101)
#' @param user user name
#' @param password user password
#' @param http an integer specifying the HTTP port of the 
#' ClickHouse database (default: NULL). Used for documentation only.
#'
#' @return a chTKCat object
#'
#' @seealso [check_chTKCat()], [db_disconnect()], [db_reconnect()]
#' 
#' @export
#'
chTKCat <- function(
   host="localhost",
   port=9101L,
   user="default",
   password,
   http=NULL
){
   if(missing(password)){
      password <- getPass::getPass(
         sprintf("%s password (press escape or cancel if no password)", user),
         noblank=TRUE
      )
   }
   
   chcon <- RClickhouse::dbConnect(
      drv=RClickhouse::clickhouse(),
      host=host,
      port=port,
      user=user, password=ifelse(is.na(password), "", password)
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
#' Check a [chTKCat] object
#'
#' @param x a [chTKCat] object
#' @param verbose a logical indicating if information messages should be
#' displayed.
#'
#' @return Invisible result: [chTKCat] object
#' 
#' @export
#'
check_chTKCat <- function(x, verbose=FALSE){
   
   stopifnot(is.chTKCat(x))
   toRet <- x
   con <- x$chcon
   
   ## Available tables in the default database ----
   defaultTables <- DBI::dbGetQuery(
      con,
      "SELECT name FROM system.tables WHERE database='default'"
   )$name
   
   ## Available databases ----
   chMDBs <- list_MDBs(x, withInfo=FALSE)
   
   if(length(defaultTables)==0 & length(chMDBs)==0){
      ## Not initialized
      toRet$init <- FALSE
      if(verbose){
         message("Empty ClickHouse instance. Ready to be initialized.")
      }
      
   }else{
      ## Not a chTKCat
      if(any(
         !names(DEFAULT_DATA_MODEL) %in% defaultTables
      )){
         stop('Non-empty Clickhouse database and not a chTKCat')
      }
      
      ## User permissions
      admin <- DBI::dbGetQuery(
         con, sprintf("SELECT admin FROM Users WHERE login='%s'", con@user)
      ) %>% 
         dplyr::pull("admin") %>%
         as.logical()
      
      ## System information
      dbSys <- DBI::dbGetQuery(
         con,
         sprintf(
            "SELECT %s FROM default.System",
            ifelse(
               admin, "*",
               "name, instance, version, contact"
            )
         )
      )
      if(
         any(
            !c("name", "instance", "version", "contact") %in% colnames(dbSys)
         ) ||
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
      toRet$contact <- dbSys$contact
      toRet$path <- dbSys$path
      toRet$admin <- admin
   }
   for(i in setdiff(intersect(names(x), names(toRet)), c("path", "admin"))){
      if(!identical(x[[i]], toRet[[i]])){
         print(toRet)
         stop(sprintf('Incoherent "%s" information', i))
      }
   }
   
   ## Check collection consistency ----
   rcols <- list_chTKCat_collections(x, withJson=TRUE)
   lcols <- list_local_collections(withJson=TRUE)
   for(col in rcols$title){
      if(col %in% lcols$title){
         lcol <- get_local_collection(title=col) %>% 
            jsonlite::fromJSON(simplifyVector=FALSE)
         rcol <- rcols %>%
            dplyr::filter(.data$title==!!col) %>%
            dplyr::pull("json") %>% 
            jsonlite::fromJSON(simplifyVector=FALSE)
         if(!identical(lcol, rcol)){
            warning(
               sprintf(
                  "Remote %s collection is different from the local version.",
                  col
               ),
               " Be careful when manipulating members of this collection."
            )
         }
      }else{
         rcol <- rcols %>%
            dplyr::filter(.data$title==!!col) %>%
            dplyr::pull("json")
         import_local_collection(rcol)
      }
   }
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
#' Format a [chTKCat] object for printing
#' 
#' @param x a [chTKCat] object
#' @param ... not used
#' 
#' @return A single character
#' 
#' @export
#'
format.chTKCat <- function(x, ...){
   toRet <- sprintf(
      "chTKCat on %s:%s", x$chcon@host, x$chcon@port
   )
   if(x$init){
      toRet <- paste(
         toRet,
         sprintf("   - Instance: %s", x$instance),
         sprintf("   - Version: %s", x$version),
         sprintf("   - Instance contact: %s", x$contact),
         sprintf("   - User: %s", x$chcon@user),
         sep="\n"
      )
      if(x$admin){
         toRet <- paste(toRet, "ADMIN MODE", sep="\n")
      }
   }
   return(toRet)
}


###############################################################################@
#' @export
#'
print.chTKCat <- function(x, ...){
   cat(format(x, ...), "\n")
   invisible()
}


###############################################################################@
#' 
#' @rdname db_disconnect
#' @method db_disconnect chTKCat
#' 
#' @export
#'
db_disconnect.chTKCat <- function(x){
   RClickhouse::dbDisconnect(x[["chcon"]])
   invisible()
}

###############################################################################@
#' 
#' @rdname db_reconnect
#' @method db_reconnect chTKCat
#' 
#' @export
#'
db_reconnect.chTKCat <- function(x, user, password, ntries=3){
   xn <- deparse(substitute(x))
   con <- x$chcon
   db_reconnect(con, user=user, password=password, ntries=ntries)
   nv <- x
   nv$chcon <- con
   nv <- check_chTKCat(nv)
   assign(xn, nv, envir=parent.frame(n=1))
   invisible(nv)
}


###############################################################################@
#' 
#' @rdname get_query
#' @method get_query chTKCat
#' 
#' @export
#'
get_query.chTKCat <- function(x, query, ...){
   DBI::dbGetQuery(x$chcon, query, ...) %>%
      as_tibble()
}


###############################################################################@
#### DATABASE MANAGEMENT ####
###############################################################################@


.create_password <- function(login){
   pw1 <- getPass::getPass(sprintf("Set %s password", login), noblank=TRUE)
   if(is.null(pw1)){
      stop("Canceled by the user")
   }
   pw2 <- getPass::getPass(sprintf("Confirm %s password", login), noblank=TRUE)
   if(is.null(pw2)){
      stop("Canceled by the user")
   }
   if(pw1!=pw2){
      stop("Provided passwords are different")
   }
   return(pw1)
}

###############################################################################@
#' Initialize a chTKCat database
#' 
#' The initialization can only be done locally (host="localhost")
#'
#' @param x a [chTKCat] object
#' @param instance instance name of the database
#' @param version version name of the database
#' @param path path to ClickHouse folder
#' @param login login of the primary administrator of the database
#' @param password password for the primary administrator of the database
#' @param contact contact information for the primary administrator of
#' the database
#' @param userfile path to a ClickHouse users.xml file. If NULL (default),
#' the file provided within the TKCat
#' package (`system.file("ClickHouse/users.xml", package="TKCat")`)
#' is used.
#'
#' @return a [chTKCat]
#' 
#' @export
#'
init_chTKCat <- function(
   x, instance, version, path,
   login, password, contact, userfile=NULL
){
   con <- x$chcon
   ## Check that ClickHouse is empty and ready for initialization ----
   check_chTKCat(x)
   if(con@host!="localhost"){
      stop("Initialisation can only be done for localhost instances")
   }
   defaultTables <- DBI::dbGetQuery(
      con,
      "select name from system.tables where database='default'"
   )$name
   if("System" %in% defaultTables){
      stop("chTKCat already initialized")
   }
   stopifnot(
      is.character(instance), length(instance)==1, !is.na(instance),
      is.character(version), length(version)==1, !is.na(version),
      is.character(path), length(path)==1, !is.na(path),
      is.character(contact), length(contact)==1, !is.na(contact),
      is.character(login), length(login)==1, !is.na(login),
      length(grep("[^[:alnum:]_]", login))==0
   )
   if(missing(password)){
      password <- .create_password(login)
   }
   stopifnot(
      is.character(password), length(password)==1
   )
   
   ## users.xml file ----
   if(!is.null(userfile)){
      stopifnot(length(userfile)==1, file.exists(userfile))
   }else{
      userfile <- system.file(
         "ClickHouse/users.xml",
         package=utils::packageName()
      )
   }
   
   ## Enabling introspection functions (for GRANT access) ----
   RClickhouse::dbSendQuery(con, "SET allow_introspection_functions=1")
   
   ## Create default tables ----
   mergeTrees_from_RelDataModel(
      con, "default",
      DEFAULT_DATA_MODEL
   )
   ch_insert(
      con, "default", "System",
      value=dplyr::tibble(
         name="chTKCat",
         instance=as.character(instance),
         version=as.character(version),
         contact=as.character(contact),
         path=as.character(path)
      )
   )
   x$init <- TRUE
   x$admin <- TRUE
   
   ## Create admin user ----
   create_chTKCat_user(
      x, login=login, password=password, contact=contact, admin=TRUE
   )
   db_disconnect(x)
   file.copy(userfile, file.path(path, "conf", "users.xml"), overwrite=TRUE)
   Sys.sleep(3)
   x <- chTKCat(
      host=con@host,
      port=con@port,
      http=x$http,
      user=login,
      password=password
   )
   con <- x$chcon
   
   ## Create default user ----
   create_chTKCat_user(
      x, login="default", password=NA, contact=NA, admin=FALSE
   )
   
   ## Finalize ----
   return(check_chTKCat(x))
}


###############################################################################@
#' List [chTKCat] user
#'
#' @param x a [chTKCat] object
#' 
#' @return A tibble with 3 columns:
#' - login: user login
#' - contact: user contact information
#' - admin: if the user is an admin of the chTKCat object
#' 
#' @export
#'
list_chTKCat_users <- function(x){
   stopifnot(
      is.chTKCat(x)
   )
   con <- x$chcon
   toRet <- DBI::dbGetQuery(
      con,
      sprintf(
         "SELECT %s FROM default.Users",
         ifelse(x$admin, "*", "login, admin")
      )
   ) %>% 
      dplyr::as_tibble()
   if("admin" %in% colnames(toRet)){
      toRet$admin <- as.logical(toRet$admin)
   }
   return(toRet)
}


###############################################################################@
#' Create a chTKCat user
#' 
#' @param x a [chTKCat] object
#' @param login user login
#' @param password user password
#' @param contact contact information (can be NA)
#' @param admin a logical indicating if the user is an admin of the chTKCat
#' instance
#' 
#' @return No return value, called for side effects
#' 
#' @export
create_chTKCat_user <- function(
   x, login, password, contact, admin=FALSE
){
   contact <- as.character(contact)
   stopifnot(
      is.chTKCat(x),
      is.character(login), length(login)==1, !is.na(login),
      length(grep("[^[:alnum:]_]", login))==0,
      is.character(contact), length(contact)==1,
      is.logical(admin), length(admin)==1, !is.na(admin)
   )
   if(!x$admin){
      stop("Only chTKCat admin can add users")
   }
   if(missing(password)){
      password <- .create_password(login)
   }
   password <- as.character(password)
   stopifnot(
      is.character(password), length(password)==1
   )
   con <- x$chcon
   ## Create the user in ClickHouse ----
   RClickhouse::dbSendQuery(
      con, 
      sprintf(
         "CREATE USER %s %s",
         login,
         ifelse(
            is.na(password),
            "IDENTIFIED WITH no_password",
            sprintf("IDENTIFIED BY '%s'", password)
         )
      )
   )
   ## Grant access ----
   if(admin){
      RClickhouse::dbSendQuery(
         con,
         sprintf(
            "GRANT ALL ON *.* TO %s WITH GRANT OPTION",
            login
         )
      )
   }else{
      RClickhouse::dbSendQuery(
         con, sprintf("GRANT SHOW DATABASES ON *.* TO %s", login)
      )
      RClickhouse::dbSendQuery(
         con, sprintf("GRANT SHOW TABLES ON *.* TO %s", login)
      )
      RClickhouse::dbSendQuery(
         con, sprintf("GRANT SHOW COLUMNS ON *.* TO %s", login)
      )
      RClickhouse::dbSendQuery(
         con,
         sprintf(
            paste(
               "GRANT SELECT(name, instance, version, contact)",
               "ON default.System TO %s"
            ),
            login
         )
      )
      RClickhouse::dbSendQuery(
         con, sprintf("GRANT SELECT ON default.Collections TO %s", login)
      )
      RClickhouse::dbSendQuery(
         con, sprintf("GRANT SELECT(login, admin) ON default.Users TO %s", login)
      )
      for(db in list_MDBs(x, withInfo=FALSE)){
         for(tn in names(CHMDB_DATA_MODEL)){
            RClickhouse::dbSendQuery(
               con, 
               sprintf(
                  "GRANT SELECT on `%s`.`%s` TO %s",
                  db, tn, login
               )
            )
         }
      }
   }
   ## Register the user ----
   ch_insert(
      con, "default", "Users", dplyr::tibble(
         login=login,
         contact=as.character(contact),
         admin=admin
      )
   )
   invisible()
}


###############################################################################@
#' Drop a user from a [chTKCat] object
#'
#' @param x a [chTKCat] object
#' @param login login of the user to drop
#' 
#' @return No return value, called for side effects
#' 
#' @export
#'
drop_chTKCat_user <- function(x, login){
   stopifnot(
      is.chTKCat(x),
      is.character(login), length(login)==1, !is.na(login)
   )
   if(!x$admin){
      stop("Only chTKCat admin can drop users")
   }
   if(!login %in% list_chTKCat_users(x)$login){
      stop("The user does not exist")
   }
   con <- x$chcon
   allDb <- list_MDBs(x, withInfo=FALSE)
   for(mdb in allDb){
      remove_chMDB_user(x=x, login=login, mdb=mdb)
   }
   RClickhouse::dbSendQuery(
      con,
      sprintf("ALTER TABLE default.Users DELETE WHERE login='%s'", login)
   )
   RClickhouse::dbSendQuery(
      con,
      sprintf("DROP USER %s", login)
   )
   invisible()
}


###############################################################################@
#### chMDB MANAGEMENT ####
###############################################################################@


###############################################################################@
#' 
#' @rdname list_MDBs
#' @method list_MDBs chTKCat
#' 
#' @export
#'
list_MDBs.chTKCat <- function(x, withInfo=TRUE){
   stopifnot(is.chTKCat(x))
   con <- x$chcon
   dbNames <- DBI::dbGetQuery(con, "SELECT * FROM system.databases") %>% 
      dplyr::pull("name") %>% 
      setdiff(CH_RESERVED_DB)
   if(!withInfo){
      return(dbNames)
   }else{
      check_chTKCat(x, verbose=TRUE)
      toRet <- c()
      for(dbName in dbNames){
         dbTables <- DBI::dbGetQuery(
            con,
            sprintf("SHOW TABLES FROM `%s`", dbName)
         )
         if("___MDB___" %in% dbTables$name){
            toRet <- bind_rows(
               toRet,
               DBI::dbGetQuery(
                  con, sprintf("SELECT * FROM `%s`.___MDB___", dbName)
               ) %>% as_tibble()
            )
         }
      }
      return(toRet)
   }
}


###############################################################################@
#' 
#' @rdname search_MDB_tables
#' @method search_MDB_tables chTKCat
#' 
#' @export
#'
search_MDB_tables.chTKCat <- function(x, searchTerm){
   mdbs <- list_MDBs(x)
   selQueries <- paste(
      sprintf(
         "SELECT '%s' as resource, name, comment,",
         mdbs$name
      ),
      sprintf(
         "positionCaseInsensitive(name, '%s')>0 as s1,",
         searchTerm
      ),
      if(nchar(searchTerm)>4){
         sprintf(
            "ngramSearchCaseInsensitive(name, '%s') as s2,",
            searchTerm
         )
      }else{
         "0 as s2,"
      },
      sprintf(
         "if(isNull(comment), 0, positionCaseInsensitive(comment, '%s')>0) as s3,",
         searchTerm
      ),
      if(nchar(searchTerm)>4){
         sprintf(
            "if(isNull(comment), 0, ngramSearchCaseInsensitive(comment, '%s')) as s4,",
            searchTerm
         )
      }else{
         "0 as s4,"
      },
      "greatest(s4, greatest(s3, greatest(s2, s1))) as ms",
      sprintf("FROM `%s`.`___Tables___`", mdbs$name),
      "WHERE ms > 0"
   )
   query <- paste(selQueries, collapse=" UNION ALL ")
   toRet <- get_query(x, query) %>% 
      dplyr::arrange(desc(.data$ms)) %>%
      dplyr::select("resource", "name", "comment")
   return(toRet)
}


###############################################################################@
#' 
#' @rdname search_MDB_fields
#' @method search_MDB_fields chTKCat
#' 
#' @export
#'
search_MDB_fields.chTKCat <- function(x, searchTerm){
   mdbs <- list_MDBs(x)
   selQueries <- paste(
      sprintf(
         "SELECT '%s' as resource, table, name, type, nullable, unique, comment,",
         mdbs$name
      ),
      sprintf(
         "positionCaseInsensitive(name, '%s')>0 as s1,",
         searchTerm
      ),
      if(nchar(searchTerm)>4){
         sprintf(
            "ngramSearchCaseInsensitive(name, '%s') as s2,",
            searchTerm
         )
      }else{
         "0 as s2,"
      },
      sprintf(
         "if(isNull(comment), 0, positionCaseInsensitive(comment, '%s')>0) as s3,",
         searchTerm
      ),
      if(nchar(searchTerm)>4){
         sprintf(
            "if(isNull(comment), 0, ngramSearchCaseInsensitive(comment, '%s')) as s4,",
            searchTerm
         )
      }else{
         "0 as s4,"
      },
      "greatest(s4, greatest(s3, greatest(s2, s1))) as ms",
      sprintf("FROM `%s`.`___Fields___`", mdbs$name),
      "WHERE ms > 0"
   )
   query <- paste(selQueries, collapse=" UNION ALL ")
   toRet <- get_query(x, query) %>%
      dplyr::arrange(desc(.data$ms)) %>%
      dplyr::select(
         "resource", "table", "name", "comment",
         "type", "nullable", "unique"
      ) %>%
      dplyr::mutate(
         nullable=as.logical(.data$nullable),
         unique=as.logical(.data$unique)
      )
}


###############################################################################@
#' Create a database in a [chTKCat]
#' 
#' @param x a [chTKCat] object
#' @param name the name of the new database
#' @param public if the database data are accessible to any user (default:FALSE)
#' 
#' @return No return value, called for side effects
#' 
#' @export
#' 
create_chMDB <- function(x, name, public=FALSE){
   stopifnot(
      is.chTKCat(x),
      is.character(name), length(name)==1, !is.na(name)
   )
   if(!x$admin){
      stop("Only chTKCat admin can create an MDB in ClickHouse")
   }
   if(name %in% list_MDBs(x, withInfo=FALSE)){
      stop("The database exists already")
   }
   con <- x$chcon
   RClickhouse::dbSendQuery(con, sprintf("CREATE DATABASE `%s`", name))
   mergeTrees_from_RelDataModel(
      con, name,
      CHMDB_DATA_MODEL
   )
   users <- list_chTKCat_users(x) %>% dplyr::pull("login")
   for(tn in setdiff(names(CHMDB_DATA_MODEL), "___MDBUsers___")){
      RClickhouse::dbSendQuery(
         con,
         sprintf(
            "GRANT SELECT ON `%s`.`%s` TO %s",
            name, tn, paste(users, collapse=", ")
         )
      )
   }
   set_chMDB_access(x, name, public=public)
   add_chMDB_user(x, name, x$chcon@user, admin=TRUE)
   invisible()
}


###############################################################################@
#' Drop a database from a [chTKCat]
#' 
#' @param x a [chTKCat] object
#' @param name the name of the database to remove
#' 
#' @return No return value, called for side effects
#' 
#' @export
#' 
drop_chMDB <- function(x, name){
   stopifnot(
      is.chTKCat(x),
      is.character(name), length(name)==1, !is.na(name)
   )
   if(!name %in% list_MDBs(x, withInfo=FALSE)){
      stop("The database does not exist")
   }
   if(!x$admin){
      stop("Only chTKCat admin can drop an MDB from ClickHouse")
   }
   con <- x$chcon
   RClickhouse::dbSendQuery(con, sprintf("DROP DATABASE `%s`", name))
   ul <- list_chTKCat_users(x) %>% 
      dplyr::filter(!.data$admin) %>% 
      dplyr::pull("login")
   RClickhouse::dbSendQuery(
      con,
      sprintf(
         "REVOKE %s ON `%s`.* FROM %s",
         paste(CH_DB_STATEMENTS, collapse=", "), name, paste(ul, collapse=", ")
      )
   )
   invisible()
}


###############################################################################@
#' Empty a chMDB in a [chTKCat]
#' 
#' @param x a [chTKCat] object
#' @param name the name of the database to empty
#' 
#' @return No return value, called for side effects
#' 
#' @export
#' 
empty_chMDB <- function(x, name){
   stopifnot(
      is.chTKCat(x),
      is.character(name), length(name)==1, !is.na(name)
   )
   if(!name %in% list_MDBs(x, withInfo=FALSE)){
      stop("The database does not exist")
   }
   con <- x$chcon
   toDrop <- setdiff(
      list_tables(con, name)$name,
      names(CHMDB_DATA_MODEL)
   )
   for(tn in toDrop){
      RClickhouse::dbSendQuery(
         x$chcon,
         sprintf("DROP TABLE `%s`.`%s`", name, tn)
      )
   }
   toEmpty <- setdiff(
      names(CHMDB_DATA_MODEL),
      c("___MDBUsers___", "___Public___")
   )
   for(tn in toEmpty){
      RClickhouse::dbSendQuery(
         x$chcon,
         sprintf("ALTER TABLE `%s`.`%s` DELETE WHERE 1", name, tn)
      )
   }
   Sys.sleep(5)
   invisible()
}


###############################################################################@
#' Is a chMDB public
#' 
#' @param x a [chTKCat] object
#' @param mdb name of the modeled database
#' 
#' @return A logical indicating if the chMDB is public or not.
#' 
#' @export
#' 
is_chMDB_public <- function(x, mdb){
   stopifnot(
      is.chTKCat(x),
      is.character(mdb), length(mdb)==1, !is.na(mdb)
   )
   if(!mdb %in% list_MDBs(x, withInfo=FALSE)){
      stop("The database does not exist")
   }
   con <- x$chcon
   toRet <- DBI::dbGetQuery(
      con, 
      sprintf("SELECT public from `%s`.___Public___", mdb)
   ) %>%
      dplyr::pull("public") %>% 
      as.logical()
   if(length(toRet)==0){
      toRet <- FALSE
   }
   return(toRet)
}


###############################################################################@
#' Set  chMDB access
#' 
#' @param x a [chTKCat] object
#' @param mdb name of the modeled database
#' @param public if access is public
#' 
#' @return No return value, called for side effects
#' 
#' @export
#' 
set_chMDB_access <- function(x, mdb, public){
   stopifnot(
      is.chTKCat(x),
      is.character(mdb), length(mdb)==1, !is.na(mdb),
      is.logical(public), length(public)==1, !is.na(public)
   )
   if(!mdb %in% list_MDBs(x, withInfo=FALSE)){
      stop("The database does not exist")
   }
   con <- x$chcon
   RClickhouse::dbSendQuery(
      con, 
      sprintf(
         "ALTER TABLE `%s`.___Public___ DELETE WHERE 1",
         mdb,
         as.integer(public)
      )
   )
   ch_insert(con, dbName=mdb, tableName="___Public___", value=tibble(public=public))
   users <- list_chTKCat_users(x) %>% 
      dplyr::filter(!.data$admin) %>% 
      dplyr::pull("login")
   chMDBusers <- list_chMDB_users(x, mdb)$login
   dbTables <- DBI::dbGetQuery(con, sprintf("SHOW TABLES FROM `%s`", mdb)) %>% 
      pull("name")
   if(public){
      RClickhouse::dbSendQuery(
         con,
         sprintf(
            "GRANT SELECT ON `%s`.* TO %s",
            mdb, paste(users, collapse=", ")
         )
      )
      ul <- setdiff(users, chMDBusers)
      RClickhouse::dbSendQuery(
         con,
         sprintf(
            "REVOKE SELECT ON `%s`.`%s` FROM %s",
            mdb, "___MDBUsers___", paste(ul, collapse=", ")
         )
      )
   }else{
      ul <- setdiff(users, chMDBusers)
      if(length(ul)>0){
         for(tn in setdiff(dbTables, names(CHMDB_DATA_MODEL))){
            RClickhouse::dbSendQuery(
               con,
               sprintf(
                  "REVOKE SELECT ON `%s`.`%s` FROM %s",
                  mdb, tn, paste(ul, collapse=", ")
               )
            )
         }
      }
   }
   invisible()
}


###############################################################################@
#' List users of an MDB of a [chTKCat] object
#'
#' @param x a [chTKCat] object
#' @param mdb name of the modeled database
#' 
#' @return A tibble with 3 columns:
#' - user: the user login
#' - mdb: the name of the modeled database
#' - admin: if the user is an admin of the MDB
#' 
#' @export
#'
list_chMDB_users <- function(x, mdb){
   stopifnot(
      is.chTKCat(x),
      is.character(mdb), length(mdb)==1, !is.na(mdb),
      mdb %in% list_MDBs(x, withInfo=FALSE)
   )
   con <- x$chcon
   mdbut <- DBI::dbGetQuery(
      con,
      sprintf(
         paste(
            "SELECT name FROM system.tables",
            "WHERE name='___MDBUsers___' AND database='%s'"
         ),
         mdb
      )
   ) %>% 
      dplyr::pull("name")
   if(length(mdbut)==0){
      stop("The chMDB does not exist or is not initialized yet")
   }
   DBI::dbGetQuery(
      con,
      sprintf(
         "SELECT * FROM `%s`.`%s`",
         mdb, mdbut
      )
   ) %>% 
      dplyr::as_tibble() %>%
      dplyr::mutate(admin=as.logical(.data$admin)) %>% 
      return()
}


###############################################################################@
#' Add a user to an MDB of a [chTKCat] object
#'
#' @param x a [chTKCat] object
#' @param mdb name of the modeled database
#' @param login login of the user to drop
#' @param admin if the user is an admin of the MDB
#' 
#' @return No return value, called for side effects
#' 
#' @export
#'
add_chMDB_user <- function(x, mdb, login, admin=FALSE){
   stopifnot(
      is.chTKCat(x),
      is.character(login), length(login)==1, !is.na(login),
      is.character(mdb), length(mdb)==1, !is.na(mdb)
   )
   if(!mdb %in% list_MDBs(x, withInfo=FALSE)){
      stop("The database does not exist")
   }
   if(!login %in% list_chTKCat_users(x)$login){
      stop("The user does not exist")
   }
   if(login %in% list_chMDB_users(x, mdb=mdb)$login){
      stop(sprintf("%s is already registered for %s chMDB", login, mdb))
   }
   con <- x$chcon
   ch_insert(
      con, dbName=mdb, tableName="___MDBUsers___",
      value=tibble(login=login, admin=admin)
   )
   RClickhouse::dbSendQuery(
      con,
      sprintf(
         "GRANT SELECT ON `%s`.* TO %s",
         mdb, login
      )
   )
   if(admin){
      RClickhouse::dbSendQuery(
         con,
         sprintf(
            "GRANT CREATE TABLE, DROP TABLE, ALTER, INSERT ON `%s`.* TO %s",
            mdb, login
         )
      )
      dbTables <- DBI::dbGetQuery(
         con, sprintf("SHOW TABLES FROM `%s`", mdb)
      ) %>% 
         dplyr::pull("name")
      modelTables <- names(CHMDB_DATA_MODEL)
      for(tn in setdiff(dbTables, modelTables)){
         RClickhouse::dbSendQuery(
            con,
            sprintf(
               "GRANT DROP TABLE ON `%s`.`%s` TO %s",
               mdb, tn, login
            )
         )
      }
   }
   invisible()
}


###############################################################################@
#' Drop a user of an MDB of a [chTKCat] object
#'
#' @param x a [chTKCat] object
#' @param mdb name of the modeled database
#' @param login login of the user to drop
#' 
#' @return No return value, called for side effects
#' 
#' @export
#'
remove_chMDB_user <- function(x, mdb, login){
   stopifnot(
      is.chTKCat(x),
      is.character(login), length(login)==1, !is.na(login),
      is.character(mdb), length(mdb)==1, !is.na(mdb)
   )
   if(!mdb %in% list_MDBs(x, withInfo=FALSE)){
      stop("The database does not exist")
   }
   con <- x$chcon
   RClickhouse::dbSendQuery(
      con,
      sprintf(
         "ALTER TABLE `%s`.___MDBUsers___ DELETE WHERE login='%s'",
         mdb, login
      )
   )
   mdbAdmin <- list_chTKCat_users(x) %>% 
      dplyr::filter(.data$login==!!login) %>% 
      dplyr::pull("admin")
   if(!mdbAdmin){
      public <- is_chMDB_public(x, mdb)
      RClickhouse::dbSendQuery(
         con,
         sprintf(
            "REVOKE %s ON `%s`.* FROM %s",
            paste(
               CH_DB_STATEMENTS,
               collapse=", "
            ),
            mdb,
            login
         )
      )
      selTables <- names(CHMDB_DATA_MODEL)
      if(public){
         selTables <- DBI::dbGetQuery(
            con, sprintf("SHOW TABLES FROM `%s`", mdb)
         ) %>% 
            dplyr::pull("name")
      }
      selTables <- setdiff(selTables, "___MDBUsers___")
      for(tn in selTables){
         RClickhouse::dbSendQuery(
            con,
            sprintf("GRANT SELECT ON `%s`.`%s` TO %s", mdb, tn, login)
         )
      }
   }
   invisible()
}


###############################################################################@
#### COLLECTIONS ####
###############################################################################@


###############################################################################@
#' List collections available in a [chTKCat]
#' 
#' @param x a [chTKCat] object
#' @param withJson if TRUE, returns the json strings of the collection
#' (default: FALSE)
#' 
#' @return A tibble with the title, the description and optionally the json
#' definition of the collections
#' 
#' @export
#' 
list_chTKCat_collections <- function(x, withJson=FALSE){
   stopifnot(is.chTKCat(x))
   dbGetQuery(
      conn=x$chcon,
      statement=sprintf(
         "SELECT title, description %s FROM default.Collections",
         ifelse(withJson, ", json", "")
      )
   ) %>%
      as_tibble() %>% 
      return()
}


###############################################################################@
#' Get a collection from a [chTKCat]
#' 
#' @param x a [chTKCat] object
#' @param title the title of the collection to get
#' 
#' @return The definition of the collection as a JSON string.
#' 
#' @export
#' 
get_chTKCat_collection <- function(x, title){
   stopifnot(
      is.chTKCat(x), is.character(title), length(title)==1, !is.na(title)
   )
   if(!title %in% list_chTKCat_collections(x)$title){
      stop("This collection is not available")
   }
   dbGetQuery(
      conn=x$chcon,
      statement=sprintf(
         "SELECT json FROM default.Collections WHERE title='%s'",
         title
      )
   )$json
}


###############################################################################@
#' Import a collection in a [chTKCat] database
#' 
#' @param x a [chTKCat] object
#' @param json a single character indicating the collection to import. Can be:
#' - a path to a file
#' - the name of a local collection (see [list_local_collections()])
#' - the json text defining the collection
#' @param overwrite a logical indicating if the existing collection should
#' be replaced.
#' 
#' @return No return value, called for side effects
#' 
#' @export
#' 
add_chTKCat_collection <- function(x, json, overwrite=FALSE){
   stopifnot(
      is.chTKCat(x),
      is.character(json),
      length(json)==1
   )
   if(!x$admin){
      stop("Only chTKCat admin can add collections")
   }
   if(file.exists(json)){
      raw <- readLines(json) %>% paste(collapse="\n")
   }else if(json %in% list_local_collections()$title){
      raw <- get_local_collection(json)
   }else{
      raw <- json
   }
   if(!jsonvalidate::json_validate(raw, tkcatEnv$COL_SCHEMA, verbose=TRUE)){
      stop("Not a valid collection")
   }
   def <- jsonlite::fromJSON(raw)
   ctitle <- def$properties$collection$enum
   writeCol <- TRUE
   if(ctitle %in% list_chTKCat_collections(x)$title){
      lcol <- raw %>% 
         jsonlite::fromJSON(simplifyVector=FALSE)
      rcol <- get_chTKCat_collection(x, ctitle) %>% 
         jsonlite::fromJSON(simplifyVector=FALSE)
      if(identical(lcol, rcol)){
         writeCol <- FALSE
      }else{
         if(!overwrite){
            stop(
               sprintf(
                  "Remote %s collection is different from the local version.",
                  ctitle
               ),
               " Use the overwrite parameter of",
               " the add_chTKCat_collection function to update it in chTKCat"
            )
         }else{
            writeCol <- TRUE
         }
      }
   }
   if(writeCol){
      RClickhouse::dbSendQuery(
         conn=x$chcon,
         statement=sprintf(
            "ALTER TABLE default.Collections DELETE WHERE title='%s'",
            ctitle
         )
      )
      toWrite<- dplyr::tibble(
         title=ctitle,
         description=def$description,
         json=raw
      )
      ch_insert(
         con=x$chcon,
         dbName="default",
         tableName="Collections",
         value=toWrite
      )
   }
   invisible()
}


###############################################################################@
#' Remove a collection from a [chTKCat] database
#' 
#' @param x a [chTKCat] object
#' @param title the title of the collection to remove
#' 
#' @return No return value, called for side effects
#' 
#' @export
#' 
remove_chTKCat_collection <- function(x, title){
   stopifnot(
      is.chTKCat(x),
      is.character(title), length(title)==1, !is.na(title)
   )
   if(!x$admin){
      stop("Only chTKCat admin can remove collections")
   }
   if(!title %in% list_chTKCat_collections(x)$title){
      stop("The collection does not exist in the chTKCat")
   }
   RClickhouse::dbSendQuery(
      conn=x$chcon,
      statement=sprintf(
         "ALTER TABLE default.Collections DELETE WHERE title='%s'",
         title
      )
   )
   invisible()
}


###############################################################################@
#' 
#' @rdname collection_members
#' @method collection_members chTKCat
#' 
#' @export
#'
collection_members.chTKCat <- function(
   x,
   ...
){
   
   con <- x$chcon
   dbNames <- DBI::dbGetQuery(con, "SELECT * FROM system.databases") %>% 
      dplyr::pull("name") %>% 
      setdiff(CH_RESERVED_DB)
   
   check_chTKCat(x, verbose=TRUE)
   toRet <- c()
   for(dbName in dbNames){
      dbTables <- DBI::dbGetQuery(
         con,
         sprintf("SHOW TABLES FROM `%s`", dbName)
      )
      if("___CollectionMembers___" %in% dbTables$name){
         toRet <- bind_rows(
            toRet,
            DBI::dbGetQuery(
               con,
               sprintf(
                  paste(
                     "SELECT DISTINCT collection, table",
                     "FROM `%s`.___CollectionMembers___"
                  ),
                  dbName
               )
            ) %>%
               dplyr::as_tibble() %>%
               dplyr::mutate(resource=dbName) %>% 
               dplyr::select("resource", "collection", "table") %>% 
               dplyr::distinct()
         )
      }
   }
   return(toRet)
   
}


###############################################################################@
#### SHINY EXPLORER ####
###############################################################################@


###############################################################################@
#'
#' @param subSetSize the maximum number of records to show
#' @param host the name of the host to show in the application
#' @param download a logical indicating if data can be downloaded
#' (default: FALSE). If TRUE a temporary directory is created and made
#' available for shiny.
#' @param workers number of available workers when download is available
#' (default: 4)
#' 
#' @rdname explore_MDBs
#' @method explore_MDBs chTKCat
#' 
#' @export
#'
explore_MDBs.chTKCat <- function(
   x,
   subSetSize=100,
   host=x$chcon@host,
   download=FALSE,
   workers=4,
   ...
){
   stopifnot(
      is.logical(download), length(download)==1, !is.na(download)
   )
   if(download){
      ddir <- tempfile()
      dir.create(ddir)
      oplan <- future::plan(
         future::multisession, workers=workers
      )
   }else{
      ddir <- NULL
   }
   on.exit({
      if(interactive()){
         warning(
            "Disconnected from clickhouse database. ",
            "Use the db_reconnect(x) function to reconnect x."
         )
      }
   }, add=TRUE)
   shiny::shinyApp(
      ui=.build_etkc_ui(x=x, ddir=ddir),
      server=.build_etkc_server(
         x=x,
         subSetSize=subSetSize,
         host=host,
         ddir=ddir
      ),
      enableBookmarking="url",
      onStart=function(){
         shiny::onStop(function(){
            unlink(ddir, recursive=TRUE, force=TRUE)
            if(exists("oplan")){
               future::plan(oplan)
            }
         })
      }
   )
}

###############################################################################@
.build_etkc_ui.chTKCat <- function(x, ddir=NULL, ...){
   
   .etkc_add_resources(ddir=ddir)
   
   function(req){
      shinydashboard::dashboardPage(
         title="chTKCat",
         
         ########################@
         ## Dashboard header ----
         ## Uses output$instance and output$status
         header=.etkc_sd_header(),
         
         ########################@
         ## Sidebar ----
         ## Uses uiOutput("currentUser") and uiOutput("signin")
         sidebar=.etkc_sd_sidebar(
            sysInterface=TRUE,
            manList=c(
               "chTKCat user guide"="doc/chTKCat-User-guide.html",
               "General TKCat user guide"="doc/TKCat-User-guide.html",
               "chTKCat operations manual"="doc/chTKCat-Operations-manual.html"
            )
         ),
         
         ########################@
         ## Body ----
         body=.etkc_sd_body(sysInterface=TRUE)
      )
   }
   
}


###############################################################################@
.build_etkc_server.chTKCat <- function(
   x,
   subSetSize=100,
   host=x$chcon@host,
   ddir=NULL
){
   .build_etkc_server_default(
      x=x, subSetSize=subSetSize, xparams=list(host=host),
      ddir=ddir
   )
}
