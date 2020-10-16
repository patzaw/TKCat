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
#' Create a database in a [chTKCat]
#' 
#' @param x a [chTKCat] object
#' @param name the name of the new database
#' @param public if the database data are accessible to any user (default:FALSE)
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
      for(tn in setdiff(dbTables, "___MDBUsers___")){
         RClickhouse::dbSendQuery(
            con,
            sprintf(
               "GRANT SELECT ON `%s`.`%s` TO %s",
               mdb, tn, paste(users, collapse=", ")
            )
         )
      }
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
            "GRANT CREATE TABLE, ALTER, INSERT ON `%s`.* TO %s",
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
#' @rdname explore_MDBs
#' @method explore_MDBs chTKCat
#' 
#' @export
#'
explore_MDBs.chTKCat <- function(
   x,
   subSetSize=100,
   host=x$chcon@host
){
   shiny::shinyApp(
      ui=.build_etkc_ui(x=k),
      server=.build_etkc_server(
         x=x,
         subSetSize=subSetSize,
         host=host
      ),
      enableBookmarking="url"
   )
}

###############################################################################@
.build_etkc_ui.chTKCat <- function(x, ...){
   
   .etkc_add_resources()
   
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
         sidebar=.etkc_sd_sidebar(sysInterface=TRUE),
         
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
   host=x$chcon@host
){
   function(input, output, session) {
      
      ########################@
      ## TKCat instance ----
      instance <- shiny::reactiveValues(
         tkcon=db_reconnect(x, user="default"),
         valid=DBI::dbIsValid(x$chcon)
      )
      shiny::observe({
         if(!instance$valid){
            instance$tkcon <- db_reconnect(
               shiny::isolate(instance$tkcon), user="default"
            )
            instance$valid <- TRUE
         }
      })
      output$instance <- shiny::renderUI({
         paste("chTKCat :", instance$tkcon$instance)
      })
      shiny::observe({
         if(instance$tkcon$chcon@user=="default"){
            sc <- "blue"
         }else{
            sc <- "yellow"
         }
         session$sendCustomMessage(
            "change_skin",
            paste0("skin-", sc)
         )
      })
      shiny::onSessionEnded(function(){
         suppressWarnings(db_disconnect(shiny::isolate(instance$tkcon)))
      })
      
      ########################@
      ## Selection status ----
      selStatus <- shiny::reactiveValues(
         resource=NULL,
         mdb=NULL,
         tables=NULL
      )
      output$status <- shiny::renderUI({
         mdb <- selStatus$mdb
         shiny::req(mdb)
         dbi <- db_info(mdb)
         shiny::tags$p(
            "Selected resource:",
            shiny::tags$strong(dbi$name),
            sprintf("(%s)", dbi$title)
         )
      })
      
      ########################@
      ## MDB list ----
      mdbs <- shiny::reactiveValues(
         list=NULL,
         collections=NULL,
         validInput=FALSE
      )
      shiny::observe({
         mdbs$list <- list_MDBs(instance$tkcon)
         mdbs$collections <- collection_members(instance$tkcon)
      })
      output$mdbList <- DT::renderDT({
         shiny::req(mdbs$list)
         toShow <- mdbs$list %>%
            dplyr::select(name, title) %>%
            dplyr::rename("Resource"="name", "Title"="title")
         cm <- mdbs$collections %>%
            dplyr::select("collection", "resource") %>%
            dplyr::distinct() %>%
            dplyr::group_by(.data$resource) %>% 
            dplyr::mutate(collection=c(.data$collection)) %>%
            dplyr::ungroup() %>%
            dplyr::rename("Collections"="collection")
         toShow <- dplyr::left_join(toShow, cm, by=c("Resource"="resource"))
         mdbs$validInput <- TRUE
         DT::datatable(
            toShow,
            rownames=FALSE,
            filter="top",
            selection = list(
               mode="single",
               selected=which(
                  toShow$Resource==shiny::isolate(selStatus$resource)
               )
            ),
            extensions='Scroller',
            options = list(
               deferRender = TRUE,
               scrollY = "70vh",
               scroller = TRUE,
               columnDefs=list(
                  list(width='60%', targets=1)
               ),
               dom=c("ti")
            )
         )
      })
      mdbListProxy <- DT::dataTableProxy("mdbList")
      shiny::observe({
         mdbListProxy %>%
            DT::selectRows(
               which(isolate(mdbs$list$name) %in% selStatus$resource)
            )
      })
      
      shiny::observe({
         shiny::req(mdbs$validInput)
         s <- input$mdbList_rows_selected
         n <- mdbs$list$name[s]
         selStatus$resource <- n
         if(length(n)==0 || n==""){
            selStatus$resource <- NULL
            selStatus$mdb <- NULL
         }else{
            selStatus$resource <- n
         }
      })
      shiny::observe({
         n <- selStatus$resource
         shiny::req(n)
         mdb <- try(get_MDB(instance$tkcon, n), silent=TRUE)
         selStatus$mdb <- mdb
         if(
            inherits(mdb, "try-error") ||
            !all(isolate(selStatus$tables) %in% names(mdb))
         ){
            selStatus$tables <- NULL
         }
      })
      shiny::observe({
         mdb <- selStatus$mdb
         shiny::req(mdb)
         tn <- isolate(selStatus$tables)
         if(!all(tn %in% names(mdb))){
            selStatus$tables <- NULL
         }
      })
      
      ########################@
      ## DB information ----
      
      output$dbInfo <- shiny::renderUI({
         mdb <- selStatus$mdb
         shiny::req(!is.null(mdb))
         if(inherits(mdb, "try-error")){
            n <- isolate(selStatus$resource)
            shiny::tagList(
               shiny::tags$p(
                  "You don't have access to",
                  shiny::strong(n),
                  '(You can sign in with different credentials)',
                  style="color:red;"
               )
            )
         }else{
            dbi <- db_info(mdb)
            dbi$records <- sum(count_records(mdb))
            shiny::tagList(
               shiny::h3(dbi$name),
               do.call(shiny::tags$ul, lapply(
                  setdiff(names(dbi), c("name", "tkcon", "table_records")),
                  function(n){
                     if(!is.na(dbi[[n]]) && dbi[[n]]!=""){
                        if(n=="url"){
                           vt <- tags$a(
                              shiny::HTML(dbi[[n]]),
                              href=dbi[[n]], target="_blank"
                           ) %>% as.character()
                        }else if(n=="maintainer"){
                           vt <- markdown::renderMarkdown(text=dbi[[n]]) %>%
                              as.character() %>%
                              gsub("<[/]?p>", "", .)
                        }else if(is.numeric(dbi[[n]])){
                           vt <- format(dbi[[n]], big.mark=",")
                        }else{
                           vt <- dbi[[n]]
                        }
                        return(shiny::tags$li(
                           shiny::strong(n), ":",
                           shiny::HTML(vt))
                        )
                     }
                  }
               )),
               shiny::tags$br(),
               shiny::downloadButton(
                  "downloadMDB", sprintf("Download %s", dbi$name)
               )
            )
         }
      })
      
      output$downloadMDB <- shiny::downloadHandler(
         filename = function() {
            n <- selStatus$resource
            shiny::req(n)
            paste0(n, ".zip")
         },
         content = function(file) {
            mdb <- selStatus$mdb
            shiny::req(mdb)
            n <- shiny::isolate(selStatus$resource)
            dbloc <- tempfile()
            as_fileMDB(mdb, path=dbloc)
            cd <- getwd()
            on.exit({
               setwd(cd)
               unlink(dbloc, recursive=TRUE)
            })
            setwd(dbloc)
            suppressMessages(utils::zip(zipfile=file, files=n, flags="-r9Xq"))
         }
      )
      
      ########################@
      ## Data model ----
      shiny::observe({
         mdb <- selStatus$mdb
         if(is.null(mdb) || inherits(mdb, "try-error")){
            session$sendCustomMessage('hideNavs', 'model')
         }else{
            n <- shiny::isolate(selStatus$resource)
            session$sendCustomMessage('showNavs', 'model')
            session$sendCustomMessage(
               'setTabLabel',
               list(
                  name="model",
                  label=htmltools::HTML(sprintf(
                     paste(
                        '<i class=" fa fa-project-diagram"></i>',
                        '%s data model'
                     ),
                     n
                  ))
               )
            )
         }
      })
      
      dbdm <- shiny::reactiveValues(
         model=NULL,
         collections=NULL,
         validInput=FALSE
      )
      shiny::observe({
         mdb <- selStatus$mdb
         if(is.chMDB(mdb)){
            dbdm$model <- data_model(mdb)
            dbdm$collections <- collection_members(mdb)
         }else{
            dbdm$model <- NULL
            dbdm$collections <- NULL
            dbdm$validInput <- FALSE
         }
      })
      
      output$dataModel <- visNetwork::renderVisNetwork({
         mdb <- selStatus$mdb
         shiny::req(mdb)
         dm <- data_model(mdb)
         dbdm$validInput <- TRUE
         nodesIdSelection <- list(enabled=TRUE, useLabels=FALSE)
         sel <- isolate(selStatus$tables)
         if(length(sel)>0){
            nodesIdSelection$selected <- sel
         }
         plot(dm) %>%
            visNetwork::visOptions(
               nodesIdSelection=nodesIdSelection,
               height="100%"
            ) 
      })
      
      shiny::observe({
         shiny::req(dbdm$validInput)
         n <- input$dataModel_selected
         mdb <- isolate(selStatus$mdb)
         shiny::req(mdb)
         if(length(n)==0 || n=="" || !all(n %in% names(mdb))){
            selStatus$tables <- NULL
         }else{
            selStatus$tables <- n
         }
      })
      
      shiny::observe({
         selTables <- selStatus$tables
         visNetworkProxy("dataModel") %>%
            visSelectNodes(selTables)
      })
      
      ########################@
      ## Collections ----
      output$collectionInfo <- shiny::renderUI({
         mdb <- selStatus$mdb
         shiny::req(mdb)
         list(
            shiny::h3("Collection members"),
            DT::DTOutput("colMembers")
         )
      })
      output$colMembers <- DT::renderDT({
         mdb <- selStatus$mdb
         shiny::req(mdb)
         cm <- collection_members(mdb)
         cm %>%
            dplyr::select(
               "collection", "id"="mid",
               "table", "field",
               "static", "value", "type"
            ) %>%
            DT::datatable(
               rownames=FALSE,
               selection = 'single',
               extensions='Scroller',
               options = list(
                  deferRender = TRUE,
                  scrollX=TRUE,
                  scrollY = 250,
                  scroller = TRUE,
                  order=list(list(0, 'asc'), list(1, 'asc')),
                  dom=c("ti")
               )
            )
      })
      shiny::observe({
         cs <- input$colMembers_rows_selected
         shiny::req(cs)
         mdb <- isolate(selStatus$mdb)
         shiny::req(mdb)
         cmt <- collection_members(mdb) %>%
            dplyr::slice(cs) %>%
            dplyr::pull(table)
         visNetwork::visNetworkProxy("dataModel") %>%
            visNetwork::visSelectNodes(id=cmt)
      })
      
      ########################@
      ## Table information ----
      output$tableInfo <- shiny::renderUI({
         mdb <- selStatus$mdb
         shiny::req(mdb)
         sel <- selStatus$tables
         shiny::req(sel)
         shiny::req(length(sel)==1)
         shiny::tagList(
            shiny::h3(sel),
            shiny::tags$ul(
               shiny::tags$li(
                  shiny::tags$strong("Number of records"),
                  ":",
                  count_records(mdb, dplyr::all_of(sel)) %>%
                     format(big.mark=","),
                  sprintf("(showing %s)", nrow(tabSubSet()))
               )
            ),
            DT::DTOutput("dataSample"),
            shiny::tags$br(),
            shiny::downloadButton(
               "downloadTable", sprintf("Download %s", sel)
            )
         )
      })
      
      tabSubSet <- reactiveVal(NULL)
      shiny::observe({
         tabSubSet(NULL)
         mdb <- selStatus$mdb
         shiny::req(mdb)
         sel <- selStatus$tables
         shiny::req(sel)
         shiny::req(length(sel)==1)
         toShow <- data_tables(mdb, dplyr::all_of(sel), n_max=subSetSize)[[1]]
         if(object.size(toShow) > 2^19){
            toShow <- toShow[
               1:max(c(1, ceiling(nrow(toShow)*(2^19/object.size(toShow))))),
            ]
         }
         tabSubSet(toShow)
      })
      output$dataSample <- DT::renderDT({
         toShow <- tabSubSet()
         shiny::req(toShow)
         DT::datatable(
            toShow,
            rownames=FALSE,
            selection = 'single',
            extensions='Scroller',
            options = list(
               deferRender = TRUE,
               scrollX=TRUE,
               scrollY = 250,
               scroller = TRUE,
               dom=c("ti")
            )
         )
      })
      
      output$downloadTable <- shiny::downloadHandler(
         filename = function() {
            sel <- selStatus$tables
            shiny::req(sel)
            shiny::req(length(sel)==1)
            paste0(sel, ".txt.gz")
         },
         content = function(file) {
            mdb <- selStatus$mdb
            shiny::req(mdb)
            sel <- selStatus$tables
            shiny::req(sel)
            shiny::req(length(sel)==1)
            readr::write_tsv(data_tables(mdb, dplyr::all_of(sel))[[1]], file)
         }
      )
      
      ########################@
      ## Search ----
      output$searchResults <- shiny::renderUI({
         shiny::tagList(
            shiny::fluidRow(
               shiny::column(
                  12,
                  shiny::uiOutput("searchResources")
               )
            ),
            shiny::fluidRow(
               shiny::column(
                  12,
                  shiny::uiOutput("searchTables")
               )
            ),
            shiny::fluidRow(
               shiny::column(
                  12,
                  shiny::uiOutput("searchFields")
               )
            )
         )
      })
      output$searchMessages <- shiny::renderUI({
         st <- input$searchInput
         shiny::req(st)
         shiny::req(c(
            input$searchResRes_rows_selected,
            input$searchTabRes_rows_selected,
            input$searchFieldRes_rows_selected
         ))
         mdb <- selStatus$mdb
         shiny::req(!is.null(mdb))
         if(inherits(mdb, "try-error")){
            n <- isolate(selStatus$resource)
            shiny::tagList(
               shiny::tags$p(
                  "You don't have access to",
                  shiny::strong(n),
                  '(You can sign in with different credentials)',
                  style="color:red;"
               )
            )
         }else{
            NULL
         }
      })
      ##
      searchRes <- shiny::reactiveValues(
         resources=NULL,
         tables=NULL,
         fields=NULL
      )
      ## _+ resources ----
      shiny::observe({
         mdbs <- mdbs$list
         shiny::req(mdbs)
         st <- input$searchInput
         shiny::req(st)
         mdbs <- mdbs %>% 
            dplyr::select("name", "title", "description", "maintainer")
         toTake <- unlist(apply(
            mdbs, 2,
            function(x){
               grep(st, x, ignore.case=T, value=FALSE)
            }
         ))
         toTake <- unique(c(0, toTake))
         toRet <- mdbs %>% dplyr::slice(toTake)
         if(nrow(toRet)>0){
            searchRes$resources <- toRet
         }else{
            searchRes$resources <- NULL
         }
      })
      output$searchResources <- shiny::renderUI({
         st <- input$searchInput
         shiny::req(st)
         shiny::tagList(
            shiny::h3("Resources"),
            DT::DTOutput("searchResRes")
         )
      })
      output$searchResRes <- DT::renderDT({
         st <- input$searchInput
         shiny::req(st)
         toRet <- searchRes$resources
         shiny::req(toRet)
         if(nrow(toRet)>0){
            toRet %>% 
               dplyr::mutate(
                  name=.highlightText(.data$name, st),
                  title=.highlightText(.data$title, st),
                  description=.highlightText(.data$description, st),
                  maintainer=.highlightText(.data$maintainer, st)
               ) %>% 
               DT::datatable(
                  rownames=FALSE,
                  escape=FALSE,
                  selection="single",
                  options=list(
                     pageLength=5,
                     dom="tip"
                  )
               )
         }else{
            NULL
         }
      })
      shiny::observe({
         sel <- input$searchResRes_rows_selected
         shiny::req(sel)
         rt <- isolate(searchRes$resources)
         shiny::req(rt)
         selStatus$resource <- rt %>% slice(sel) %>% pull("name")
      })
      ## _+ tables ----
      shiny::observe({
         mdbs <- mdbs$list
         shiny::req(mdbs)
         st <- input$searchInput
         shiny::req(st)
         selQueries <- paste(
            sprintf(
               "SELECT '%s' as resource, name, comment,",
               mdbs$name
            ),
            sprintf(
               "positionCaseInsensitive(name, '%s')>0 as s1,",
               st
            ),
            if(nchar(st)>4){
               sprintf(
                  "ngramSearchCaseInsensitive(name, '%s') as s2,",
                  st
               )
            }else{
               "0 as s2,"
            },
            sprintf(
               "if(isNull(comment), 0, positionCaseInsensitive(comment, '%s')>0) as s3,",
               st
            ),
            if(nchar(st)>4){
               sprintf(
                  "if(isNull(comment), 0, ngramSearchCaseInsensitive(comment, '%s')) as s4,",
                  st
               )
            }else{
               "0 as s4,"
            },
            "greatest(s4, greatest(s3, greatest(s2, s1))) as ms",
            sprintf("FROM `%s`.`___Tables___`", mdbs$name),
            "WHERE ms > 0"
         )
         query <- paste(selQueries, collapse=" UNION ALL ")
         toRet <- DBI::dbGetQuery(instance$tkcon$chcon, query)
         if(nrow(toRet)>0){
            searchRes$tables <- toRet %>% 
               dplyr::as_tibble() %>%
               dplyr::arrange(desc(ms)) %>%
               dplyr::select("resource", "name", "comment")
         }else{
            searchRes$tables <- NULL
         }
      })
      output$searchTables <- shiny::renderUI({
         st <- input$searchInput
         shiny::req(st)
         shiny::tagList(
            shiny::h3("Tables"),
            DT::DTOutput("searchTabRes")
         )
      })
      output$searchTabRes <- DT::renderDT({
         st <- input$searchInput
         shiny::req(st)
         toRet <- searchRes$tables
         shiny::req(toRet)
         if(nrow(toRet)>0){
            toRet %>%
               dplyr::mutate(
                  # resource=.highlightText(.data$resource, st),
                  name=.highlightText(.data$name, st),
                  comment=.highlightText(.data$comment, st)
               ) %>%
               DT::datatable(
                  rownames=FALSE,
                  escape=FALSE,
                  selection="single",
                  filter="top",
                  options=list(
                     pageLength=5,
                     dom="tip"
                  )
               )
         }else{
            NULL
         }
      })
      shiny::observe({
         sel <- input$searchTabRes_rows_selected
         shiny::req(sel)
         rt <- isolate(searchRes$tables)
         shiny::req(rt)
         selStatus$resource <- rt %>% slice(sel) %>% pull("resource")
         selStatus$tables <- rt %>% slice(sel) %>% pull("name")
      })
      ## _+ fields ----
      shiny::observe({
         mdbs <- mdbs$list
         shiny::req(mdbs)
         st <- input$searchInput
         shiny::req(st)
         selQueries <- paste(
            sprintf(
               "SELECT '%s' as resource, table, name, type, nullable, unique, comment,",
               mdbs$name
            ),
            sprintf(
               "positionCaseInsensitive(name, '%s')>0 as s1,",
               st
            ),
            if(nchar(st)>4){
               sprintf(
                  "ngramSearchCaseInsensitive(name, '%s') as s2,",
                  st
               )
            }else{
               "0 as s2,"
            },
            sprintf(
               "if(isNull(comment), 0, positionCaseInsensitive(comment, '%s')>0) as s3,",
               st
            ),
            if(nchar(st)>4){
               sprintf(
                  "if(isNull(comment), 0, ngramSearchCaseInsensitive(comment, '%s')) as s4,",
                  st
               )
            }else{
               "0 as s4,"
            },
            "greatest(s4, greatest(s3, greatest(s2, s1))) as ms",
            sprintf("FROM `%s`.`___Fields___`", mdbs$name),
            "WHERE ms > 0"
         )
         query <- paste(selQueries, collapse=" UNION ALL ")
         toRet <- DBI::dbGetQuery(instance$tkcon$chcon, query)
         if(nrow(toRet)>0){
            searchRes$fields <- toRet %>% 
               dplyr::as_tibble() %>%
               dplyr::arrange(desc(ms)) %>%
               dplyr::select(
                  "resource", "table", "name", "comment",
                  "type", "nullable", "unique"
               ) %>%
               dplyr::mutate(
                  nullable=as.logical(.data$nullable),
                  unique=as.logical(.data$unique)
               )
         }else{
            searchRes$fields <- NULL
         }
      })
      output$searchFields <- shiny::renderUI({
         st <- input$searchInput
         shiny::req(st)
         shiny::tagList(
            shiny::h3("Fields"),
            DT::DTOutput("searchFieldRes")
         )
      })
      output$searchFieldRes <- DT::renderDT({
         st <- input$searchInput
         shiny::req(st)
         toRet <- searchRes$fields
         shiny::req(toRet)
         if(nrow(toRet)>0){
            toRet %>%
               dplyr::mutate(
                  # resource=.highlightText(.data$resource, st),
                  # table=.highlightText(.data$resource, st),
                  name=.highlightText(.data$name, st),
                  comment=.highlightText(.data$comment, st)
               ) %>%
               DT::datatable(
                  rownames=FALSE,
                  escape=FALSE,
                  selection="single",
                  filter="top",
                  options=list(
                     pageLength=5,
                     dom="tip"
                  )
               )
         }else{
            NULL
         }
      })
      shiny::observe({
         sel <- input$searchFieldRes_rows_selected
         shiny::req(sel)
         rt <- isolate(searchRes$fields)
         shiny::req(rt)
         selStatus$resource <- rt %>% slice(sel) %>% pull("resource")
         selStatus$tables <- rt %>% slice(sel) %>% pull("table")
      })
      
      ########################@
      ## System information ----
      output$systemInfo <- shiny::renderUI({
         k <- instance$tkcon
         shiny::tagList(
            shiny::tags$ul(
               shiny::tags$li(
                  shiny::tags$strong("Host"), ":", host
               ),
               shiny::tags$li(
                  shiny::tags$strong("Native port"), ":", k$chcon@port
               ),
               shiny::tags$li(
                  shiny::tags$strong("HTTP port"), ":", k$http
               ),
               shiny::tags$li(
                  shiny::tags$strong("Current user"), ":", k$chcon@user
               ),
               shiny::tags$li(
                  shiny::tags$strong("Instance"), ":", k$instance,
                  sprintf("(Version %s)", k$version)
               ),
               shiny::tags$li(
                  shiny::HTML(markdown::renderMarkdown(
                     text=paste(
                        shiny::tags$strong("Administrator"), ":",
                        k$contact
                     )
                  ))
               )
            )
         )
      })
      
      ########################@
      ## Sign in ----
      output$signin <- shiny::renderUI({
         shiny::actionLink(
            inputId="silink",
            label=shiny::span("Sign in", style="margin-left:6px;"),
            icon=shiny::icon("sign-in-alt"),
            style="margin:0;"
         )
      })
      shiny::observeEvent(input$silink, {
         okConnect(TRUE)
         showModal(modalDialog(
            title="Sign in",
            shiny::div(
               shiny::fluidRow(
                  shiny::column(
                     8,
                     shiny::textInput(
                        "userName", label=NULL, width="100%",
                        placeholder="User name"
                     ),
                     shiny::passwordInput(
                        "password", label=NULL, width="100%",
                        placeholder="Password"
                     ),
                  ),
                  shiny::column(4, shiny::actionButton("connect", "Connect"))
               ),
               shiny::fluidRow(shiny::column(12,
                                             shiny::uiOutput("notOkConnect"),
                                             paste(
                                                "Contact",
                                                "if you've forgotten your password",
                                                "or if you want to sign up:"
                                             ),
                                             shiny::HTML(markdown::renderMarkdown(
                                                text=shiny::isolate(instance$tkcon$contact)
                                             ))
               ))
            ),
            size="s",
            easyClose=TRUE
         ))
      })
      okConnect <- shiny::reactiveVal(TRUE)
      output$notOkConnect <- shiny::renderUI({
         shiny::req(!okConnect())
         shiny::tagList(
            shiny::strong("Invalid credentials", style="color:red;"),
         )
      })
      shiny::observeEvent(input$connect, {
         u <- shiny::isolate(input$userName)
         p <- shiny::isolate(input$password)
         nk <- try(db_reconnect(
            shiny::isolate(instance$tkcon),
            user=u, password=p
         ), silent=TRUE)
         if(!inherits(nk, "try-error")){
            suppressWarnings(db_disconnect(instance$tkcon))
            instance$tkcon <- nk
            okConnect(TRUE)
            shiny::removeModal()
         }else{
            instance$tkcon <- db_reconnect(instance$tkcon, user="default")
            okConnect(FALSE)
         }
      })
      
      ########################@
      ## Sign out ----
      output$currentUser <- shiny::renderUI({
         shiny::actionLink(
            inputId=ifelse(
               instance$tkcon$chcon@user!="default",
               "solink",
               "disabledSoLink"
            ),
            label=shiny::span(
               shiny::HTML(paste(c(
                  ifelse(
                     instance$tkcon$chcon@user!="default",
                     instance$tkcon$chcon@user,
                     "Public access"
                  ),
                  ifelse(
                     instance$tkcon$chcon@user!="default",
                     as.character(shiny::span(
                        shiny::icon("sign-out-alt"),
                        style="margin-left:6px;"
                     )),
                     ""
                  )
               ), collapse=" ")),
               style="margin-left:6px;"
            ),
            icon=shiny::icon(
               ifelse(
                  instance$tkcon$chcon@user!="default",
                  "user",
                  "user-slash"
               )
            ),
            style="margin:0;",
            title=ifelse(
               instance$tkcon$chcon@user!="default",
               "Sign out",
               ""
            )
         )
      })
      shiny::observeEvent(input$solink, {
         suppressWarnings(db_disconnect(instance$tkcon))
         instance$valid <- FALSE
      })
      shiny::observe({
         k <- instance$tkcon
         if(k$chcon@user=="default"){
            session$sendCustomMessage('showNavs', 'signinTab')
         }else{
            session$sendCustomMessage('hideNavs', 'signinTab')
         }
      })
      
      ########################@
      ## Bookmarks ----
      shiny::observe({
         # Trigger this observer every time an input changes
         shiny::reactiveValuesToList(input)
         session$doBookmark()
      })
      shiny::onBookmark(function(state) {
         state$values$resource <- selStatus$resource
         state$values$tables <- selStatus$tables
      })
      shiny::onBookmarked(function(url){
         shiny::updateQueryString(url)
      })
      shiny::onRestore(function(state) {
         selStatus$resource <- state$values$resource
         selStatus$tables <- state$values$tables
      })
      
   }
}
