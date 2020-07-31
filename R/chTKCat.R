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
#' @param http an integer specifying the HTTP port of the 
#' ClickHouse database (default: 9111). Used for documentation only.
#' @param user user name
#' @param password user password
#'
#' @return a chTKCat object
#'
#' @seealso [check_chTKCat()], [disconnect_chTKCat()]
#' 
#' @export
#'
chTKCat <- function(
   host="localhost",
   port=9101L,
   http=9111L,
   user="default",
   password
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
   chMDBs <- list_chMDBs(x, withInfo=FALSE)
   
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
#' Disconnect from a [chTKCat] instance
#'
#' @param x a [chTKCat] object
#' 
#' @export
#'
disconnect_chTKCat <- function(x){
   stopifnot(inherits(x, "chTKCat"))
   RClickhouse::dbDisconnect(x[["chcon"]])
   invisible()
}

###############################################################################@
#' Reconnect to a [chTKCat] instance
#'
#' @param x a [chTKCat] object
#' @param ... further arguments for [ch_reconnect]:
#' - **user**: user name. If not provided, it's taken from x
#' - **password**: user password. If not provided, first the function
#' tries to connect without any password. If it fails, the function asks the
#' user to provide a password.
#' - **ntries**: the number of times the user can enter a wrong password
#' 
#' @export
#'
reconnect_chTKCat <- function(x, ...){
   stopifnot(inherits(x, "chTKCat"))
   xn <- deparse(substitute(x))
   con <- x$chcon
   ch_reconnect(con, ...)
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
#' @importFrom getPass getPass
#' @importFrom utils packageName
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
   disconnect_chTKCat(x)
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
      for(db in list_chMDBs(x, withInfo=FALSE)){
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
   allDb <- list_chMDBs(x, withInfo=FALSE)
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
#' List available databases in a [chTKCat]
#' 
#' @param x a [chTKCat] object
#' @param withInfo if TRUE (default), it returns only initialized MDBs along
#' with general information. If FALSE, it returns all the available databases
#' in the system (excepted those reserved by the system).
#' 
#' @export
#' 
list_chMDBs <- function(x, withInfo=TRUE){
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
         if("___MDB___" %in% dbTables){
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
      stop("Only chTKCat admin can add users")
   }
   if(name %in% list_chMDBs(x, withInfo=FALSE)){
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
   add_chMDB_user(x, x$chcon@user, name, admin=TRUE)
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
   if(!name %in% list_chMDBs(x, withInfo=FALSE)){
      stop("The database does not exist")
   }
   if(!x$admin){
      stop("Only chTKCat admin can add users")
   }
   con <- x$chcon
   RClickhouse::dbSendQuery(con, sprintf("DROP DATABASE %s", name))
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
   if(!mdb %in% list_chMDBs(x, withInfo=FALSE)){
      stop("The database does not exist")
   }
   con <- x$chcon
   toRet <- DBI::dbGetQuery(
      con, 
      sprintf("SELECT public from `%s`.Public", mdb)
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
   if(!mdb %in% list_chMDBs(x, withInfo=FALSE)){
      stop("The database does not exist")
   }
   con <- x$chcon
   RClickhouse::dbSendQuery(
      con, 
      sprintf(
         "ALTER TABLE `%s`.Public DELETE WHERE 1",
         mdb,
         as.integer(public)
      )
   )
   ch_insert(con, dbName=mdb, tableName="Public", value=tibble(public=public))
   users <- list_chTKCat_users(x) %>% 
      dplyr::filter(!.data$admin) %>% 
      dplyr::pull("login")
   chMDBusers <- list_chMDB_users(x, mdb)$login
   dbTables <- DBI::dbGetQuery(con, sprintf("SHOW TABLES FROM %s", mdb)) %>% 
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
      mdb %in% list_chMDBs(x, withInfo=FALSE)
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
   if(!mdb %in% list_chMDBs(x, withInfo=FALSE)){
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
      dbTables <- DBI::dbGetQuery(con, sprintf("SHOW TABLES FROM %s", mdb)) %>% 
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
   if(!mdb %in% list_chMDBs(x, withInfo=FALSE)){
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
            con, sprintf("SHOW TABLES FROM %s", mdb)
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
#### COLLECTIONS (TODO) ####
###############################################################################@


###############################################################################@
#' List collections available in a [chTKCat]
#' 
#' @param x a [chTKCat] object
#' 
#' @export
#' 
list_chTKCat_collections <- function(x){
   stopifnot(is.chTKCat(x))
   dbGetQuery(
      conn=x$chcon,
      statement="SELECT title, description FROM default.Collections"
   ) %>%
      as_tibble() %>% 
      return()
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
      env <- environment()
      raw <- tkcatEnv$COLLECTIONS %>%
         dplyr::filter(.data$title==get("json", env)) %>%
         dplyr::pull("json")
   }else{
      raw <- json
   }
   if(!jsonvalidate::json_validate(raw, tkcatEnv$COL_SCHEMA, verbose=TRUE)){
      stop("Not a valid collection")
   }
   def <- jsonlite::fromJSON(raw)
   ctitle <- def$properties$collection$enum 
   if(
      ctitle %in% list_chTKCat_collections(x)$title &&
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
#' @param x a [chTKCat] object
#' @param ... names of the collections
#' to focus on. By default, all of them are taken.
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
   #### TODO ####
   stop("TODO")
   
   toTake <- unlist(list(...))
   
   stopifnot(
      length(toTake)==0 || is.character(toTake)
   )
   toRet <- DBI::dbGetQuery(
      conn=x$chcon,
      statement=
         sprintf(
            "SELECT * FROM default.CollectionMembers %s",
            if(length(toTake)==0){
               ""
            }else{
               sprintf(
                  "WHERE collection IN ('%s')",
                  paste(toTake, collapse="', '")
               )
            }
         )
   ) %>%
      dplyr::as_tibble() %>%
      dplyr::select(
         "collection", "resource", "cid",
         "table", "field", "static", "value", "type"
      ) %>%
      dplyr::mutate(static=as.logical(.data$static))
   return(toRet)
}
