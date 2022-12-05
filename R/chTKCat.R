###############################################################################@
#### chTKCat object ####
###############################################################################@


###############################################################################@
#' Connect to a ClickHouse TKCat instance
#'
#' @param host a character string specifying the host heberging the 
#' database (default: localhost)
#' @param port an integer specifying the port on which the 
#' database is listening (default: 9111)
#' @param user user name
#' @param password user password
#' @param settings list of
#' [Clickhouse 
#' settings](https://clickhouse.com/docs/en/operations/settings/settings/)
#' @param ports a named list of available ports for accessing ClickHouse
#' (default: NULL; example: `c(Native=9101, HTTP=9111)`)
#' @param drv a DBI driver for connecting to ClickHouse
#' (default: [ClickHouseHTTP::ClickHouseHTTP()];
#' other supported driver: [RClickhouse::clickhouse()])
#' @param ... additional parameters for connection
#' (see [ClickHouseHTTP::dbConnect,ClickHouseHTTPDriver-method]
#' for the default driver)
#'
#' @return a chTKCat object
#'
#' @seealso [check_chTKCat()], [db_disconnect()], [db_reconnect()]
#' 
#' @export
#'
chTKCat <- function(
   host="localhost",
   port=9111L,
   user="default",
   password,
   settings=list(
      # Which part of the query can be read into RAM for parsing
      # (the remaining data for INSERT, if any, is read later)
      "max_query_size"=1073741824,
      # Whether to use the cache of uncompressed blocks. Zero means FALSE.
      "use_uncompressed_cache"=0,
      # Which replicas (among healthy replicas) to preferably send
      # a query to (on the first attempt) for distributed processing.
      "load_balancing"="random",
      # Maximum memory usage for processing of single query.
      # Zero means unlimited.
      "max_memory_usage"=0,
      # Enabling introspection functions for GRANT access. Zero means FALSE.
      "allow_introspection_functions"=1,
      # Force joined subqueries and table functions to have aliases for correct
      # name qualification. Zero means FALSE.
      "joined_subquery_requires_alias"=0
   ),
   ports=NULL,
   drv=ClickHouseHTTP::ClickHouseHTTP(),
   ...
){
   
   if(!class(drv) %in% c("ClickhouseDriver", "ClickHouseHTTPDriver")){
      stop(
         "Only ClickhouseDriver from RClickhouse package",
         " and ClickHouseHTTPDriver from ClickHouseHTTP package",
         " are supported."
      )
   }
   
   if(missing(password)){
      password <- getPass::getPass(
         sprintf("%s password (press escape or cancel if no password)", user),
         noblank=TRUE
      )
      if(is.null(password)){
         password=""
      }
   }
   
   chcon <- DBI::dbConnect(
      drv=drv,
      host=host,
      port=port,
      user=user, password=ifelse(is.na(password), "", password),
      ...
   )
   
   
   for(s in names(settings)){
      DBI::dbSendQuery(chcon, sprintf("SET %s='%s'", s, settings[[s]]))
   }
   
   toRet <- list(
      chcon=chcon,
      settings=settings,
      ports=ports,
      drv=drv,
      cpar=list(...)
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
      "SELECT name FROM system.tables WHERE database='default'",
      format="TabSeparatedWithNamesAndTypes"
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
      ui <- try(DBI::dbGetQuery(
         con,
         sprintf(
            "SELECT admin, provider FROM default.Users WHERE login='%s'",
            con@user
         ),
         format="TabSeparatedWithNamesAndTypes"
      ), silent=TRUE)
      if(inherits(ui, "try-error")){
         ui <- DBI::dbGetQuery(
            con,
            sprintf(
               "SELECT admin FROM default.Users WHERE login='%s'",
               con@user
            ),
            format="TabSeparatedWithNamesAndTypes"
         )
      }
      admin <- as.logical(ui$admin)
      if("provider" %in% colnames(ui)){
         provider <- as.logical(ui$provider)
      }else{
         provider <- as.logical(NA)
      }
      
      ## System information
      dbSys <- DBI::dbGetQuery(
         con,
         sprintf(
            "SELECT %s FROM default.System",
            ifelse(
               admin, "*",
               "name, instance, version, contact"
            )
         ),
         format="Arrow"
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
      toRet$instance <- dbSys[["instance"]]
      toRet$version <- dbSys[["version"]]
      toRet$contact <- dbSys[["contact"]]
      toRet$path <- dbSys[["path"]]
      toRet$admin <- admin
      toRet$provider <- provider
   }
   for(
      i in setdiff(
         intersect(names(x), names(toRet)),
         c("path", "admin", "provider")
      )
   ){
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
   toRet <- paste(
      toRet,
      sprintf(
         "   - ClickHouse: %s",
         get_query(
            x,
            paste(
               "SELECT value FROM system.build_options ",
               "WHERE name='VERSION_DESCRIBE'"
            ),
            format="TabSeparatedWithNamesAndTypes"
         )$value
      ),
      sep="\n"
   )
   if(x$init){
      toRet <- do.call(paste, c(
         list(
            toRet,
            sprintf("   - Instance: %s", x$instance),
            sprintf("   - Version: %s", x$version),
            sprintf("   - Instance contact: %s", x$contact),
            sprintf("   - User: %s", x$chcon@user),
            "",
            sprintf(
                    "   - R DBI Driver: %s%s",
               class(x$drv),
               ifelse(
                  is.null(attr(class(x$drv), "package")),
                  "",
                  sprintf(" (%s package)", attr(class(x$drv), "package"))
               )
            )
         ),
         lapply(names(x$cpar), function(p){
            sprintf("      + %s: %s", p, x$cpar[[p]])
         }),
         list(
            sep="\n"
         )
      ))
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
   DBI::dbDisconnect(x[["chcon"]])
   invisible()
}

###############################################################################@
#' 
#' @rdname db_reconnect
#' @method db_reconnect chTKCat
#' 
#' @export
#'
db_reconnect.chTKCat <- function(x, user, password, ntries=3, ...){
   xn <- deparse(substitute(x))
   con <- x$chcon
   
   if(missing(user)){
      user <- con@user
   }
   suppressWarnings(try(DBI::dbDisconnect(con), silent=TRUE))
   if(missing(password)){
      ncon <- try(do.call(DBI::dbConnect, c(
         list(
            drv=x$drv,
            host=con@host,
            port=con@port,
            user=user,
            password=""
         ),
         x$cpar
      )), silent=TRUE)
      n <- 0
      while(inherits(ncon, "try-error") & n < ntries){
         password <- getPass::getPass(
            msg=paste0(user, " password on ", con@host, ":", con@port)
         )
         if(is.null(password)){
            stop("Canceled by the user")
         }
         ncon <- try(do.call(DBI::dbConnect, c(
            list(
               drv=x$drv,
               host=con@host,
               port=con@port,
               user=user,
               password=password
            ),
            x$cpar
         )), silent=TRUE)
         n <- n+1
      }
      if(inherits(ncon, "try-error")){
         stop(as.character(ncon))
      }
   }else{
      ncon <- try(do.call(DBI::dbConnect, c(
         list(
            drv=x$drv,
            host=con@host,
            port=con@port,
            user=user,
            password=password
         ),
         x$cpar
      )), silent=TRUE)
   }
   for(s in names(x$settings)){
      DBI::dbSendQuery(ncon, sprintf("SET %s='%s'", s, x$settings[[s]]))
   }
   
   nv <- x
   nv$chcon <- ncon
   nv <- check_chTKCat(nv)
   assign(xn, nv, envir=parent.frame(n=1))
   invisible(nv)
}


###############################################################################@
#' 
#' @rdname get_hosts
#' @method get_hosts chTKCat
#' 
#' @export
#'
get_hosts.chTKCat <- function(x, ...){
   get_hosts(x$chcon)
}


###############################################################################@
#' 
#' @param ... Additional parameters for [dbGetQuery()] function.
#' For the ClickHouseHTTP DBI, `format` can be set to "Arrow" (default) or
#' "TabSeparatedWithNamesAndTypes"
#' (see [ClickHouseHTTP::dbSendQuery,ClickHouseHTTPConnection,character-method])
#' 
#' @rdname get_query
#' @method get_query chTKCat
#' 
#' @export
#'
get_query.chTKCat <- function(x, query, ...){
   DBI::dbGetQuery(x$chcon, query, ...) %>%
      dplyr::as_tibble()
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
#'
#' @return a [chTKCat]
#' 
#' @export
#'
init_chTKCat <- function(
   x, instance, version, path,
   login, password, contact
){
   con <- x$chcon
   ## Check that ClickHouse is empty and ready for initialization ----
   check_chTKCat(x)
   if(con@host!="localhost"){
      stop("Initialisation can only be done for localhost instances")
   }
   defaultTables <- DBI::dbGetQuery(
      con,
      "SELECT name from system.tables WHERE database='default'",
      format="TabSeparatedWithNamesAndTypes"
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
   TKCAT_USERS <- file.path(path, "conf/users.xml")
   if(!file.exists(TKCAT_USERS)){
      stop(sprintf("%s does not exist", TKCAT_USERS))
   }
   if(missing(password)){
      password <- .create_password(login)
   }
   stopifnot(
      is.character(password), length(password)==1
   )

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
   uf <- xml2::read_xml(TKCAT_USERS)
   users <- xml2::xml_children(uf)[which(
      xml2::xml_name(xml2::xml_children(uf))=="users"
   )]
   if(length(users)!=1){
      stop("Cannot find <users> in users.xml config")
   }
   xml2::xml_replace(users, xml2::read_xml("<users></users>"))
   xml2::write_xml(uf, file=TKCAT_USERS)
   Sys.sleep(3)
   x <- do.call(chTKCat, c(
      list(
         host=con@host,
         port=con@port,
         user=login,
         password=password,
         settings=x$settings,
         ports=x$ports,
         drv=x$drv
      ),
      x$cpar
   ))
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
   uf <- DBI::dbGetQuery(
      con,
      paste(
         "SELECT name FROM system.columns",
         "WHERE database='default' AND table='Users'"
      ),
      format="TabSeparatedWithNamesAndTypes"
   ) %>%
      dplyr::pull("name")
   toRet <- DBI::dbGetQuery(
      con,
      sprintf(
         "SELECT %s FROM default.Users",
         ifelse(
            x$admin, "*",
            paste(intersect(uf, c("login", "admin", "provider")), collapse=", ")
         )
      ),
      format="Arrow"
   ) %>% 
      dplyr::as_tibble()
   if("admin" %in% colnames(toRet)){
      toRet$admin <- as.logical(toRet$admin)
   }
   if("provider" %in% colnames(toRet)){
      toRet$provider <- as.logical(toRet$provider)
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
#' @param admin a logical indicating if the user is an admin of the chTKCat
#' instance (default: TRUE)
#' @param provider a logical indicating if the user is data provider (TRUE)
#' or a data consumer (FALSE: default). If admin
#' is set to TRUE provider will be set to TRUE
#' 
#' @return No return value, called for side effects
#' 
#' @export
create_chTKCat_user <- function(
   x, login, password, contact, admin=FALSE, provider=admin
){
   contact <- as.character(contact)
   valid_email <- function(x){
      toRet <- rep(FALSE, length(x))
      toRet[grep(
         "^[[:alnum:].!#$%&'*+/=?^_`{|}~-]+@[[:alnum:]-]+(?:[.][[:alnum:]-]+)*$",
         x
      )] <- TRUE
      return(toRet)
   }
   stopifnot(
      is.chTKCat(x),
      is.character(login), length(login)==1, !is.na(login),
      valid_email(login) || length(grep("[^[:alnum:]_]", login))==0,
      is.character(contact), length(contact)==1,
      is.logical(admin), length(admin)==1, !is.na(admin),
      is.logical(provider), length(provider)==1, !is.na(provider)
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
   DBI::dbSendQuery(
      con, 
      sprintf(
         "CREATE USER '%s' %s",
         login,
         ifelse(
            is.na(password),
            "IDENTIFIED WITH no_password",
            sprintf("IDENTIFIED BY '%s'", password)
         )
      )
   )
   
   ## Register the user ----
   if(admin){
      provider=TRUE
   }
   ch_insert(
      con, "default", "Users", dplyr::tibble(
         login=login,
         contact=as.character(contact),
         admin=admin,
         provider=provider
      )
   )
   
   ## Grant access ----
   if(admin){
      DBI::dbSendQuery(
         con,
         sprintf(
            "GRANT ALL ON *.* TO '%s' WITH GRANT OPTION",
            login
         )
      )
   }else{
      
      DBI::dbSendQuery(
         con,
         sprintf(
            "REVOKE ALL ON *.* FROM '%s'",
            login
         )
      )
      
      if(provider){
         DBI::dbSendQuery(
            con, 
            sprintf(
               paste(
                  "GRANT SELECT,",
                  " CREATE DATABASE, CREATE TABLE, DROP TABLE, ALTER, INSERT",
                  " ON *.* TO '%s' WITH GRANT OPTION"
               ),
               login
            )
         )
         
         DBI::dbSendQuery(
            con,
            sprintf(
               "REVOKE ALL ON default.* FROM '%s'",
               login
            )
         )
         DBI::dbSendQuery(
            con,
            sprintf(
               "REVOKE ALL ON system.* FROM '%s'",
               login
            )
         )
         
      }
      
      DBI::dbSendQuery(
         con, sprintf("GRANT SHOW DATABASES ON *.* TO '%s'", login)
      )
      DBI::dbSendQuery(
         con, sprintf("GRANT SHOW TABLES ON *.* TO '%s'", login)
      )
      DBI::dbSendQuery(
         con, sprintf("GRANT SHOW COLUMNS ON *.* TO '%s'", login)
      )
      DBI::dbSendQuery(
         con,
         sprintf(
            paste(
               "GRANT SELECT(name, instance, version, contact)",
               "ON default.System TO '%s'"
            ),
            login
         )
      )
      DBI::dbSendQuery(
         con, sprintf("GRANT SELECT ON default.Collections TO '%s'", login)
      )
      DBI::dbSendQuery(
         con,
         sprintf(
            "GRANT SELECT(login, admin, provider) ON default.Users TO '%s'",
            login
         )
      )
   }
   for(db in list_MDBs(x, withInfo=TRUE)$name){
      update_chMDB_grants(x, db)
   }
   
   invisible()
}

###############################################################################@
#' Change chTKCat password
#' 
#' @param x a [chTKCat] object
#' @param login user login
#' @param password new user password
#' 
#' @return No return value, called for side effects
#' 
#' @export
change_chTKCat_password <- function(
   x, login, password
){
   
   stopifnot(
      is.chTKCat(x),
      is.character(login), length(login)==1, !is.na(login)
   )
   if(!x$admin){
      stop("Only chTKCat admin can change user information")
   }
   if(!login %in% list_chTKCat_users(x)$login){
      stop("The user does not exist")
   }
   
   if(missing(password)){
      password <- .create_password(login)
   }
   password <- as.character(password)
   stopifnot(
      is.character(password), length(password)==1
   )
   con <- x$chcon
   ## Alter the user in ClickHouse ----
   DBI::dbSendQuery(
      con, 
      sprintf(
         "ALTER USER '%s' %s",
         login,
         ifelse(
            is.na(password),
            "IDENTIFIED WITH no_password",
            sprintf("IDENTIFIED BY '%s'", password)
         )
      )
   )
   
   invisible()
}

###############################################################################@
#' Update a chTKCat user information
#' 
#' @param x a [chTKCat] object
#' @param login user login
#' @param contact contact information (can be NA)
#' @param admin a logical indicating if the user is an admin of the chTKCat
#' instance
#' @param provider a logical indicating if the user is data provider (TRUE)
#' or a data consumer (FALSE: default)
#' 
#' @return No return value, called for side effects
#' 
#' @export
update_chTKCat_user <- function(
   x, login, contact, admin, provider
){
   
   stopifnot(
      is.chTKCat(x),
      is.character(login), length(login)==1, !is.na(login)
   )
   if(!x$admin){
      stop("Only chTKCat admin can change user information")
   }
   if(!login %in% list_chTKCat_users(x)$login){
      stop("The user does not exist")
   }
   
   if(!missing(contact) || !missing(admin) || !missing(provider)){
      
      con <- x$chcon
      
      new_val <- get_query(
         x,
         sprintf("SELECT * FROM default.Users WHERE login='%s'", login),
         format="Arrow"
      )
      new_val$admin <- as.logical(new_val$admin)
      new_val$provider <- as.logical(new_val$provider)
      
      ## Contact information ----
      if(!missing(contact)){
         contact <- as.character(contact)
         stopifnot(is.character(contact), length(contact)==1)
         new_val$contact <- contact
      }
      
      ## Admin right ----
      updateGrants <- FALSE
      if(!missing(provider)){
         stopifnot(is.logical(provider), length(provider)==1, !is.na(provider))
         new_val$provider <- provider
         updateGrants <- TRUE
      }
      if(!missing(admin)){
         stopifnot(is.logical(admin), length(admin)==1, !is.na(admin))
         new_val$admin <- admin
         if(admin){
            new_val$provider <- TRUE
         }
         updateGrants <- TRUE
      }
      provider <- new_val$provider
      admin <- new_val$admin

      ## Update the value in 2 steps because of issues with Clickhouse UPDATE 
      DBI::dbSendQuery(
         con,
         sprintf(
            "ALTER TABLE default.Users DELETE WHERE login='%s'",
            login
         )
      )
      ch_insert(con, "default", "Users", new_val)
      
      ## Update GRANTs if necessary ----
      if(updateGrants){
         if(admin){
            DBI::dbSendQuery(
               con,
               sprintf(
                  "GRANT ALL ON *.* TO '%s' WITH GRANT OPTION",
                  login
               )
            )
         }else{
            
            DBI::dbSendQuery(
               con,
               sprintf(
                  "REVOKE ALL ON *.* FROM '%s'",
                  login
               )
            )
            
            if(provider){
               DBI::dbSendQuery(
                  con, 
                  sprintf(
                     paste(
                        "GRANT SELECT,",
                        " CREATE DATABASE, CREATE TABLE, DROP TABLE,",
                        " ALTER, INSERT",
                        " ON *.* TO '%s' WITH GRANT OPTION"
                     ),
                     login
                  )
               )
               
               DBI::dbSendQuery(
                  con,
                  sprintf(
                     "REVOKE ALL ON default.* FROM '%s'",
                     login
                  )
               )
               DBI::dbSendQuery(
                  con,
                  sprintf(
                     "REVOKE ALL ON system.* FROM '%s'",
                     login
                  )
               )
               
            }
            
            DBI::dbSendQuery(
               con, sprintf("GRANT SHOW DATABASES ON *.* TO '%s'", login)
            )
            DBI::dbSendQuery(
               con, sprintf("GRANT SHOW TABLES ON *.* TO '%s'", login)
            )
            DBI::dbSendQuery(
               con, sprintf("GRANT SHOW COLUMNS ON *.* TO '%s'", login)
            )
            DBI::dbSendQuery(
               con,
               sprintf(
                  paste(
                     "GRANT SELECT(name, instance, version, contact)",
                     "ON default.System TO '%s'"
                  ),
                  login
               )
            )
            DBI::dbSendQuery(
               con, sprintf("GRANT SELECT ON default.Collections TO '%s'", login)
            )
            DBI::dbSendQuery(
               con,
               sprintf(
                  "GRANT SELECT(login, admin, provider) ON default.Users TO '%s'",
                  login
               )
            )
         }
         for(db in list_MDBs(x, withInfo=TRUE)$name){
            update_chMDB_grants(x, db)
         }
      }
      
   }
   
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
   if(login==con@user){
      stop("You cannot drop yourself")
   }
   allDb <- list_MDBs(x, withInfo=FALSE)
   for(mdb in allDb){
      remove_chMDB_user(x=x, login=login, mdb=mdb)
   }
   DBI::dbSendQuery(
      con,
      sprintf("ALTER TABLE default.Users DELETE WHERE login='%s'", login)
   )
   DBI::dbSendQuery(
      con,
      sprintf("DROP USER '%s'", login)
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
   dbNames <- DBI::dbGetQuery(
      con,
      sprintf(
         "SELECT name, database FROM system.tables WHERE name IN ('%s')",
         paste(c("___MDB___", "___Public___"), collapse="', '")
      ),
      format="TabSeparatedWithNamesAndTypes"
   )
   if(nrow(dbNames) > 0){
      dbNames <- dbNames %>%
         dplyr::group_by(.data$database) %>% 
         dplyr::summarise(n=n()) %>% 
         dplyr::ungroup() %>% 
         dplyr::filter(.data$n==2) %>% 
         dplyr::pull("database") %>% 
         setdiff(CH_RESERVED_DB)
   }else{
      dbNames <- character(0)
   }
   if(!withInfo){
      return(dbNames)
   }else{
      accessLevels <- c("none", "read only", "write and read")
      if(length(dbNames)==0){
         toRet <- dplyr::tibble(
            name=character(),
            title=character(),
            description=character(),
            url=character(),
            version=character(),
            maintainer=character(),
            public=logical(),
            populated=logical(),
            access=factor(c(), levels=accessLevels)
         )
      }else{
         mdbDesc <- DBI::dbGetQuery(
            con,
            paste(
               'SELECT * FROM (',
               paste(
                  sprintf(
                     paste(
                        "SELECT '%s' AS db, * FROM `%s`.___MDB___ ",
                        " FULL JOIN ",
                        " (SELECT '%s' AS db, * FROM `%s`.___Public___)",
                        "USING db"
                     ),
                     dbNames, dbNames, dbNames, dbNames
                  ),
                  collapse=" UNION ALL "
               ),
               ") LEFT JOIN ",
               " (SELECT database AS db, total_rows FROM system.tables ",
               " WHERE name='___Timestamps___') USING db"
            ),
            format="Arrow"
         ) %>% 
            dplyr::as_tibble() %>% 
            dplyr::mutate(
               public=as.logical(.data$public),
               populated=ifelse(
                  is.na(.data$name), FALSE, .data$name==.data$db
               ),
               timestamps=ifelse(
                  is.na(.data$total_rows), FALSE, .data$total_rows > 0
               )
            ) %>% 
            dplyr::select(-"name", -"total_rows") %>% 
            dplyr::rename("name"="db") %>% 
            dplyr::arrange(.data$name)
         
         withTs <- mdbDesc$name[which(mdbDesc$timestamps)]
         if(length(withTs)>0){
            latestTS <- DBI::dbGetQuery(
               con,
               paste(
                  sprintf(
                     paste(
                        "SELECT '%s' AS name, timestamp",
                        " FROM `%s`.___Timestamps___",
                        " ORDER BY timestamp DESC LIMIT 1"
                     ),
                     withTs, withTs
                  ),
                  collapse=" UNION ALL "
               ),
               format="TabSeparatedWithNamesAndTypes"
            )
            mdbDesc <- dplyr::left_join(mdbDesc, latestTS, by="name")
         }else{
            mdbDesc$latest <- NA
         }
         
         mdbUsers <- list_chMDB_users(x) %>% 
            dplyr::filter(.data$login==con@user)
         notInit <- setdiff(dbNames, mdbDesc$name)
         if(length(notInit) >0){
            mdbDesc <- dplyr::bind_rows(
               mdbDesc,
               dplyr::tibble(
                  name=notInit,
                  title=as.character(NA),
                  description=as.character(NA),
                  url=as.character(NA),
                  version=as.character(NA),
                  maintainer=as.character(NA),
                  public=as.logical(NA),
                  populated=FALSE
               )
            )
         }
         toRet <- mdbDesc %>%
            dplyr::mutate(
               access = dplyr::case_when(
                  !!x$admin ~ "write and read",
                  .data$name %in% !!mdbUsers$db[which(!!mdbUsers$admin)] ~
                     "write and read",
                  .data$name %in% !!mdbUsers$db ~ "read only",
                  !is.na(.data$public) & .data$public ~ "read only",
                  is.na(.data$public) ~ as.character(NA),
                  TRUE ~ "none"
               ) %>%
                  factor(levels=accessLevels)
            )
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
         paste(
            "if(isNull(comment), 0, positionCaseInsensitive(comment, '%s')>0)",
            " as s3,"
         ),
         searchTerm
      ),
      if(nchar(searchTerm)>4){
         sprintf(
            paste(
               "if(",
               "isNull(comment), 0, ngramSearchCaseInsensitive(comment, '%s')",
               ")",
               " as s4,"
            ),
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
   toRet <- get_query(x, query, format="Arrow") %>% 
      dplyr::arrange(dplyr::desc(.data$ms)) %>%
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
         paste(
            "SELECT '%s' as resource,",
            " table, name, type, nullable, unique, comment,"
         ),
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
         paste(
            "if(isNull(comment), 0, positionCaseInsensitive(comment, '%s')>0)",
            " as s3,"
         ),
         searchTerm
      ),
      if(nchar(searchTerm)>4){
         sprintf(
            paste(
               "if(",
               "isNull(comment), 0, ngramSearchCaseInsensitive(comment, '%s')",
               ")",
               " as s4,"
            ),
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
   toRet <- get_query(x, query, format="Arrow") %>%
      dplyr::arrange(dplyr::desc(.data$ms)) %>%
      dplyr::select(
         "resource", "table", "name", "comment",
         "type", "nullable", "unique"
      ) %>%
      dplyr::mutate(
         nullable=as.logical(.data$nullable),
         unique=as.logical(.data$unique)
      )
   return(toRet)
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
   if(!is.na(x$provider)){
      if(!x$provider && !x$admin){
         stop(paste(
            "Only chTKCat admin or data provider",
            "can create an MDB in ClickHouse"
         ))
      }
   }else{
      if(!x$admin){
         stop("Only chTKCat admin can create an MDB in ClickHouse")
      }
   }
   if(name %in% list_MDBs(x, withInfo=FALSE)){
      stop("The database exists already")
   }
   con <- x$chcon
   DBI::dbSendQuery(con, sprintf("CREATE DATABASE `%s`", name))
   mergeTrees_from_RelDataModel(
      con, name,
      CHMDB_DATA_MODEL
   )
   users <- list_chTKCat_users(x) %>% dplyr::pull("login")
   add_chMDB_user(x, name, x$chcon@user, admin=TRUE)
   set_chMDB_access(x, name, public=public)
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
   DBI::dbSendQuery(con, sprintf("DROP DATABASE `%s`", name))
   ul <- list_chTKCat_users(x) %>% 
      dplyr::filter(!.data$admin)
   if("provider" %in%  colnames(ul)){
      pl <- ul$login[which(ul$provider)]
      if(length(pl) > 0){
         DBI::dbSendQuery(
            con, 
            sprintf(
               paste(
                  "GRANT SELECT,",
                  " CREATE DATABASE, CREATE TABLE, DROP TABLE,",
                  " ALTER, INSERT",
                  " ON `%s`.* TO '%s' WITH GRANT OPTION"
               ),
               name,
               paste(pl, collapse="', '")
            )
         )
      }
      cl <- ul$login[which(!ul$provider)]
   }else{
      cl <- ul$login
   }
   if(length(cl) > 0){
      DBI::dbSendQuery(
         con,
         sprintf(
            "REVOKE %s ON `%s`.* FROM '%s'",
            paste(CH_DB_STATEMENTS, collapse=", "),
            name,
            paste(cl, collapse="', '")
         )
      )
   }
   invisible()
}

###############################################################################@
#' Update grants on tables in an MDB of a [chTKCat] object
#' 
#' The update is done automatically based on user access.
#'
#' @param x a [chTKCat] object
#' @param mdb name of the modeled database
#' 
#' @return No return value, called for side effects
#' 
#' @export
#'
update_chMDB_grants <- function(x, mdb){
   
   stopifnot(
      is.chTKCat(x),
      is.character(mdb), length(mdb)==1, !is.na(mdb)
   )
   managedMdbs <- list_MDBs(x, withInfo=TRUE) %>% 
      dplyr::filter(.data$access=="write and read" & .data$name==!!mdb)
   if(!mdb %in% managedMdbs$name){
      stop(sprintf("No admin permission on '%s' database", mdb))
   }
   con <- x$chcon
   
   ## User groups ----
   tkcUsers <- list_chTKCat_users(x)
   mdbUsers <- list_chMDB_users(x, mdb)
   adminUsers <- c(
      tkcUsers %>% dplyr::filter(.data$admin) %>% dplyr::pull("login"),
      mdbUsers %>% dplyr::filter(.data$admin) %>% dplyr::pull("login")
   ) %>% 
      unique()
   readUsers <- mdbUsers$login
   if(!is.na(managedMdbs$public) && managedMdbs$public){
      readUsers <- c(readUsers, tkcUsers$login)
   }
   readUsers <- setdiff(unique(readUsers), adminUsers)
   others <- setdiff(tkcUsers$login, c(readUsers, adminUsers))
   
   ## Revoke read access ----
   if(length(others) > 0){
      DBI::dbSendQuery(
         con,
         sprintf(
            "REVOKE SELECT ON `%s`.* FROM '%s'",
            mdb, paste(others, collapse="', '")
         )
      )
      modelTables <- names(CHMDB_DATA_MODEL)
      for(tn in modelTables){
         DBI::dbSendQuery(
            con,
            sprintf(
               "GRANT SELECT ON `%s`.`%s` TO '%s'",
               mdb, tn, paste(others, collapse="', '")
            )
         )
      }
   }
   
   ## Grant read access ----
   DBI::dbSendQuery(
      con,
      sprintf(
         "GRANT SELECT ON `%s`.* TO '%s'",
         mdb, paste(c(readUsers, adminUsers), collapse="', '")
      )
   )
   
   ## Revoke write access ----
   if(length(c(readUsers, others)) > 0){
      DBI::dbSendQuery(
         con,
         sprintf(
            paste(
               "REVOKE CREATE TABLE, DROP TABLE, ALTER, INSERT",
               " ON `%s`.* FROM '%s'"
            ),
            mdb, paste(c(readUsers, others), collapse="', '")
         )
      )
   }
   
   ## Grant write access ----
   DBI::dbSendQuery(
      con,
      sprintf(
         paste(
            "GRANT SELECT, CREATE TABLE, DROP TABLE, ALTER, INSERT",
            " ON `%s`.* TO '%s' WITH GRANT OPTION"
         ),
         mdb, paste(adminUsers, collapse="', '")
      )
   )
   
   invisible()
}


###############################################################################@
#' List instance timestamps of an MDB in [chTKCat]
#' 
#' @param x a [chTKCat] object
#' @param name the name of the database
#' 
#' @return A tibble with the instance of each table at each timestamp.
#' The "current" attribute indicate the current timestamp instance.
#' If there is no recorded timestamp, the function returns NULL.
#' 
#' @export
#' 
list_chMDB_timestamps <- function(x, name){
   stopifnot(
      is.chTKCat(x),
      is.character(name), length(name)==1, !is.na(name)
   )
   if(!name %in% list_MDBs(x, withInfo=FALSE)){
      stop("The database does not exist")
   }
   allTables <- list_tables(x$chcon, dbNames=name)
   if(!"___Timestamps___" %in% allTables$name){
      return(NULL)
   }
   toRet <- get_query(
      x, sprintf("SELECT * FROM `%s`.`___Timestamps___`", name),
      format="TabSeparatedWithNamesAndTypes"
   )
   current <- toRet %>%
      dplyr::filter(
         .data$instance=="___MDB___" & .data$table=="___MDB___"
      ) %>% 
      dplyr::pull("timestamp")
   if(length(current)==0){
      current <- NA
   }
   attr(toRet, "current") <- current
   return(toRet)
}

###############################################################################@
#' Get instance timestamps of an MDB in [chTKCat]
#' 
#' @param x a [chTKCat] object
#' @param name the name of the database
#' 
#' @return A tibble with the instance "timestamp" and a logical indicating if
#' it's the "current" one or not.
#' 
#' @export
#' 
get_chMDB_timestamps <- function(x, name){
   ts <- list_chMDB_timestamps(x, name)
   if(is.null(ts)){
      return(NULL)
   }
   toRet <- dplyr::distinct(ts, .data$timestamp)
   if(is.na(attr(ts, "current"))){
      toRet$current <- FALSE
   }else{
      toRet$current <- toRet$timestamp==attr(ts, "current")
   }
   toRet <- dplyr::arrange(toRet, dplyr::desc(.data$timestamp))
   return(toRet)
}


###############################################################################@
#' Set timestamp of the current version of an MDB in [chTKCat]
#' 
#' @param x a [chTKCat] object
#' @param name the name of the database to affect
#' @param timestamp a single POSIXct value as a timestamp for
#' the chMDB instance.
#' 
#' @return No return value, called for side effects
#' 
#' @export
#' 
set_chMDB_timestamp <- function(x, name, timestamp){
   timestamp <- as.POSIXct(timestamp)
   stopifnot(
      is.chTKCat(x),
      inherits(timestamp, "POSIXct"), length(timestamp)==1, !is.na(timestamp),
      is.character(name), length(name)==1, !is.na(name)
   )
   dbl <- list_MDBs(x) %>% 
      dplyr::filter(.data$populated)
   if(!is.data.frame(dbl) || !name %in% dbl$name){
      stop(sprintf(
         "%s does not exist in the provided chTKCat or is not populated",
         name
      ))
   }
   mdbTables <- c(
      setdiff(
         names(CHMDB_DATA_MODEL),
         MGT_TABLES
      ),
      get_query(
         x, sprintf("SELECT name FROM `%s`.`___Tables___`", name),
         format="TabSeparatedWithNamesAndTypes"
      )$name
   )
   ntst <- dplyr::tibble(
      timestamp=timestamp,
      table=mdbTables,
      instance=mdbTables
   )
   
   ### Sub-tables ----
   sdtables <- get_query(
      x,
      sprintf(
         "SELECT table FROM `%s`.`___Fields___` WHERE type='row'",
         name
      ),
      format="TabSeparatedWithNamesAndTypes"
   )$table %>% 
      unique()
   if(length(sdtables)>0){
      stables <- get_query(
         x,
         paste(
            sprintf(
               "SELECT '%s' as ref, table as subtab FROM `%s`.`%s`",
               sdtables, name, sdtables
            ),
            collapse=" UNION ALL "
         ),
         format="TabSeparatedWithNamesAndTypes"
      )$subtab
      if(length(stables)>0){
         ntst <- rbind(
            ntst,
            dplyr::tibble(
               timestamp=timestamp,
               table=stables,
               instance=stables
            )
         )
      }
   }
   
   tst <- list_chMDB_timestamps(x, name)
   if(is.null(tst)){
      mergeTree_from_RelTableModel(
         con=x$chcon, dbName=name, tm=CHMDB_DATA_MODEL$"___Timestamps___"
      )
      tst <- list_chMDB_timestamps(x, name)
   }
   current <- attr(tst, "current")
   if(!is.na(current)){
      if(timestamp < current){
         stop("Cannot set a timestamp earlier than the current one.")
      }
      toRm <- get_query(
         x,
         sprintf(
            "SELECT * FROM `%s`.`___Timestamps___` WHERE timestamp=%s",
            name, as.numeric(current)
         ),
         format="TabSeparatedWithNamesAndTypes"
      )
      if(nrow(toRm)==0){
         stop("Error in timestamp encoding. Contact chTKCat admin.")
      }
      get_query(
         x,
         sprintf(
            "ALTER TABLE `%s`.`___Timestamps___` DELETE WHERE timestamp=%s",
            name, as.numeric(current)
         )
      )
   }
   ch_insert(
      con=x$chcon, dbName=name,
      tableName="___Timestamps___", value=ntst
   )
   
   ## Update grants ----
   update_chMDB_grants(x, name)
}


###############################################################################@
#' Empty a chMDB in a [chTKCat]
#' 
#' @param x a [chTKCat] object
#' @param name the name of the database to empty
#' @param timestamp timestamp of the instance to empty. If NA (default)
#' the current instance is emptied.
#' 
#' @return No return value, called for side effects
#' 
#' @export
#' 
empty_chMDB <- function(
   x, name, timestamp=NA
){
   .empty_chMDB(x=x, name=name, timestamp=timestamp)
}

.empty_chMDB <- function(
   x, name, timestamp=NA,
   .toKeep=character()    ## For internal use only. A character vector
                          ## indicating the tables to not drop
                          ## (default: `character()`)
){
   timestamp <- as.POSIXct(timestamp)
   stopifnot(
      is.chTKCat(x),
      is.character(name), length(name)==1, !is.na(name),
      is.character(.toKeep),
      is.na(timestamp) || inherits(timestamp, "POSIXct"), length(timestamp)==1
   )
   if(any(.toKeep %in% names(CHMDB_DATA_MODEL))){
      stop("Cannot keep data model tables: don't put them in the .toKeep param")
   }
   if(!name %in% list_MDBs(x, withInfo=FALSE)){
      stop("The database does not exist")
   }
   con <- x$chcon
   
   ## Matrix tables ----
   matTables <- get_query(
      x,
      sprintf(
         "SELECT DISTINCT table FROM `%s`.___Fields___ WHERE type='column'",
         name
      ),
      format="TabSeparatedWithNamesAndTypes"
   )$table
   get_submat_tn <- function(n){
      n <- intersect(n, matTables)
      if(length(n)==0){
         return(character())
      }else{
         return(get_query(
            x,
            paste(
               sprintf(
                  "SELECT DISTINCT table FROM `%s`.`%s`",
                  name, n
               ),
               collapse=" UNION ALL "
            ),
            format="TabSeparatedWithNamesAndTypes"
         )$table)
      }
   }
   
   ## Identify tables to drop and tables to empty ----
   allTabInstances <- list_tables(con, name)$name
   tst <- list_chMDB_timestamps(x, name)
   if(is.null(tst) || nrow(tst)==0){
      if(!is.na(timestamp)){
         stop("No timestamp available for this database")
      }
      toDrop <- setdiff(
         allTabInstances,
         MGT_TABLES
      )
      ### Keep tables to keep
      .toKeep <- unique(c(.toKeep, get_submat_tn(.toKeep)))
      toDrop <- setdiff(toDrop, .toKeep)
      reinit <- TRUE
   }else{
      if(is.na(timestamp) && is.na(attr(tst, "current"))){
         toDrop <- setdiff(
            allTabInstances,
            c(tst$instance, MGT_TABLES)
         )
         ### Keep tables to keep
         .toKeep <- unique(c(.toKeep, get_submat_tn(.toKeep)))
         toDrop <- setdiff(toDrop, .toKeep)
         reinit <- TRUE
      }else{
         if(is.na(timestamp)){
            timestamp <- attr(tst, "current")
         }
         if(!timestamp %in% tst$timestamp){
            stop("The selected timestamp does not exist")
         }
         toDrop <- tst %>%
            dplyr::filter(
               .data$timestamp==!!timestamp
            ) %>% 
            dplyr::pull("instance")
         .toKeep <- unique(c(
            .toKeep,
            tst$instance[which(tst$timestamp != timestamp)]
         ))
         ### Keep tables to keep
         .toKeep <- unique(c(.toKeep, get_submat_tn(.toKeep)))
         toDrop <- setdiff(toDrop, .toKeep)
         
         ### Clean timestamps ----
         get_query(
            x,
            sprintf(
               "ALTER TABLE `%s`.`___Timestamps___` DELETE WHERE timestamp=%s",
               name, as.numeric(timestamp)
            )
         )
         
         if(timestamp==attr(tst, "current")){
            reinit <- TRUE
         }else{
            reinit <- FALSE
         }
         
      }
   }
  
   ## Remove tables and re-initialize if necessary ----
   for(tn in toDrop){
      DBI::dbSendQuery(
         x$chcon,
         sprintf("DROP TABLE `%s`.`%s`", name, tn)
      )
   }
   if(reinit){
      mergeTrees_from_RelDataModel(
         con, name,
         CHMDB_DATA_MODEL[setdiff(names(CHMDB_DATA_MODEL), MGT_TABLES)]
      )
   }
   
   ## Update grants ----
   update_chMDB_grants(x, name)
   
   Sys.sleep(5)
   invisible()
}


###############################################################################@
#' Archive a chMDB in a [chTKCat]
#' 
#' @param x a [chTKCat] object
#' @param name the name of the database to archive
#' @param defaultTS a default timestamp value to use when not existing in
#' the DB (default: `as.POSIXct("1970-01-01 00:00.0", tz="UTC")`)
#' 
#' @return No return value, called for side effects
#' 
#' @export
#' 
archive_chMDB <- function(
   x, name,
   defaultTS=as.POSIXct("1970-01-01 00:00.0", tz="UTC")
){
   .archive_chMDB(x=x, name=name, defaultTS=defaultTS)
}

.archive_chMDB <- function(
   x, name,
   defaultTS=as.POSIXct("1970-01-01 00:00.0", tz="UTC"),
   .toKeep=character() ## A character vector indicating the
                       ## tables to not archive (default: `character()`)
){
   defaultTS <- as.POSIXct(defaultTS)
   stopifnot(
      is.chTKCat(x),
      is.character(name), length(name)==1, !is.na(name),
      is.character(.toKeep),
      !is.na(defaultTS), inherits(defaultTS, "POSIXct"), length(defaultTS)==1
   )
   if(any(.toKeep %in% names(CHMDB_DATA_MODEL))){
      stop("Cannot keep data model tables: don't put them in the .toKeep param")
   }
   mdbl <- list_MDBs(x, withInfo=TRUE)
   if(!name %in% mdbl$name){
      stop("The database does not exist")
   }
   con <- x$chcon
   if(!name %in% dplyr::pull(dplyr::filter(mdbl, .data$populated), "name")){
      stop(sprintf("%s is not filled or has no current state", name))
   }
   
   ## Set timestamp when not existing ----
   tst <- list_chMDB_timestamps(x, name)
   if(is.null(tst) || is.na(attr(tst, "current"))){
      set_chMDB_timestamp(x, name, defaultTS)
      tst <- list_chMDB_timestamps(x, name)
   }
   
   ## Archive tables ----
   toArchive <- c(
      names(CHMDB_DATA_MODEL),
      get_query(
         x,
         sprintf("SELECT name FROM `%s`.`___Tables___`", name),
         format="TabSeparatedWithNamesAndTypes"
      )$name
   )
   toArchive <- tst %>% 
      dplyr::filter(
         .data$timestamp==attr(tst, "current"),
         .data$table %in% toArchive
      )
   toArchive$newInstance <- uuid::UUIDgenerate(n=nrow(toArchive))
   toArchive$newInstance <- ifelse(
      toArchive$table %in% .toKeep,
      toArchive$instance,
      toArchive$newInstance
   )
   toUpdate <- tst %>%
      dplyr::filter(.data$instance %in% toArchive$instance) %>% 
      dplyr::left_join(
         toArchive[,c("instance", "newInstance")],
         by="instance"
      ) %>% 
      dplyr::select("timestamp", "table", "instance"="newInstance")
   if(nrow(toArchive) > 0){
      get_query(
         x,
         sprintf(
            "ALTER TABLE `%s`.`___Timestamps___` DELETE WHERE instance IN (%s)",
            name,
            paste0("'", paste(unique(toArchive$instance), collapse="', '"), "'")
         )
      )
      ch_insert(
         con=x$chcon, dbName=name,
         tableName="___Timestamps___", value=toUpdate
      )
      for(i in 1:nrow(toArchive)){
         if(toArchive$instance[i] != toArchive$newInstance[i]){
            get_query(
               x,
               sprintf(
                  "RENAME TABLE `%s`.`%s` TO `%s`.`%s`",
                  name, toArchive$instance[i],
                  name, toArchive$newInstance[i]
               )
            )
         }
      }
   }
   
   ## Prepare new instance ----
   mergeTrees_from_RelDataModel(
      con, name,
      CHMDB_DATA_MODEL[setdiff(
         names(CHMDB_DATA_MODEL),
         MGT_TABLES
      )]
   )
   
   ## Update grants ----
   update_chMDB_grants(x, name)
   
   Sys.sleep(5)
   invisible()
}

###############################################################################@
#' Unarchive a chMDB in a [chTKCat]
#' 
#' @param x a [chTKCat] object
#' @param name the name of the database to archive
#' 
#' @return No return value, called for side effects
#' 
#' @export
#' 
unarchive_chMDB <- function(x, name){
   stopifnot(
      is.chTKCat(x),
      is.character(name), length(name)==1, !is.na(name)
   )
   mdbl <- list_MDBs(x, withInfo=TRUE)
   if(!name %in% mdbl$name){
      stop("The database does not exist")
   }
   con <- x$chcon
   
   if(name %in% dplyr::pull(dplyr::filter(mdbl, .data$populated), "name")){
      stop(sprintf("%s is already unarchived", name))
   }
   tst <- list_chMDB_timestamps(x, name)
   if(is.null(tst)){
      stop(sprintf("There is no available archive for %s", name))
   }
   if(!is.na(attr(tst, "current"))){
      stop(sprintf("%s is already unarchived", name))
   }
   
   current <- max(tst$timestamp)
   toUnarchive <- tst %>% 
      dplyr::filter(.data$timestamp==current)
   toUnarchive$newInstance <- toUnarchive$table
   toUpdate <- tst %>%
      dplyr::filter(.data$instance %in% toUnarchive$instance) %>% 
      dplyr::left_join(
         toUnarchive[,c("instance", "newInstance")],
         by="instance"
      ) %>% 
      dplyr::select("timestamp", "table", "instance"="newInstance")
   
   if(nrow(toUnarchive) > 0){
      get_query(
         x,
         sprintf(
            "ALTER TABLE `%s`.`___Timestamps___` DELETE WHERE instance IN (%s)",
            name,
            paste0(
               "'", paste(unique(toUnarchive$instance), collapse="', '"), "'"
            )
         )
      )
      ch_insert(
         con=x$chcon, dbName=name,
         tableName="___Timestamps___", value=toUpdate
      )
      for(tn in setdiff(names(CHMDB_DATA_MODEL), MGT_TABLES)){
         get_query(x, sprintf("DROP TABLE `%s`.`%s`", name, tn))
      }
      for(i in 1:nrow(toUnarchive)){
         if(toUnarchive$instance[i] != toUnarchive$newInstance[i]){
            get_query(
               x,
               sprintf(
                  "RENAME TABLE `%s`.`%s` TO `%s`.`%s`",
                  name, toUnarchive$instance[i],
                  name, toUnarchive$newInstance[i]
               )
            )
         }
      }
   }
   
   ## Update grants ----
   update_chMDB_grants(x, name)
   
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
      sprintf("SELECT public from `%s`.___Public___", mdb),
      format="TabSeparatedWithNamesAndTypes"
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
   DBI::dbSendQuery(
      con, 
      sprintf(
         "ALTER TABLE `%s`.___Public___ DELETE WHERE 1",
         mdb
      )
   )
   ch_insert(
      con, dbName=mdb, tableName="___Public___",
      value=dplyr::tibble(public=public)
   )
   update_chMDB_grants(x, mdb)
   invisible()
}


###############################################################################@
#' List users of an MDB of a [chTKCat] object
#'
#' @param x a [chTKCat] object
#' @param mdbs names of the modeled databases.
#' If NULL (default), all the databases are considered.
#' 
#' @return A tibble with 3 columns:
#' - user: the user login
#' - mdb: the name of the modeled database
#' - admin: if the user is an admin of the MDB
#' 
#' @export
#'
list_chMDB_users <- function(x, mdbs=NULL){
   stopifnot(
      is.chTKCat(x)
   )
   con <- x$chcon
   ## Check access ----
   allMdbs <- list_MDBs(x, withInfo=FALSE)
   mdbWithUsers <- DBI::dbGetQuery(
      con,
      "SELECT database FROM system.tables WHERE name='___MDBUsers___'",
      format="TabSeparatedWithNamesAndTypes"
   ) %>%
      dplyr::pull("database")
   if(is.null(mdbs)){
      mdbs <- mdbWithUsers
   }else{
      stopifnot(
         is.character(mdbs), all(!is.na(mdbs)), length(mdbs) > 0
      )
      notIn <- setdiff(mdbs, allMdbs)
      if(length(notIn) > 0){
         stop(sprintf(
            "The following mdbs do not exist: %s",
            paste(notIn, collapse=", ")
         ))
      }
      withoutUsers <- setdiff(mdbs, mdbWithUsers)
      if(length(withoutUsers) > 0){
         stop(sprintf(
            "The following mdbs have not any registered user: %s",
            paste(withoutUsers, collapse=", ")
         ))
      }
   }
   if(length(mdbs)==0){
      toRet <- dplyr::tibble(
         db=character(), login=character(), admin=logical()
      )
   }else{
      toRet <-  DBI::dbGetQuery(
         con,
         paste(
            sprintf(
               "SELECT '%s' as db, login, admin from `%s`.`___MDBUsers___`",
               mdbs, mdbs
            ),
            collapse=" UNION ALL "
         ),
         format="TabSeparatedWithNamesAndTypes"
      ) %>%
         dplyr::as_tibble() %>% 
         dplyr::mutate(admin=as.logical(.data$admin)) %>% 
         dplyr::arrange(.data$db, .data$admin, .data$login)
   }
   return(toRet)
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
   if(login %in% list_chMDB_users(x, mdbs=mdb)$login){
      stop(sprintf("%s is already registered for %s chMDB", login, mdb))
   }
   con <- x$chcon
   ch_insert(
      con, dbName=mdb, tableName="___MDBUsers___",
      value=dplyr::tibble(login=login, admin=admin)
   )
   update_chMDB_grants(x, mdb)
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
   DBI::dbSendQuery(
      con,
      sprintf(
         "ALTER TABLE `%s`.___MDBUsers___ DELETE WHERE login='%s'",
         mdb, login
      )
   )
   update_chMDB_grants(x, mdb)
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
   if("Collections" %in% list_tables(x$chcon, "default")$name){
      toRet <- DBI::dbGetQuery(
         conn=x$chcon,
         statement=sprintf(
            "SELECT title, description %s FROM default.Collections",
            ifelse(withJson, ", json", "")
         ),
         format="Arrow"
      ) %>%
         dplyr::as_tibble()
   }else{
      toRet <- dplyr::tibble("title"=character(), "description"=character())
   }
   return(toRet)
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
   DBI::dbGetQuery(
      conn=x$chcon,
      statement=sprintf(
         "SELECT json FROM default.Collections WHERE title='%s'",
         title
      ),
      format="Arrow"
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
   if(!jsonvalidate::json_validate(
      raw, tkcatEnv$COL_SCHEMA, verbose=TRUE, engine="ajv"
   )){
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
      DBI::dbSendQuery(
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
   DBI::dbSendQuery(
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
   dbNames <- DBI::dbGetQuery(
      con, "SELECT name FROM system.databases",
      format="TabSeparatedWithNamesAndTypes"
   ) %>% 
      dplyr::pull("name") %>% 
      setdiff(CH_RESERVED_DB)
   
   check_chTKCat(x, verbose=TRUE)
   toRet <- c()
   for(dbName in dbNames){
      dbTables <- DBI::dbGetQuery(
         con,
         sprintf("SHOW TABLES FROM `%s`", dbName),
         format="TabSeparatedWithNamesAndTypes"
      )
      if("___CollectionMembers___" %in% dbTables$name){
         toRet <- dplyr::bind_rows(
            toRet,
            DBI::dbGetQuery(
               con,
               sprintf(
                  paste(
                     "SELECT DISTINCT collection, table",
                     "FROM `%s`.___CollectionMembers___"
                  ),
                  dbName
               ),
               format="TabSeparatedWithNamesAndTypes"
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
#' @param userManager URL for user management interface
#' (see [manage_chTKCat_users()]). If NULL (default), the functionality
#' is not added.
#' @param title A title for the application. If NULL (default):
#' the chTKCat instance name
#' @param skinColors two colors for the application skin: one for default
#' connection ("blue" by default) and one for user
#' connection ("yellow" by default).
#' Working values: "blue", "black", "purple", "green", "red", "yellow".
#' @param logoDiv a [shiny::div] object with a logo to display in side bar.
#' The default is the TKCat hex sticker with a link to TKCat github repository.
#' @param rDirs a named character vector with resource path
#' for [shiny::addResourcePath]
#' @param tabTitle a title to display in tab (default: "chTKCat")
#' @param tabIcon a path to an image
#' (in available resource paths: "www", "doc" or in rDirs) to use as a tab icon.
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
   userManager=NULL,
   title=NULL,
   skinColors=c("blue", "yellow"),
   logoDiv=TKCAT_LOGO_DIV,
   tabTitle="chTKCat",
   tabIcon='www/TKCat-small.png',
   rDirs=NULL,
   ...
){
   stopifnot(
      is.logical(download), length(download)==1, !is.na(download),
      is.character(skinColors), length(skinColors)>0, all(!is.na(skinColors))
   )
   if(length(skinColors)==1){
      skinColors <- rep(skinColors, 2)
   }
   skinColors <- skinColors[1:2]
   if(!is.null(userManager)){
      stopifnot(
         is.character(userManager), length(userManager)==1, !is.na(userManager)
      )
   }
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
      ui=.build_etkc_ui(
         x=x, ddir=ddir, userManager=!is.null(userManager),
         logoDiv=logoDiv, rDirs=rDirs,
         tabTitle=tabTitle, tabIcon=tabIcon
      ),
      server=.build_etkc_server(
         x=x,
         subSetSize=subSetSize,
         host=host,
         ddir=ddir,
         userManager=userManager,
         title=title,
         skinColors=skinColors
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
.build_etkc_ui.chTKCat <- function(
   x, ddir=NULL, userManager=FALSE,
   logoDiv=TKCAT_LOGO_DIV, rDirs=NULL,
   tabTitle="chTKCat",
   tabIcon='www/TKCat-small.png',
   ...
){
   
   .etkc_add_resources(ddir=ddir, rDirs=rDirs)
   
   function(req){
      shinydashboard::dashboardPage(
         title=tabTitle,
         
         ########################@
         ## Dashboard header ----
         ## Uses output$instance and output$status
         header=.etkc_sd_header(),
         
         ########################@
         ## Sidebar ----
         ## Uses uiOutput("currentUser") and uiOutput("signin")
         sidebar=.etkc_sd_sidebar(
            sysInterface=TRUE,
            userManager=userManager,
            manList=c(
               "TKCat user guide"="doc/TKCat-User-guide.html"
            ),
            logoDiv=logoDiv
         ),
         
         ########################@
         ## Body ----
         body=.etkc_sd_body(sysInterface=TRUE, tabIcon=tabIcon)
      )
   }
   
}


###############################################################################@
.build_etkc_server.chTKCat <- function(
   x,
   subSetSize=100,
   host=x$chcon@host,
   ddir=NULL,
   userManager=NULL,
   title=NULL,
   skinColors=c("blue", "yellow")
){
   .build_etkc_server_default(
      x=x, subSetSize=subSetSize, xparams=list(host=host),
      ddir=ddir,
      userManager=userManager,
      title=title,
      skinColors=skinColors
   )
}


###############################################################################@
#### SHINY MANAGER ####
###############################################################################@


###############################################################################@
#' Manage user information in a shiny interface
#' 
#' @param x a [chTKCat] object
#' @param pwdFile a local file in which the password for x can be found.
#' If NULL (default), the connection is shared by all sessions and can
#' be disabled at some point.
#' 
#' @export
#'
manage_chTKCat_users <- function(x, pwdFile=NULL){
   
   stopifnot(
      is.chTKCat(x), x$admin
   )
   check_chTKCat(x)
   
   .etkc_add_resources(ddir=NULL)
   
   shiny::shinyApp(
      
      ui=function(req){
         shiny::fluidPage(
            shiny::tags$script("
               Shiny.addCustomMessageHandler(
                  'current_password',
                  function(value) {
                     Shiny.setInputValue('current_password', value);
                  }
               );
               $(document).keyup(function(event) {
                  if(event.key == 'Enter'){
                     if($('#update')[0]) {
                        $('#update').click();
                     }else{
                        if($('#connect')[0]) {
                           $('#connect').click();
                        }
                     }
                  }
               });
            "),
            shiny::fluidRow(
               shiny::column(
                  6,
                  shiny::textInput(
                     "login", "User name"
                  ),
                  shiny::passwordInput(
                     "current_password", "Current password"
                  ),
                  shiny::actionButton(
                     "connect", "Connect to check and modify settings"
                  )
               ),
               shiny::column(
                  6,
                  shiny::uiOutput("userInfo")
               )
            )
         )
      },
      
      server=function(input, output, session){
         
         ## Manage DB connection ----
         if(!is.null(pwdFile)){
            db_reconnect(x, password=readLines(pwdFile))
            shiny::onSessionEnded(function() db_disconnect(x))
         }
         shiny::onStop(function() db_disconnect(x))
         userInstance <- shiny::reactiveVal()
         
         ## Connect as user ----
         shiny::observeEvent(input$connect, {
            l <- shiny::isolate(input$login)
            p <- shiny::isolate(input$current_password)
            shiny::req(l)
            userInstance(try(
               chTKCat(
                  host=x$chcon@host, port=x$chcon@port,
                  user=l, password=p,
                  settings=x$settings
               ),
               silent=TRUE
            ))
            session$sendCustomMessage("current_password", 'null')
         })
         
         ## Display user settings ----
         newpwd <- shiny::reactiveValues(
            pwd=character(0),
            cpwd=character(0),
            valid=FALSE
         )
         output$userInfo <- shiny::renderUI({
            ui <- userInstance()
            shiny::req(!is.null(ui))
            if(inherits(ui, "try-error")){
               return(shiny::fluidRow(shiny::column(
                  12,
                  shiny::p(shiny::strong("Bad credentials", style="color:red;"))
               )))
            }
            if(ui$chcon@user=="default"){
               return(shiny::fluidRow(shiny::column(
                  12,
                  shiny::p(shiny::strong(
                     "The default user cannot be modified",
                     style="color:red;"
                  ))
               )))
            }
            if(ui$chcon@user==x$chcon@user){
               return(shiny::fluidRow(shiny::column(
                  12,
                  shiny::p(shiny::strong(
                     sprintf("The %s user cannot be modified", x$chcon@user),
                     style="color:red;"
                  ))
               )))
            }
            ut <- list_chTKCat_users(x)
            cur_cont <- ut %>%
               dplyr::filter(.data$login==ui$chcon@user) %>%
               dplyr::pull("contact")
            toRet <- shiny::fluidRow(shiny::column(
               12,
               shiny::textInput(
                  "contact", "Contact", placeholder=cur_cont
               ),
               shiny::passwordInput(
                  "new_password", "New password (8 charcters or more)"
               ),
               shiny::passwordInput(
                  "conf_password", "Confirm new password"
               ),
               shiny::uiOutput("pwdCheck"),
               shiny::actionButton(
                  "update", "Update settings"
               ),
               shiny::uiOutput("updInfo")
            ))
            return(toRet)
         })
         output$pwdCheck <- shiny::renderUI({
            nclim <- 8
            pwd <- input$new_password
            cpwd <- input$conf_password
            valid <- nchar(pwd) >= nclim &
               pwd==cpwd
            newpwd$pwd <- pwd
            newpwd$cpwd <- cpwd
            newpwd$valid <- valid
            if(!valid){
               if(nchar(pwd)==0){
                  return()
               }else{
                  if(nchar(pwd) < nclim){
                     return(shiny::strong(
                        "8 characters or more are required",
                        style="color:red;"
                     ))
                  }
                  if(pwd != cpwd){
                     return(shiny::strong(
                        "Passwords are differents",
                        style="color:red;"
                     ))
                  }
               }
            }else{
               return(shiny::strong(
                  "Valid new password, ready for update",
                  style="color:green;"
               ))
            }
         })
         
         ## Update settings ----
         output$updInfo <- shiny::renderUI({
            shiny::req(input$update)
            ui <- shiny::isolate(userInstance())
            shiny::req(ui)
            vui <- try(check_chTKCat(ui), silent=TRUE)
            if(inherits(vui, "try-error")){
               userInstance(vui)
               return()
            }
            newCont <- shiny::isolate(input$contact)
            toRet <- shiny::tagList()
            if(!is.na(newCont) & length(newCont) & nchar(newCont)>0){
               update_chTKCat_user(x, login=ui$chcon@user, contact=newCont)
               toRet <- c(
                  toRet,
                  shiny::tagList(shiny::p(shiny::strong(
                     "- Contact information has been updated",
                     style="color:green;"
                  )))
               )
            }
            if(shiny::isolate(newpwd$valid)){
               pwd <- shiny::isolate(newpwd$pwd)
               change_chTKCat_password(x, login=ui$chcon@user, password=pwd)
               db_disconnect(ui)
               newpwd$pwd <- ""
               newpwd$cpwd <- ""
               newpwd$valid <- FALSE
               toRet <- c(
                  toRet,
                  shiny::tagList(shiny::p(shiny::strong(
                     paste(
                        "- Password has been updated",
                        "(you need to reconnect to make other changes)"
                     ),
                     style="color:green;"
                  )))
               )
            }
            return(toRet)
         })
         
      }
   )
}
