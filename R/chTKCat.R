#' Connect to a ClickHouse TKCat instance
#'
#' @param host a character string specifying the host heberging the 
#' database (default: localhost)
#' @param port an integer specifying the port on which the 
#' database is listening (default: 9101)
#'
#' @return a chTKCat object
#'
#' @seealso \code{\link{check_chTKCat}}, \code{\link{disconnect_chTKCat}}
#'
#' @importFrom RClickhouse clickhouse dbConnect
#' @export
#'
chTKCat <- function(
   host="localhost",
   port=9101
   # username=NULL,
   # password=NULL,
   # readOnly=TRUE
){
   
   chcon <- dbConnect(
      drv=clickhouse(),
      host=host,
      port=port
   )
   
   toRet <- list(
      chcon=chcon
   )
   class(toRet) <- "chTKCat"
   
   toRet <- check_chTKCat(toRet, verbose=TRUE)
   
   return(toRet)
   
}

###############################################################################@
#' @export
#'
is.chTKCat <- function(x){
   inherits(x, "chTKCat")
}

###############################################################################@
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
#' Disconnect from a ClickHouse TKCat instance
#'
#' @param x a chTKCat object
#'
#' @seealso \code{\link{chTKCat}}
#'
#' @importFrom RClickhouse dbDisconnect
#' @export
#'
disconnect_chTKCat <- function(x){
   stopifnot(inherits(x, "chTKCat"))
   dbDisconnect(x[["chcon"]])
}

###############################################################################@
#' Check the chTKCat object
#'
#' @param x a chTKCat object
#' @param instance instance name of the database
#' @param version version name of the database
#'
#' @return a chTKCat
#'
#' @seealso \code{\link{connectToTbkm}}
#'
init_chTKCat <- function(x, instance, version){
   check_chTKCat(x)
   defaultTables <- dbGetQuery(
      x$chcon,
      "select name from system.tables where database='default'"
   )$name
   if("System" %in% defaultTables){
      stop("chTKCat already initialized")
   }
   dbWriteTable(
      conn=x$chcon,
      name="System",
      value=tibble(
         name="chTKCat",
         instance=instance,
         version=version
      ),
      append=FALSE,
      overwrite=FALSE,
      row.names=FALSE
   )
   dbSendQuery(
      conn=x$chcon,
      statement=paste(
         "CREATE TABLE default.MDB (",
         "name String,",
         "title String,",
         "description String,",
         "url String,",
         "version String",
         ")",
         "ENGINE = MergeTree() ORDER BY (name)"
      )
   )
   x$init <- TRUE
   return(check_chTKCat(x))
}

###############################################################################@
#' Check the chTKCat object
#'
#' @param x a chTKCat object
#' @param verbose a logical indicating if information messages should be
#' displayed.
#'
#' @return Invisible result: chTKCat object
#'
#' @seealso \code{\link{connectToTbkm}}
#'
#' @importFrom DBI dbGetQuery dbReadTable
#' @export
#'
check_chTKCat <- function(x, verbose=FALSE){
   stopifnot(inherits(x, "chTKCat"))
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
#' List available database
#' 
#' @export
#' 
listMDBs <- function(tkcon){
   check_chTKCat(tkcon, verbose=TRUE)
   dbGetQuery(tkcon$chcon, "SELECT * FROM default.MDB") %>% as_tibble()
}

