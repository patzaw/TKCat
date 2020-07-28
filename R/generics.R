###############################################################################@
#' Get DB information
#' 
#' @param x an object with embedded database information
#' @param ... further arguments for the method
#'
#' @export
#'
db_info <- function(x, ...){
   UseMethod("db_info", x)
}

###############################################################################@
#' Get collection members
#' 
#' @param x an object with embedded collection members
#' @param ... further arguments for the method
#'
#' @export
#'
collection_members <- function(x, ...){
   UseMethod("collection_members", x)
}

###############################################################################@
#' Set collection members
#' 
#' @param x an object with embedded collection members
#' @param value the new collection members
#'
#' @export
#'
'collection_members<-' <- function(x, value){
   UseMethod("collection_members<-", x)
}


###############################################################################@
#' Get object data model
#'
#' @param x an object with an embedded data model
#' @param ... further arguments for the method
#' 
#' @export
#'
data_model <- function(x, ...){
   UseMethod("data_model", x)
}

###############################################################################@
#' Get object data tables
#'
#' @param x an object with embedded data tables
#' @param ... further arguments for the method
#' 
#' @export
#'
data_tables <- function(x, ...){
   UseMethod("data_tables", x)
}
