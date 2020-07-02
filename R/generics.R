###############################################################################@
#' Get DB information
#'
#' @export
#'
db_info <- function(x, ...){
   UseMethod("db_info", x)
}

###############################################################################@
#' Get collection members
#'
#' @export
#'
collection_members <- function(x, ...){
   UseMethod("collection_members", x)
}

###############################################################################@
#' Set collection members
#'
#' @export
#'
'collection_members<-' <- function(x, value, ...){
   UseMethod("collection_members<-", x)
}


###############################################################################@
#' Get object data model
#'
#' @export
#'
data_model <- function(x, ...){
   UseMethod("data_model", x)
}

###############################################################################@
#' Get object data tables
#'
#' @export
#'
data_tables <- function(x, ...){
   UseMethod("data_tables", x)
}
