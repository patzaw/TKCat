###############################################################################@
#' Get DB information
#'
#' @export
#'
dbInfo <- function(x, ...){
   UseMethod("dbInfo", x)
}

###############################################################################@
#' Get object dataModel
#'
#' @export
#'
dataModel <- function(x, ...){
   UseMethod("dataModel", x)
}

###############################################################################@
#' Get object data tables
#'
#' @export
#'
dataTables <- function(x, ...){
   UseMethod("dataTables", x)
}
