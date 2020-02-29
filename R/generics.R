###############################################################################@
#' Get DB information
#'
#' @export
#'
dbInfo <- function(x, ...){
   UseMethod("dbInfo", x)
}

###############################################################################@
#' Get collection members
#'
#' @export
#'
collectionMembers <- function(x, ...){
   UseMethod("collectionMembers", x)
}

###############################################################################@
#' Set collection members
#'
#' @export
#'
'collectionMembers<-' <- function(x, value, ...){
   UseMethod("collectionMembers<-", x)
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

###############################################################################@
#' Export an object in a folder
#'
#' @export
#'
export <- function(x, path, ...){
   UseMethod("export", x)
}

###############################################################################@
#' Interactive exploration of an object
#'
#' @export
#'
explore <- function(x, ...){
   UseMethod("explore", x)
}
