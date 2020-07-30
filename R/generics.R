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
#' @param value the new collection members.
#' A data.frame with the following columns:
#' - **collection** (character): The name of the collection
#' - **cid** (character): Collection identifier
#' - **resource** (character): The name of the resource
#' - **mid** (integer): The identifier of the member
#' - **table** (character): The table recording collection information
#' - **field** (character): The collection field.
#' - **static** (logical): TRUE if the field value is common to all elements.
#' - **value** (character): The name of the table column if static is FALSE
#' or the field value if static is TRUE.
#' - **type** (character): the type of the field.
#' (not necessarily used ==> NA if not)
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
