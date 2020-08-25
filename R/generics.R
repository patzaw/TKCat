###############################################################################@
#' Get DB information
#' 
#' @param x an object with embedded database information
#' 
#' @return A list with the following elements:
#' - **name**: a single character
#' - **title**: a single character
#' - **description**: a single character
#' - **url**: a single character
#' - **version**: a single character
#' - **maintainer**: a single character vector
#' - **size**: a numeric vector providing the size of the DB in bytes
#'
#' @export
#'
db_info <- function(x, ...){
   UseMethod("db_info", x)
}

###############################################################################@
#' Set DB information
#' 
#' @param x an object with embedded database information
#' 
#' @param value list with the following elements:
#' - **name**: a single character
#' - **title**: a single character
#' - **description**: a single character
#' - **url**: a single character
#' - **version**: a single character
#' - **maintainer**: a single character vector
#' - **size**: a numeric vector providing the size of the DB in bytes
#'
#' @export
#'
'db_info<-' <- function(x, value){
   UseMethod("db_info<-", x)
}


###############################################################################@
#' Get object data model
#'
#' @param x an object with an embedded data model
#' 
#' @return A [ReDaMoR::RelDataModel] object
#' 
#' @export
#'
data_model <- function(x, ...){
   UseMethod("data_model", x)
}


###############################################################################@
#' Get collection members
#' 
#' @param x an object with embedded collection members
#' 
#' @return A [tibble::tibble] with the following columns:
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
#' Get object data tables
#'
#' @param x an object with embedded data tables
#' 
#' @return A list of [dplyr::tibble]
#' 
#' @export
#'
data_tables <- function(x, ...){
   UseMethod("data_tables", x)
}


###############################################################################@
#' Count the number of records
#'
#' @param x an object with embedded data tables
#' 
#' @return A named vector with the number of records per table.
#' 
#' @export
#'
count_records <- function(x, ...){
   UseMethod("count_records", x)
}


###############################################################################@
#' Filter an MDB object according to provided tables
#' 
#' @param x an MDB object
#' @param tables a named list of tibbles to filter with. The names should
#' correspond to the table names in x and the tibbles should fit the
#' data model.
#' @param checkTables if TRUE, the tables are confronted to their model
#' in the data model of x.
#' 
#' @return a [memoMDB] object
#' 
#' @export
#'
filter_with_tables <- function(x, tables, checkTables=TRUE){
   UseMethod("filter_with_tables", x)
}



###############################################################################@
#' Write an MDB object
#'
#' @param x an MDB object
#' @param path the path where the MDB should be written
#' 
#' @return A [fileMDB] object.
#' 
#' @export
#'
write_MDB <- function(x, path, ...){
   UseMethod("write_MDB", x)
}


## Helpers ----
.filter_table <- function(x){
   UseMethod(".filter_table", x)
}
