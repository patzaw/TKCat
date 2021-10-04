###############################################################################@
#' DB information
#' 
#' @param x an object with embedded database information
#' @param ... method specific parameters
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
#' DB information
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
#' @rdname db_info
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
#' @param ... method specific parameters
#' 
#' @return A [ReDaMoR::RelDataModel] object
#' 
#' @export
#'
data_model <- function(x, ...){
   UseMethod("data_model", x)
}


###############################################################################@
#' Collection members
#' 
#' @param x an object with embedded collection members
#' @param ... names of the collections
#' to focus on. By default, all of them are taken.
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
#' Collection members
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
#' @rdname collection_members
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
#' @param ... the name of the tables to get (default: all of them)
#' @param skip the number of rows to skip (default: 0)
#' @param n_max maximum number of rows to return (default: Inf)
#' 
#' @return A list of [dplyr::tibble] and [matrix]
#' 
#' @export
#'
data_tables <- function(x, ..., skip=0, n_max=Inf){
   UseMethod("data_tables", x)
}


###############################################################################@
#' Count the number of records
#'
#' @param x an object with embedded data tables
#' @param ... the name of the tables to consider (default: all of them)
#' 
#' @return A named vector with the number of records per table.
#' 
#' @export
#'
count_records <- function(x, ...){
   UseMethod("count_records", x)
}

###############################################################################@
#' Detailed information about the format of the tables
#'
#' @param x an object with embedded data tables
#' @param ... the name of the tables to consider (default: all of them)
#' 
#' @return A tibble with one row for each considered table and the
#' following columns:
#' 
#' - name: the name of the table
#' - format: "table" or "matrix"
#' - ncol: number of columns
#' - nrow: number of rows
#' - records: number of records (`nrow` for tables and `ncol*nrow` for matrices)
#' - transposed: FALSE by default. TRUE only for matrices stored in a
#' transposed format.
#' 
#' @export
#'
dims <- function(x, ...){
   UseMethod("dims", x)
}

###############################################################################@
#' Get the first records of each object data tables
#'
#' @param x an object with embedded data tables
#' @param ... the name of the tables to get (default: all of them)
#' @param n maximum number of records to return (default: 6)
#' 
#' @return A list of [dplyr::tibble] and [matrix]
#' 
#' @export
#'
heads <- function(x, ..., n=6L){
   UseMethod("heads", x)
}


###############################################################################@
#' Filter an [MDB] object according to provided tables
#' 
#' @param x an [MDB] object
#' @param tables a named list of tibbles to filter with. The names should
#' correspond to the table names in x and the tibbles should fit the
#' data model.
#' @param checkTables if TRUE, the tables are confronted to their model
#' in the data model of x.
#' @param ... method specific parameters
#' 
#' @return a [memoMDB] object
#' 
#' @export
#'
filter_with_tables <- function(x, tables, checkTables=TRUE, ...){
   UseMethod("filter_with_tables", x)
}


###############################################################################@
#' Write an MDB object
#'
#' @param x an MDB object
#' @param path the path where the MDB should be written
#' @param readParameters The following parameters are currently supported:
#' - **delim**: a single character used to separate fields within a record
#' (default: '\\t')
#' - **quoted_na**: a single logical indicating if missing values inside quotes
#' should be treated as missing values or strings.
#' WARNING: THIS PARAMETER IS NOT TAKEN INTO ACCOUNT WITH readr>=2.0.0.
#' - **na**: String used for missing values. The default value for reading
#' a fileMDB is "NA". But the default value for writing a fileMDB
#' is ""&lt;NA&gt;"".
#' This value is written in the DESCRIPTION.json file to avoid ambiguity
#' when reading the fileMDB.
#' @param htmlModel a logical. If TRUE (default) the model is also plotted in
#' an html file.
#' @param compress a logical specifying whether saving data
#' is to use "gzip" compression (default: TRUE)
#' @param by the size of the batch: number of records to write
#' together (default: 10^5)
#' @param ... method specific parameters
#' 
#' @return A [fileMDB] object.
#' 
#' @export
#'
as_fileMDB <- function(
   x, path,
   readParameters=list(delim="\t", na="<NA>"),
   htmlModel=TRUE,
   compress=TRUE,
   by=10^5,
   ...
){
   UseMethod("as_fileMDB", x)
}


###############################################################################@
#' List available [MDB]
#' 
#' @param x a [TKCat] related object (e.g. [chTKCat])
#' @param withInfo if TRUE (default), the function returns a table with
#' [db_info]. If FALSE, it returns only [MDB] names.
#' 
#' @return A tibble with information about the [MDB] available in
#' a [TKCat] related object.
#' 
#' @export
#' 
list_MDBs <- function(x, withInfo=TRUE){
   UseMethod("list_MDBs", x)
}

###############################################################################@
#' Get an [MDB] object from a [TKCat] related object
#' 
#' @param x a [TKCat] related object (e.g. [chTKCat])
#' @param dbName the name of the database
#' @param ... method specific parameters
#' 
#' @return An [MDB] object
#' 
#' @seealso [get_confrontation_report], [ReDaMoR::format_confrontation_report]
#' and [ReDaMoR::format_confrontation_report_md] for getting and formatting
#' the report confronting the data to the model.
#' 
#' @export
#' 
get_MDB <- function(x, dbName, ...){
   UseMethod("get_MDB", x)
}


###############################################################################@
#' Search tables in a [TKCat] related object
#' 
#' @param x a [TKCat] related object (e.g. [chTKCat])
#' @param searchTerm a single character with the term to search
#' 
#' @return An [MDB] object
#' 
#' @export
#' 
search_MDB_tables <- function(x, searchTerm){
   stopifnot(
      is.character(searchTerm),
      length(searchTerm)==1,
      !is.na(searchTerm)
   )
   UseMethod("search_MDB_tables", x)
}


###############################################################################@
#' Search fields in a [TKCat] related object
#' 
#' @param x a [TKCat] related object (e.g. [chTKCat])
#' @param searchTerm a single character with the term to search
#' 
#' @return An [MDB] object
#' 
#' @export
#' 
search_MDB_fields <- function(x, searchTerm){
   stopifnot(
      is.character(searchTerm),
      length(searchTerm)==1,
      !is.na(searchTerm)
   )
   UseMethod("search_MDB_fields", x)
}


###############################################################################@
#' Disconnect an object from a database
#'
#' @param x an object with a database connection
#' 
#' @return No return value, called for side effects
#' 
#' @export
#'
db_disconnect <- function(x){
   UseMethod("db_disconnect", x)
}


###############################################################################@
#' Reconnect an object to a database
#' 
#' @param x an object with a database connection
#' @param user user name. If not provided, it's taken from x
#' @param password user password. If not provided, first the function
#' tries to connect without any password.If it fails, the function asks the
#' user to provide a password.
#' @param ntries the number of times the user can enter a wrong password
#' (default: 3)
#' @param ... additional parameters for methods
#' 
#' @return A new database connection object.
#' 
#' @export
#' 
db_reconnect <- function(x, user, password, ntries=3, ...){
   UseMethod("db_reconnect", x)
}

###############################################################################@
#' Get database hosts
#' 
#' @param x an object with database connection(s)
#' @param ... additional parameters for methods.
#' 
#' @return A character vector with hosts information (generaly 1)
#' in the following shape: "host:port"
#' 
#' @export
#' 
get_hosts <- function(x, ...){
   UseMethod("get_hosts", x)
}



###############################################################################@
#' Get SQL query
#' 
#' @param x an object with a database connection
#' @param query the SQL query
#' @param ... method specific parameters
#' 
#' @return A tibble with query results
#' 
#' @export
#' 
get_query <- function(x, query, ...){
   UseMethod("get_query", x)
}

###############################################################################@
#' Filter a matrix stored in an MDB
#' 
#' @param x an [MDB] object
#' @param tableName a character vector of length 1 corresponding to the name of
#' the table to filter (must be a matrix)
#' @param ... character vectors with the row names and/or columns names to
#' select. The names of the parameters must correspond to the name of the
#' column and of the row fields (the matrix cannot be filtered from values).
#' 
#' @return A sub-matrix of tableName in x. Only existing elements are returned.
#' No error is raised if any element is missing. The result must be checked
#' and adapted to user needs.
#' 
#' @examples
#' \dontrun{
#' ## Return the matrix of expression values focused on the selected genes
#' filter_mdb_matrix(x=db, "Expression_value", gene=c("SNCA", "MAPT"))
#' }
#' 
#' @export
#' 
filter_mdb_matrix <- function(x, tableName, ...){
   UseMethod("filter_mdb_matrix", x)
}


###############################################################################@
#' Explore available [MDB] in a shiny web interface
#' 
#' @param x a [TKCat] related object (e.g. [chTKCat])
#' @param ... method specific parameters
#' 
#' @return No return value, called for side effects
#' 
#' @export
#' 
explore_MDBs <- function(x, ...){
   UseMethod("explore_MDBs", x)
}


###############################################################################@
## Helpers ----
.write_chTables <- function(x, con, dbName, by, ...){
   UseMethod(".write_chTables", x)
}
.build_etkc_ui <- function(x, ...){
   UseMethod(".build_etkc_ui", x)
}
.build_etkc_server <- function(x, ...){
   UseMethod(".build_etkc_server", x)
}
