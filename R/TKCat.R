###############################################################################@
#' TKCat: a catalog of [MDB]
#'
#' @param list a list of [MDB] objects
#' @param ... [MDB] objects used if list is NULL
#'
#' @return a TKCat object
#' 
#' @seealso [scan_fileMDBs]
#' 
#' @export
#'
TKCat <- function(..., list=NULL){
   if(is.null(list)){
      toRet <- list(...)
   }else{
      toRet <- list
   }
   if(any(!unlist(lapply(toRet, is.MDB)))){
      stop("All provided objects should be MDB objects")
   }
   dbnames <- unlist(lapply(toRet, function(x) db_info(x)$name))
   if(any(duplicated(dbnames))){
      stop("MDB objects cannot have the same names")
   }
   names(toRet) <- dbnames
   class(toRet) <- c("TKCat", class(toRet))
   attr(toRet, "tables") <- .get_tkcat_tables(toRet)
   attr(toRet, "fields") <- .get_tkcat_fields(toRet)
   return(toRet)
}


###############################################################################@
#' Scan a catalog of [fileMDB]
#'
#' @param path directory from which all the [fileMDB] should be read
#' @param subdirs the sub directories (relative to path) to take into account.
#' If NULL (default) all the sub directories are considered.
#' @param check logical: if TRUE (default) the data are confronted to the
#' data model
#' @param n_max maximum number of records to read
#' for checks purpose (default: 10). See also [ReDaMoR::confront_data()].
#'
#' @return a TKCat object
#' 
#' @seealso [read_fileMDB]
#' 
#' @export
#'
scan_fileMDBs <- function(path, subdirs=NULL, check=TRUE, n_max=10){
   if(is.null(subdirs)){
      files <- list.files(path=path, full.names=TRUE)
   }else{
      files <- file.path(path, subdirs)
   }
   toRet <- list()
   for(f in files){
      toAdd <- suppressWarnings(try(read_fileMDB(
         path=f, check=check, n_max=n_max
      ), silent=TRUE))
      if(!inherits(toAdd, "try-error")){
         toRet <- c(toRet, list(toAdd))
      }else{
         warning(paste(basename(f), as.character(toAdd), sep=": "))
      }
   }
   return(TKCat(list=toRet))
}


###############################################################################@
#' Check the object is  a [TKCat] object
#' 
#' @param x any object
#' 
#' @return A single logical: TRUE if x is a [TKCat] object
#' 
#' @export
#'
is.TKCat <- function(x){
   inherits(x, "TKCat")
}


###############################################################################@
#' @export
#'
format.TKCat <- function(x, ...){
   toRet <- sprintf("TKCat gathering %s MDB objects", length(x))
   return(toRet)
}


###############################################################################@
#' @export
#'
print.TKCat <- function(x, ...){
   cat(format(x, ...), "\n")
   invisible()
}

###############################################################################@
#' 
#' @param x a [TKCat] object
#' @param value new [MDB] names
#' 
#' @rdname TKCat
#' 
#' @export
#'
'names<-.TKCat' <- function(x, value){
   stopifnot(
      is.character(value),
      !any(is.na(value)),
      !any(duplicated(value)),
      length(value)==length(x)
   )
   dmt <- attr(x, "tables")
   dmf <- attr(x, "fields")
   x <- unclass(x)
   for(i in 1:length(x)){
      dbi <- db_info(x[[i]])
      dmt[which(dmt$resource==dbi$name), "resource"] <- value[i]
      dmf[which(dmf$resource==dbi$name), "resource"] <- value[i]
      dbi$name <- value[i]
      db_info(x[[i]]) <- dbi
   }
   names(x) <- value
   class(x) <- c("TKCat", class(x))
   attr(x, "tables") <- dmt
   attr(x, "fields") <- dmf
   return(x)
}

###############################################################################@
#' Rename a [TKCat] object
#'
#' @param .data a [TKCat] object
#' @param ... Use new_name = old_name to rename selected [MDB]
#' 
#' @rdname TKCat
#' 
#' @export
#' 
rename.TKCat <- function(.data, ...){
   loc <- tidyselect::eval_rename(rlang::expr(c(...)), .data)
   names <- names(.data)
   names[loc] <- names(loc)
   magrittr::set_names(.data, names)
}

###############################################################################@
#' 
#' @param x a [TKCat] object
#' @param i index or names of the MDB to take
#'
#' @rdname TKCat
#' 
#' @export
#'
'[.TKCat' <- function(x, i){
   dmt <- attr(x, "tables")
   dmf <- attr(x, "fields")
   x <- unclass(x)[i]
   class(x) <- c("TKCat", class(x))
   attr(x, "tables") <- dmt %>% dplyr::filter(.data$resource %in% names(x))
   attr(x, "fields") <- dmf %>% dplyr::filter(.data$resource %in% names(x))
   return(x)
}

###############################################################################@
#' @export
#'
'[<-.TKCat' <- function(x, i, value){
   stop("'[<-' is not supported for TKCat")
}

###############################################################################@
#' @export
#'
'[[<-.TKCat' <- function(x, i, value){
   stop("'[[<-' is not supported for TKCat")
}

###############################################################################@
#' @export
#'
'$<-.TKCat' <- function(x, i, value){
   stop("'$<-' is not supported for TKCat")
}


###############################################################################@
#'
#' @param ... [TKCat] objects
#'
#' @rdname TKCat
#' 
#' @export
#'
c.TKCat <- function(...){
   alltkcat <- list(...)
   if(length(alltkcat)==0){
      stop("At least one TKCat should be provided as an input")
   }
   if(any(!unlist(lapply(alltkcat, is.TKCat)))){
      stop("All arguments should be TKCat objects")
   }
   allnames <- unlist(lapply(alltkcat, names))
   if(any(duplicated(allnames))){
      stop("Same names cannot be used in the different TKCat objects")
   }
   dmt <- do.call(dplyr::bind_rows, lapply(
      alltkcat,
      attr, which="tables"
   ))
   dmf <- do.call(dplyr::bind_rows, lapply(
      alltkcat,
      attr, which="fields"
   ))
   toRet <- do.call(c, lapply(alltkcat, unclass))
   class(toRet) <- c("TKCat", class(toRet))
   attr(toRet, "tables") <- dmt
   attr(toRet, "fields") <- dmf
   return(toRet)
}


###############################################################################@
#' 
#' @rdname list_MDBs
#' @method list_MDBs TKCat
#' 
#' @export
#'
list_MDBs.TKCat <- function(x, withInfo=TRUE){
   if(!withInfo){
      return(names(x))
   }
   return(do.call(dplyr::bind_rows, lapply(
      x,
      function(y) dplyr::as_tibble(db_info(y))
   )))
}


###############################################################################@
#' 
#' @rdname search_MDB_tables
#' @method search_MDB_tables TKCat
#' 
#' @export
#'
search_MDB_tables.TKCat <- function(x, searchTerm){
   dmt <- attr(x, "tables")
   toTake <- unique(c(
      grep(searchTerm, dmt$name, ignore.case=TRUE),
      grep(searchTerm, dmt$comment, ignore.case=TRUE)
   ))
   toRet <- dmt %>% dplyr::slice(c(0, toTake))
   return(toRet)
}


###############################################################################@
#' 
#' @rdname search_MDB_fields
#' @method search_MDB_fields TKCat
#' 
#' @export
#'
search_MDB_fields.TKCat <- function(x, searchTerm){
   dmf <- attr(x, "fields")
   toTake <- unique(c(
      grep(searchTerm, dmf$name, ignore.case=TRUE),
      grep(searchTerm, dmf$comment, ignore.case=TRUE)
   ))
   toRet <- dmf %>% dplyr::slice(c(0, toTake))
   return(toRet)
}



###############################################################################@
#' 
#' @rdname get_MDB
#' @method get_MDB TKCat
#' 
#' @export
#'
get_MDB.TKCat <- function(x, dbName, ...){
   stopifnot(dbName %in% names(x))
   return(x[[dbName]])
}

###############################################################################@
#' 
#' @rdname collection_members
#' @method collection_members TKCat
#' 
#' @export
#'
collection_members.TKCat <- function(
   x,
   ...
){
   return(do.call(dplyr::bind_rows, lapply(
      x,
      function(y){
         cm <- collection_members(y)
         if(is.null(cm)){
            return(NULL)
         }
         cm %>% 
            dplyr::select("resource", "collection", "table") %>% 
            dplyr::distinct()
      }
   )))
}


###############################################################################@
#### SHINY EXPLORER ####
###############################################################################@


###############################################################################@
#'
#' @param subSetSize the maximum number of records to show
#' @param download a logical indicating if data can be downloaded
#' (default: FALSE). If TRUE a temporary directory is created and made
#' available for shiny.
#' @param workers number of available workers when download is available
#' (default: 4)
#' @param skinColors one color for the application skin.
#' Working values: "blue", "black", "purple", "green", "red", "yellow".
#' @param title A title for the application. If NULL (default):
#' the chTKCat instance name
#' @param logoDiv a [shiny::div] object with a logo to display in side bar.
#' The default is the TKCat hex sticker with a link to TKCat github repository.
#' @param rDirs a named character vector with resource path
#' for [shiny::addResourcePath]
#' @param tabTitle a title to display in tab (default: "chTKCat")
#' @param tabIcon a path to an image
#' (in available resource paths: "www", "doc" or in rDirs) to use as a tab icon.
#' 
#' @rdname explore_MDBs
#' @method explore_MDBs TKCat
#' 
#' @export
#'
explore_MDBs.TKCat <- function(
   x,
   subSetSize=100,
   download=FALSE,
   workers=4,
   title=NULL,
   skinColors="green",
   logoDiv=TKCAT_LOGO_DIV,
   rDirs=NULL,
   tabTitle="TKCat",
   tabIcon='www/TKCat-small.png',
   ...
){
   stopifnot(
      is.logical(download), length(download)==1, !is.na(download),
      is.character(skinColors), length(skinColors)>0, all(!is.na(skinColors))
   )
   skinColors <- skinColors[1]
   if(download){
      ddir <- tempfile()
      dir.create(ddir)
      oplan <- future::plan(
         future::multisession, workers=workers
      )
   }else{
      ddir <- NULL
   }
   shiny::shinyApp(
      ui=.build_etkc_ui(
         x=x, ddir=ddir, skinColors=skinColors,
         logoDiv=logoDiv, rDirs=rDirs,
         tabTitle=tabTitle, tabIcon=tabIcon,
      ),
      server=.build_etkc_server(
         x=x,
         subSetSize=subSetSize,
         ddir=ddir,
         title=title
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
.build_etkc_ui.TKCat <- function(
   x, ddir=NULL, skinColors="green",
   logoDiv=TKCAT_LOGO_DIV, rDirs=NULL,
   tabTitle="TKCat",
   tabIcon='www/TKCat-small.png',
   ...
){
   
   .etkc_add_resources(ddir=ddir, rDirs=rDirs)
   
   function(req){
      shinydashboard::dashboardPage(
         title=tabTitle,
         skin=skinColors[1],
         
         ########################@
         ## Dashboard header ----
         ## Uses output$instance and output$status
         header=.etkc_sd_header(),
         
         ########################@
         ## Sidebar ----
         ## Uses uiOutput("currentUser") and uiOutput("signin")
         sidebar=.etkc_sd_sidebar(
            sysInterface=FALSE,
            manList=c(
               "General TKCat user guide"="doc/TKCat-User-guide.html"
            ),
            logoDiv=logoDiv
         ),
         
         ########################@
         ## Body ----
         body=.etkc_sd_body(sysInterface=FALSE, tabIcon=tabIcon)
      )
   }
   
}


###############################################################################@
.build_etkc_server.TKCat <- function(
   x,
   subSetSize=100,
   ddir=NULL,
   title=NULL
){
   .build_etkc_server_default(
      x=x, subSetSize=subSetSize,
      ddir=ddir,
      title=title
   )
}


###############################################################################@
## Helpers ----
.get_tkcat_tables <- function(x){
   dmt <- c()
   for(n in names(x)){
      dm <- data_model(get_MDB(x, n))
      dmt <- dplyr::bind_rows(
         dmt,
         ReDaMoR::toDBM(dm)$tables %>% 
            dplyr::mutate(resource=n) %>% 
            dplyr::select("resource", "name", "comment")
      )
   }
   return(dmt)
}
.get_tkcat_fields <- function(x){
   dmf <- c()
   for(n in names(x)){
      dm <- data_model(get_MDB(x, n))
      dmf <- dplyr::bind_rows(
         dmf,
         ReDaMoR::toDBM(dm)$fields %>% 
            dplyr::mutate(resource=n) %>% 
            dplyr::select(
               "resource", "table", "name",
               "type", "nullable", "unique", "comment"
            )
      )
   }
   return(dmf)
}
