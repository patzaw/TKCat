###############################################################################@
#' An [MDB] (Modeled DataBase) based on files: fileMDB
#' 
#' @param dataFiles a named vector of path to data files with
#' `all(names(dataFiles) %in% names(dataModel))`
#' @param dbInfo a list with DB information:
#' **"name"** (only mandatory field), "title", "description", "url",
#' "version", "maintainer".
#' @param dataModel a [ReDaMoR::RelDataModel] object
#' @param readParameters a list of parameters for reading the data file.
#' (e.g. `list(delim='\t', quoted_na=FALSE,)`)
#' @param collectionMembers the members of collections as provided to the
#' [collection_members<-] function (default: NULL ==> no member).
#' @param n_max maximum number of records to read
#' for checks purpose (default: 10). See also [ReDaMoR::confront_data()].
#' @param verbose if TRUE display the data confrontation report
#' (default: FALSE)
#'
#' @return A fileMDB object
#' 
#' @seealso
#' - MDB methods:
#' [db_info], [data_model], [data_tables], [collection_members],
#' [count_records], [filter_with_tables], [as_fileMDB]
#' - Additional general documentation is related to [MDB].
#' - [filter.fileMDB], [slice.fileMDB]
#' 
#' @example inst/examples/fileMDB-examples.R
#' 
#' @export
#' 
fileMDB <- function(
   dataFiles,
   dbInfo,
   dataModel,
   readParameters=DEFAULT_READ_PARAMS,
   collectionMembers=NULL,
   n_max=10,
   verbose=FALSE
){
   
   ## Data files ----
   stopifnot(
      all(file.exists(dataFiles)),
      all(names(dataFiles) %in% names(dataModel))
   )
   dataFiles <- dataFiles[names(dataModel)]
   dataFiles <- normalizePath(dataFiles)
   names(dataFiles) <- names(dataModel)
   
   ## DB information ----
   dbInfo <- .check_dbInfo(dbInfo)
   
   ## Data model ----
   if(!ReDaMoR::is.RelDataModel(dataModel)){
      stop(
         "dataModel should be a RelDataModel object"
      )
   }
   
   ## Read parameters ----
   readParameters <- .check_read_params(readParameters)
   
   ## Confront data to model ----
   rnDataModel <- dataModel
   if(length(dataModel)>0){
      names(rnDataModel) <- sub(
         pattern="(\\.[[:alnum:]]+)(\\.gz)?$", replacement="",
         x=basename(dataFiles)
      )
      cr <- do.call(ReDaMoR::confront_data, c(
         list(
            rnDataModel,
            paths=dataFiles,
            returnData=FALSE,
            verbose=FALSE,
            n_max=n_max
         ),
         readParameters
      ))
      assign("confrontationReport", cr, envir=tkcatEnv)
      if(!cr$success){
         stop(ReDaMoR::format_confrontation_report(cr, title=dbInfo[["name"]]))
      }
      if(verbose){
         cat(ReDaMoR::format_confrontation_report(cr, title=dbInfo[["name"]]))
      }
   }
   
   ## Object ----
   toRet <- list(
      dataFiles=dataFiles,
      dataModel=dataModel,
      dbInfo=dbInfo,
      readParameters=readParameters
   )
   class(toRet) <- c("fileMDB", "MDB", class(toRet))
   
   ## Collection members ----
   collection_members(toRet) <- collectionMembers
   
   return(toRet)
}

###############################################################################@
#' Read a [fileMDB] from a path
#' 
#' @param path the path to a folder with data or with the following structure:
#' - **data**: a folder with the data
#' - **DESCRIPTION.json**: a file with db information
#' - **model**: a folder with the data model json file with the same name as the
#' one given in the DESCRIPTION.json file
#' @param dbInfo a list or a json file with DB information:
#' **"name"** (only mandatory field), "title", "description",
#' "url" (or "reference URL"),
#' "version", "maintainer". If NULL (default), the DESCRIPTION.json file found
#' in path. This file should also contains relevant parameters for the
#' [readr::read_delim()] function. For example:
#' - **delim delimiter** (default: '\\\\t')
#' - **quoted_na**: Should missing values inside quotes be treated
#' as missing values or as strings or strings (the default).
#' Be aware that the default value here is different than the one for the
#' original [readr::read_delim()] function.
#' @param dataModel a [ReDaMoR::RelDataModel] object or json file.
#' If NULL (default), the model json file found in path/model.
#' @param collectionMembers the members of collections as provided to the
#' [collection_members<-] function. If NULL (default), the members
#' are taken from json files found in path/model/Collections
#' @param n_max maximum number of records to read
#' for checks purpose (default: 10). See also [ReDaMoR::confront_data()].
#' @param verbose if TRUE (default) display the data confrontation report
#'
#' @return A [fileMDB] object
#' 
#' @seealso [get_confrontation_report], [ReDaMoR::format_confrontation_report]
#' and [ReDaMoR::format_confrontation_report_md] for getting and formatting
#' the report confronting the data to the model.
#' 
#' @export
#' 
read_fileMDB <- function(
   path,
   dbInfo=NULL,
   dataModel=NULL,
   collectionMembers=NULL,
   n_max=10,
   verbose=TRUE
){
   stopifnot(file.exists(path))
   
   ## DB information ----
   if(is.null(dbInfo)){
      dbifp <- file.path(path, "DESCRIPTION.json")
      if(!file.exists(dbifp)){
         stop(sprintf("%s does not exist", dbifp))
      }
      dbInfo <- jsonlite::read_json(dbifp)
   }else{
      if(is.character(dbInfo)){
         stopifnot(
            length(dbInfo)==1,
            !is.na(dbInfo),
            file.exists(dbInfo)
         )
         dbInfo <- jsonlite::read_json(dbInfo)
      }
      
   }
   names(dbInfo) <- sub("^reference URL$", "url", names(dbInfo))
   allInfo <- dbInfo
   dbInfo <- .check_dbInfo(dbInfo)
   
   ## Read parameters ----
   readParameters <- .check_read_params(allInfo)
   
   ## Data model ----
   if(is.null(dataModel)){
      dataModel <- ReDaMoR::read_json_data_model(
         file.path(path, "model", sprintf("%s.json", dbInfo[["name"]]))
      )
   }else{
      if(is.character(dataModel)){
         stopifnot(
            length(dataModel)==1,
            !is.na(dataModel),
            file.exists(dataModel)
         )
         dataModel <- ReDaMoR::read_json_data_model(dataModel)
      }
   }
   if(!ReDaMoR::is.RelDataModel(dataModel)){
      stop(
         "dataModel should be a path to a valid json file ",
         "or a RelDataModel object"
      )
   }
   
   ## Data ----
   dp <- file.path(path, "data")
   if(!file.exists(dp)){
      dp <- path
   }
   dataFiles <- lapply(
      names(dataModel),
      function(x){
         f <- list.files(dp, pattern=paste0(x, "(\\.[[:alnum:]]+)(\\.gz)?$"))
         if(length(f)>1){
            stop(sprintf("There are several files for %s", x))
         }
         if(length(f)==0){
            stop(sprintf("Cannot find a file for %s", x))
         }
         return(f)
      }
   ) %>% unlist()
   dataFiles <- file.path(dp, dataFiles) %>% 
      normalizePath() %>% 
      magrittr::set_names(names(dataModel))
   
   ## Collection members ----
   if(is.null(collectionMembers)){
      for(
         cm in list.files(
            path=file.path(path, "model", "Collections"),
            pattern="[.]json$",
            full.names=TRUE
         )
      ){
         collectionMembers <- dplyr::bind_rows(
            collectionMembers,
            read_collection_members(cm)
         )
      }
   }
   
   ## Object ----
   return(fileMDB(
      dataFiles=dataFiles,
      dbInfo=dbInfo,
      dataModel=dataModel,
      readParameters=readParameters,
      collectionMembers=collectionMembers,
      n_max=n_max,
      verbose=verbose
   ))
}


###############################################################################@
#' Check if the object is  a [fileMDB] object
#' 
#' @param x any object
#' 
#' @return A single logical: TRUE if x is an [fileMDB] object
#' 
#' @export
#'
is.fileMDB <- function(x){
   inherits(x, "fileMDB")
}


###############################################################################@
#' 
#' @param x a [fileMDB] object
#' @param value new table names
#' 
#' @rdname fileMDB
#' 
#' @export
#'
'names<-.fileMDB' <- function(x, value){
   colMb <- collection_members(x)
   ovalues <- names(x)
   x <- unclass(x)
   ncolMb <- NULL
   if(!is.null(colMb)){
      ncolMb <- NULL
      for(i in 1:length(ovalues)){
         toAdd <- colMb %>% dplyr::filter(.data$table==!!ovalues[i])
         if(nrow(toAdd)>0){
            toAdd$table <- value[i]
            ncolMb <- ncolMb %>% dplyr::bind_rows(toAdd)
         }
      }
   }
   names(x$dataModel) <- names(x$dataFiles) <-value
   class(x) <- c("fileMDB", class(x))
   collection_members(x) <- ncolMb
   return(x)
}


###############################################################################@
#' Rename tables of a [fileMDB] object
#'
#' @param .data a [fileMDB] object
#' @param ... Use new_name = old_name to rename selected tables
#' 
#' @rdname fileMDB
#' 
#' @export
#' 
rename.fileMDB <- function(.data, ...){
   loc <- tidyselect::eval_rename(expr(c(...)), .data)
   names <- names(.data)
   names[loc] <- names(loc)
   set_names(.data, names)
}


###############################################################################@
#' 
#' @rdname db_info
#' @method db_info fileMDB
#' 
#' @export
#'
db_info.fileMDB <- function(x, ...){
   return(unclass(x)$dbInfo)
}


###############################################################################@
#' 
#' @rdname db_info
#' @method db_info<- fileMDB
#' 
#' @export
#'
'db_info<-.fileMDB' <- function(x, value){
   toRet <- unclass(x)
   dbInfo <- .check_dbInfo(value)
   toRet$dbInfo <- dbInfo
   if(!is.null(toRet$collectionMembers)){
      toRet$collectionMembers$resource <- dbInfo$name
   }
   class(toRet) <- c("fileMDB", "MDB", class(toRet))
   return(toRet)
}


###############################################################################@
#' 
#' @rdname data_model
#' @method data_model fileMDB
#' 
#' @export
#'
data_model.fileMDB <- function(x, ...){
   unclass(x)$dataModel
}


###############################################################################@
#' 
#' @rdname collection_members
#' @method collection_members fileMDB
#' 
#' @export
#'
collection_members.fileMDB <- function(
   x,
   ...
){
   x <- unclass(x)
   toRet <- x$"collectionMembers"
   toTake <- unlist(list(...))
   if(length(toTake)>0){
      stopifnot(is.character(toTake))
      toRet <- toRet[which(toRet$collection %in% toTake),]
   }
   return(toRet)
}


###############################################################################@
#'
#' @rdname collection_members
#' @method collection_members<- fileMDB
#' 
#' @export
#'
'collection_members<-.fileMDB' <- function(x, value){
   
   if(is.null(value)){
      x <- unclass(x)
      x$"collectionMembers" <- value
      class(x) <- c("fileMDB", "MDB", class(x))
      return(x)
   }
   
   stopifnot(
      is.data.frame(value),
      all(
         colnames(value) %in%
            c(
               "collection", "cid", "resource", "mid", "table",
               "field", "static", "value", "type"
            )
      ),
      is.character(value$collection),
      is.character(value$cid),
      is.character(value$resource),
      is.integer(value$mid),
      is.character(value$table),
      is.character(value$field),
      is.logical(value$static),
      is.character(value$value),
      is.character(value$type),
      all(value$resource==db_info(x)$name),
      all(value$collection %in% list_local_collections()$title),
      sum(duplicated(
         dplyr::select(value, "collection", "mid", "table", "field")
      ))==0
   )
   
   for(collection in unique(value$collection)){
      cv <- dplyr::filter(value, .data$collection==!!collection)
      for(cid in unique(cv$cid)){
         jval <- dplyr::filter(cv, .data$cid==!!cid) %>%
            write_collection_members()
      }
   }
   
   for(mbt in unique(value$table)){
      notFound <- setdiff(
         value$value[which(value$table==mbt & !value$static)],
         data_model(x)[[mbt]]$fields$name
      )
      if(length(notFound)>0){
         stop(
            sprintf(
               "Cannot find the following fields in the %s table: ",
               mbt
            ),
            paste(notFound, collapse=", ")
         )
      }
   }
   x <- unclass(x)
   x$"collectionMembers" <- value
   class(x) <- c("fileMDB", "MDB", class(x))
   return(x)
}


###############################################################################@
#' 
#' @rdname data_tables
#' @method data_tables fileMDB
#' 
#' @export
#'
data_tables.fileMDB <- function(x, ..., skip=0, n_max=Inf){
   stopifnot(
      is.numeric(skip), length(skip)==1, skip>=0, is.finite(skip),
      is.numeric(n_max), length(n_max)==1, n_max>0
   )
   m <- data_model(x)
   toTake <- tidyselect::eval_select(expr(c(...)), x)
   if(length(toTake)==0){
      toTake <- 1:length(x)
      names(toTake) <- names(x)
   }
   x <- unclass(x)
   toRet <- lapply(
      toTake,
      function(y){
         ht <- do.call(readr::read_delim, c(
            list(
               file=x$dataFiles[y],
               col_types=ReDaMoR::col_types(m[[y]]),
               skip=0, n_max=0
            ),
            x$readParameters
         ))
         toRet <- do.call(readr::read_delim, c(
            list(
               file=x$dataFiles[y],
               col_names=colnames(ht),
               col_types=ReDaMoR::col_types(m[[y]]),
               skip=skip+1,
               n_max=n_max
            ),
            x$readParameters
         ))
         attr(toRet, "spec") <- NULL
         return(toRet)
      }
   )
   names(toRet) <- names(toTake)
   return(toRet)
}


###############################################################################@
#' 
#' @rdname count_records
#' @method count_records fileMDB
#' 
#' @export
#'
count_records.fileMDB <- function(x, ...){
   count_lines <- function(f, by=10^5){
      con <- file(f, "r")
      on.exit(close(con))
      d <- c()
      n <- cn <- length(readLines(con, n=by))
      while(cn>0){
         cn <- length(readLines(con, n=by))
         n <- n+cn
      }
      return(n)
   }
   toTake <- tidyselect::eval_select(expr(c(...)), x)
   if(length(toTake)==0){
      toTake <- 1:length(x)
      names(toTake) <- names(x)
   }
   x <- unclass(x)
   lapply(x$dataFiles[toTake], count_lines) %>% 
      unlist() %>% 
      `-`(1)
}


###############################################################################@
#' Get the data files from a [fileMDB] object
#' 
#' @param x a [fileMDB] object
#' 
#' @return a list with "dataFiles" and "readParameters" for reading the files.
#' 
#' @export
#'
data_files <- function(x){
   stopifnot(is.fileMDB(x))
   x <- unclass(x)
   toTake <- c("dataFiles", "readParameters")
   stopifnot(all(toTake %in% names(x)))
   return(x[toTake])
}


###############################################################################@
#' Get the size of data files from a [fileMDB] object
#' 
#' @param x a [fileMDB] object
#' @param hr a logical indicating if the values should be "human readable".
#' (default: FALSE)
#' 
#' @return a numeric vector with size in bytes (hr=FALSE) or
#' a character vector with size and units (hr=TRUE)
#' 
#' @export
#'
data_file_size <- function(x, hr=FALSE){
   df <- data_files(x)$dataFiles
   toRet <- file.size(df)
   if(hr){
      toRet <- .format_file_size(toRet)
   }
   names(toRet) <- names(df)
   return(toRet)
}


###############################################################################@
#' 
#' @param x a [fileMDB] object
#' @param i index or names of the tables to take
#'
#' @rdname fileMDB
#' 
#' @export
#'
'[.fileMDB' <- function(x, i){
   if(missing(i)){
      return(x)
   }
   if(length(i)==0){
      dbi <- db_info(x)
      return(fileMDB(
         dataFiles=as.character(),
         dbInfo=dbi,
         dataModel=RelDataModel(l=list()),
         readParameters=data_files(x)$readParameters
      ))
   }
   stopifnot(
      is.character(i) || is.numeric(i),
      all(!is.na(i))
   )
   if(is.numeric(i)){
      stopifnot(all(i %in% 1:length(x)))
      i <- names(x)[i]
   }
   if(is.character(i)){
      stopifnot(all(i %in% names(x)))
   }
   dbi <- db_info(x)
   dm <- data_model(x)[i, rmForeignKeys=TRUE]
   df <- data_files(x)
   rp <- df$readParameters
   df <- df$dataFiles[i]
   cm <- collection_members(x)
   if(!is.null(cm)){
      cm <- cm %>%
         dplyr::filter(.data$table %in% !!i) %>%
         dplyr::mutate(resource=!!dbi$name)
   }
   toRet <- fileMDB(
      dataFiles=df,
      dbInfo=dbi,
      dataModel=dm,
      readParameters=rp,
      collectionMembers=cm
   )
   return(toRet)
}


###############################################################################@
#' 
#' @param x a [fileMDB] object
#' @param i the index or the name of the tables to take
#' 
#' @export
#'
#' @rdname fileMDB
#'
'[[.fileMDB' <- function(x, i){
   stopifnot(
      length(i)==1
   )
   ## Rstudio hack to avoid DB call when just looking for names
   cc <- grep('.rs.getCompletionsDollar', deparse(sys.calls()), value=FALSE)
   if(length(cc)!=0){
      invisible(NULL)
   }else{
      cc <- c(
         grep('.rs.valueContents', deparse(sys.calls()), value=FALSE),
         grep('.rs.explorer.inspectObject', deparse(sys.calls()), value=FALSE)
      )
      if(length(cc)!=0){
         invisible(as.character(data_files(x)$dataFiles[i]))
      }else{
         return(data_tables(x, dplyr::all_of(i))[[1]])
      }
   }
}
#' @rdname fileMDB
#' @export
'$.fileMDB' <- `[[.fileMDB`


###############################################################################@
#'
#' @param ... [fileMDB] objects
#'
#' @rdname fileMDB
#' 
#' @export
#'
c.fileMDB <- function(...){
   alldb <- list(...)
   if(length(alldb)==0){
      stop("At least one fileMDB should be provided as an input")
   }
   df <- data_files(alldb[[1]])
   rp1 <- df$readParameters
   for(i in 1:length(alldb)){
      if(!is.fileMDB(alldb[[i]])){
         stop("All objects should be fileMDB")
      }
      rpi <- data_files(alldb[[i]])$readParameters[names(rp1)]
      if(!identical(rp1, rpi)){
         stop("readParameters of all fileMDB should be identical")
      }
   }
   di <- db_info(alldb[[1]])
   dm <- data_model(alldb[[1]])
   df <- data_files(alldb[[1]])
   dc <- collection_members(alldb[[1]])
   if(length(alldb)>1) for(i in 2:length(alldb)){
      dm <- c(dm, data_model(alldb[[i]]))
      df$dataFiles <- c(df$dataFiles, data_files(alldb[[i]])$dataFiles)
      dc <- dplyr::bind_rows(
         dc,
         collection_members(alldb[[i]]) %>%
            dplyr::mutate(resource=di$name)
      )
   }
   fileMDB(
      dataFiles=df$dataFiles,
      dbInfo=di,
      dataModel=dm,
      readParameters=df$readParameters,
      collectionMembers=dc
   )
}


###############################################################################@
#' 
#' @rdname as_fileMDB
#' @method as_fileMDB fileMDB
#' 
#' @export
#'
as_fileMDB.fileMDB <- function(
   x, path,
   readParameters=DEFAULT_READ_PARAMS,
   htmlModel=TRUE,
   ...
){
   stopifnot(is.character(path), length(path)==1, !is.na(path))
   dbInfo <- db_info(x)
   dbName <- dbInfo$name
   
   ## Initialization ----
   fullPath <- file.path(path, dbName)
   if(file.exists(fullPath)){
      stop(sprintf("%s already exists", fullPath))
   }
   dir.create(fullPath, recursive=TRUE)
   
   ## Description file ----
   readParameters <- .check_read_params(readParameters)
   descFile <- file.path(fullPath, "DESCRIPTION.json")
   .writeDescription(c(dbInfo, readParameters), descFile)
   rp <- data_files(x)$readParameters
   if(
      !identical(sort(names(rp)), sort(names(readParameters))) ||
      !all(unlist(lapply(
         names(readParameters), 
         function(rpn) readParameters[[rpn]]!=rp[[rpn]]
      )))
   ){
      rewrite=TRUE
   }else{
      rewrite=FALSE
   }
   
   ## Data model ----
   dm <- data_model(x)
   modelPath <- file.path(fullPath, "model")
   dir.create(modelPath)
   jModelPath <- file.path(modelPath, paste0(dbName, ".json"))
   hModelPath <- file.path(modelPath, paste0(dbName, ".html"))
   write_json_data_model(dm, jModelPath)
   if(htmlModel){
      plot(dm) %>%
         visNetwork::visSave(hModelPath)
   }
   
   ## Collection members ----
   cm <- collection_members(x)
   if(!is.null(cm) && nrow(cm)>0){
      colPath <- file.path(modelPath, "Collections")
      dir.create(colPath)
      for(collection in unique(cm$collection)){
         cv <- dplyr::filter(cm, .data$collection==!!collection)
         for(cid in unique(cv$cid)){
            dplyr::filter(cv, .data$cid==!!cid) %>%
               write_collection_members(path=file.path(
                  colPath,
                  paste0(collection, "-", cid, ".json")
               ))
         }
      }
   }
   
   ## Data ----
   dataPath <- file.path(fullPath, "data")
   dir.create(dataPath)
   ofiles <- data_files(x)$dataFiles
   dfiles <- file.path(dataPath, basename(ofiles)) %>%
      magrittr::set_names(names(ofiles))
   if(rewrite){
      for(tn in names(x)){
         do.call(
            readr::read_delim_chunked,
            c(
               list(file=ofiles[tn]),
               rp,
               list(
                  col_types=ReDaMoR::col_types(dm[[tn]])
               ),
               list(
                  callback=readr::DataFrameCallback$new(function(y, pos){
                     readr::write_delim(
                        y, file=dfiles[tn], delim=readParameters$delim,
                        append=file.exists(dfiles[tn])
                     )
                  }),
                  chunk_size=10^5
               )
            )
         )
      }
   }else{
      file.copy(ofiles, dfiles)
   }
   
   ## Return fileMDB ----
   return(fileMDB(
      dataFiles=dfiles,
      dbInfo=dbInfo,
      dataModel=dm,
      readParameters=readParameters,
      collectionMembers=cm
   ))
}


###############################################################################@
#' Filter a [fileMDB] object and return a [memoMDB]
#' 
#' @param .data a [fileMDB] object
#' @param ... each argument should have the name of one of the tables of the
#' [fileMDB] object and contain a simple logical expression involving
#' the names of the corresponding table.
#' @param .preserve not used
#' 
#' @return a [memoMDB] object
#' 
#' @export
#'
filter.fileMDB <- function(.data, ..., .preserve=FALSE){
   
   x <- .data
   dm <- data_model(x)
   
   ## Apply rules
   toRet <- list()
   dots <- enquos(...)
   files <- data_files(x)
   rp <- files$readParameters
   files <- files$dataFiles
   for(tn in names(dots)){
      if(!tn %in% names(x)){
         stop(sprintf("%s table does not exist", tn))
      }
      toRet[[tn]] <- do.call(
         readr::read_delim_chunked,
         c(
            list(file=files[tn]),
            rp,
            list(
               col_types=ReDaMoR::col_types(dm[[tn]])
            ),
            list(
               callback=readr::DataFrameCallback$new(function(y, pos){
                  dplyr::filter(y, !!dots[[tn]])
               }),
               chunk_size=10^5
            )
         )
      )
   }
   
   ## Filter with tables
   return(filter_with_tables(x, toRet, checkTables=FALSE))
   
}


###############################################################################@
#' Subset a [fileMDB] object according to row position in one table
#' and return a [memoMDB]
#' 
#' @param .data a [fileMDB] object
#' @param ... a single argument. The name of this argument should be a table
#' name of x and the value of this argument should be vector of integers
#' corresponding to row indexes.
#' @param .preserve not used
#' 
#' @return a [memoMDB] object
#' 
#' @export
#'
slice.fileMDB <- function(.data, ..., .preserve=FALSE){
   
   x <- .data
   
   ## Apply rules
   toRet <- list()
   dots <- list(...)
   if(length(dots)>1){
      stop("Only one argument should be supplied in '...'")
   }
   tn <- names(dots)
   if(!tn %in% names(x)){
      stop(sprintf("%s table does not exist", tn))
   }
   i <- dots[[tn]]
   
   dm <- data_model(x)
   files <- data_files(x)
   rp <- files$readParameters
   files <- files$dataFiles
   toRet[[tn]] <- do.call(
      readr::read_delim_chunked,
      c(
         list(file=files[tn]),
         rp,
         list(
            col_types=ReDaMoR::col_types(dm[[tn]])
         ),
         list(
            callback=readr::DataFrameCallback$new(function(y, pos){
               j <- i-pos+1
               j <- j[j>0]
               dplyr::slice(y, j)
            }),
            chunk_size=10^5
         )
      )
   )
   
   ## Filter with tables
   return(filter_with_tables(x, toRet, checkTables=FALSE))
   
}


###############################################################################@
#' 
#' @rdname filter_with_tables
#' @method filter_with_tables fileMDB
#' 
#' @export
#'
filter_with_tables.fileMDB <- function(x, tables, checkTables=TRUE){
   
   ## Check the tables ----
   if(checkTables){
      for(tn in names(tables)){
         cr <- ReDaMoR::confront_table_data(data_model(x)[[tn]], tables[[tn]])
         if(!cr$success){
            stop(sprintf("The %s table does not fit the data model"), tn)
         }
      }
   }
   
   ## Identify foreign keys ----
   dm <- data_model(x)
   fk <- ReDaMoR::get_foreign_keys(dm)
   
   ## Filter by contamination ----
   tables <- .file_filtByConta(tables, x, fk)
   dm <- dm[names(tables), rmForeignKeys=TRUE]
   tables <- .norm_data(tables,  dm)
   
   ## Collection members ----
   cm <- collection_members(x)
   if(!is.null(cm)){
      cm <- cm %>% dplyr::filter(.data$table %in% names(tables))
   }
   
   ## Results ----
   return(memoMDB(
      dataTables=tables,
      dataModel=dm,
      dbInfo=db_info(x),
      collectionMembers=cm
   ))
   
}

###############################################################################@
## READ PARAMETERS -----
DEFAULT_READ_PARAMS <- list(delim='\t', quoted_na=FALSE)

.check_read_params <- function(readParameters){
   readParameters <- readParameters[intersect(
      names(readParameters), names(DEFAULT_READ_PARAMS)
   )]
   if("delim" %in% names(readParameters)){
      stopifnot(
         length(readParameters$delim)==1,
         is.character(readParameters$delim),
         !is.na(readParameters$delim)
      )
   }
   if("quoted_na" %in% names(readParameters)){
      stopifnot(
         length(readParameters$quoted_na)==1,
         is.logical(readParameters$quoted_na),
         !is.na(readParameters$quoted_na)
      )
   }
   return(c(
      readParameters,
      DEFAULT_READ_PARAMS[setdiff(
         names(DEFAULT_READ_PARAMS), names(readParameters)
      )]
   )[names(DEFAULT_READ_PARAMS)])
}


###############################################################################@
## Helpers ----
.write_chTables.fileMDB <- function(x, con, dbName, by=10^5){
   dm <- data_model(x)
   df <- data_files(x)
   rp <- df$readParameters
   df <- df$dataFiles
   for(tn in names(x)){
      do.call(
         readr::read_delim_chunked,
         c(
            list(file=df[tn]),
            rp,
            list(
               col_types=ReDaMoR::col_types(dm[[tn]])
            ),
            list(
               callback=readr::DataFrameCallback$new(function(y, pos){
                  ch_insert(con=con, dbName=dbName, tableName=tn, value=y)
               }),
               chunk_size=by
            )
         )
      )
   }
}

.file_filtByConta <- function(d, fdb, fk){
   nfk <- fk
   files <- data_files(fdb)
   rp <- files$readParameters
   files <- files$dataFiles
   dm <- data_model(fdb)
   .contaminate <- function(tn){
      
      ## Backward ----
      fkf <- fk %>% dplyr::filter(.data$from==!!tn & .data$fmin>0)
      fkt <- fk %>% dplyr::filter(.data$to==!!tn & .data$tmin>0)
      nfk <<- nfk %>%
         dplyr::anti_join(select(fkf, "from", "to"), by=c("from", "to")) %>% 
         dplyr::anti_join(select(fkt, "from", "to"), by=c("from", "to"))
      fkl <- dplyr::bind_rows(
         fkf,
         fkt %>% dplyr::rename("from"="to", "ff"="tf", "to"="from", "tf"="ff")
      ) %>% 
         distinct()
      if(nrow(fkl)>0){
         for(i in 1:nrow(fkl)){
            ntn <- fkl$to[i]
            if(ntn %in% names(d)){
               nv <- dplyr::semi_join(
                  d[[ntn]], d[[tn]],
                  by=magrittr::set_names(
                     fkl$ff[[i]], fkl$tf[[i]]
                  )
               )
            }else{
               nv <- do.call(
                  readr::read_delim_chunked,
                  c(
                     list(file=files[ntn]),
                     rp,
                     list(
                        col_types=ReDaMoR::col_types(dm[[ntn]])
                     ),
                     list(
                        callback=readr::DataFrameCallback$new(function(y, pos){
                           dplyr::semi_join(
                              y, d[[tn]],
                              by=magrittr::set_names(
                                 fkl$ff[[i]], fkl$tf[[i]]
                              )
                           )
                        }),
                        chunk_size=10^5
                     )
                  )
               )
            }
            d[[ntn]] <<- nv
         }
      }

      ## Forward ----
      fkf <- fk %>% dplyr::filter(.data$from==!!tn & .data$fmin==0)
      fkt <- fk %>% dplyr::filter(.data$to==!!tn & .data$tmin==0)
      nfk <<- nfk %>%
         dplyr::anti_join(select(fkf, "from", "to"), by=c("from", "to")) %>% 
         dplyr::anti_join(select(fkt, "from", "to"), by=c("from", "to"))
      fkl <- dplyr::bind_rows(
         fkf,
         fkt %>% dplyr::rename("from"="to", "ff"="tf", "to"="from", "tf"="ff")
      ) %>% 
         distinct()
      if(nrow(fkl)>0){
         for(i in 1:nrow(fkl)){
            ntn <- fkl$to[i]
            toAdd <- do.call(
               readr::read_delim_chunked,
               c(
                  list(file=files[ntn]),
                  rp,
                  list(
                     col_types=ReDaMoR::col_types(dm[[ntn]])
                  ),
                  list(
                     callback=readr::DataFrameCallback$new(function(y, pos){
                        dplyr::semi_join(
                           y, d[[tn]],
                           by=magrittr::set_names(
                              fkl$ff[[i]], fkl$tf[[i]]
                           )
                        )
                     }),
                     chunk_size=10^5
                  )
               )
            )
            d[[ntn]] <<- dplyr::bind_rows(
               d[[ntn]],
               toAdd
            ) %>%
               dplyr::distinct()
         }
      }
   }
   for(tn in names(d)){
      if(!is.null(fk)){
         .contaminate(tn)
      }
   }
   if(!is.null(fk) && nrow(fk) > nrow(nfk)){
      d <- .file_filtByConta(d, fdb, nfk)
   }
   return(d)
}

.format_file_size <- function(n){
   sunits <- c("B", "KB", "MB", "GB", "TB")
   nunits <- log2(n)%/%10
   toRet <- lapply(
      1:length(n),
      function(i){
         format(
            n[i]/(2^(10*nunits[i])),
            digit=1,
            nsmall=ifelse(nunits[i]==0, 0, 1)
         )
      }
   )
   return(paste(toRet, sunits[nunits+1]))
}

