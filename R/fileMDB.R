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
#' @param check logical: if TRUE (default) the data are confronted to the
#' data model
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
#' [count_records], [dims], [filter_with_tables], [as_fileMDB]
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
   check=TRUE,
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
   if(check){
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
            cat(ReDaMoR::format_confrontation_report(
               cr, title=dbInfo[["name"]]
            ))
            stop("Data do not fit the data model")
         }
         if(verbose){
            cat(ReDaMoR::format_confrontation_report(
               cr, title=dbInfo[["name"]]
            ))
         }
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
#' as missing values or as strings or strings.
#' WARNING: THIS PARAMETER IS NOT TAKEN INTO ACCOUNT WITH readr>=2.0.0.
#' - **na**: String used for missing values. The default value for reading
#' a fileMDB is "NA". But the default value for writing a fileMDB is
#' "&lt;NA&gt;".
#' This value is written in the DESCRIPTION.json file to avoid ambiguity
#' when reading the fileMDB.
#' @param dataModel a [ReDaMoR::RelDataModel] object or json file.
#' If NULL (default), the model json file found in path/model.
#' @param collectionMembers the members of collections as provided to the
#' [collection_members<-] function. If NULL (default), the members
#' are taken from json files found in path/model/Collections
#' @param check logical: if TRUE (default) the data are confronted to the
#' data model
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
   check=TRUE,
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
         f <- list.files(
            dp,
            pattern=paste0("^", x, "(\\.[[:alnum:]]+)(\\.gz)?$")
         )
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
      check=check,
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
   stopifnot(
      length(x)==length(value),
      sum(duplicated(value))==0
   )
   if(length(x)==0){
      return(x)
   }
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
   loc <- tidyselect::eval_rename(rlang::expr(c(...)), .data)
   names <- names(.data)
   names[loc] <- names(loc)
   magrittr::set_names(.data, names)
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
   if(length(x)==0){
      return(list())
   }
   m <- data_model(x)
   toTake <- tidyselect::eval_select(rlang::expr(c(...)), x)
   if(length(toTake)==0){
      toTake <- 1:length(x)
      names(toTake) <- names(x)
   }
   x <- unclass(x)
   toRet <- lapply(
      toTake,
      function(y){
         toRet <- do.call(.read_td, c(
            list(
               tm=x$dataModel[[y]],
               f=x$dataFiles[y],
               skip=skip, n_max=n_max
            ),
            x$readParameters
         ))
         return(toRet)
      }
   )
   names(toRet) <- names(toTake)
   return(toRet)
}


###############################################################################@
#' 
#' @rdname heads
#' @method heads fileMDB
#' 
#' @export
#'
heads.fileMDB <- function(x, ..., n=6L){
   
   stopifnot(
      is.numeric(n), length(n)==1, n>0
   )
   if(length(x)==0){
      return(list())
   }
   m <- data_model(x)
   toTake <- tidyselect::eval_select(rlang::expr(c(...)), x)
   if(length(toTake)==0){
      toTake <- 1:length(x)
      names(toTake) <- names(x)
   }
   
   toRet <- lapply(
      names(toTake),
      function(name){
         
         ismm <- ReDaMoR::is_MM(data_files(x)$dataFiles[name])
         if(ReDaMoR::is.MatrixModel(m[[name]]) && !ismm){
            if(is.infinite(n)){
               return(data_tables(x, dplyr::all_of(name))[[1]])
            }
            ncol <- data_tables(x, dplyr::all_of(name), n_max=1)[[1]] %>% ncol()
            mn <- min(floor(sqrt(n)), ncol)
            for(nc in mn:floor(mn/2)){
               if(n %% nc == 0){
                  break()
               }
            }
            if(n %% nc == 0){
               nr <- n %/% nc
            }else{
               nr <- nc <- mn
            }
            if(nc * nr != n){
               warning(sprintf("Returning %s records", nc * nr))
            }
            d <- data_tables(x, dplyr::all_of(name), n_max=nr)[[1]]
            return(d[1:min(nr, nrow(d)), 1:nc])
         }else{
            return(data_tables(x, dplyr::all_of(name), skip=0, n_max=n)[[1]])
         }
         
      }
   )
   names(toRet) <- names(toTake)
   
   return(toRet)
   
}


###############################################################################@
#' 
#' @param by the size of the batch: number of lines to count
#' together (default: 1000)
#' @param estimateThr file size threshold in bytes from which an estimation
#' of row number should be computed instead of a precise count
#' (default: 50000000 = 50MB)
#' @param estimateSample number of values on which the estimation is based
#' (default: 10^6)
#' @param showWarnings a warning is raised by default if estimation is done.
#' 
#' @rdname dims
#' @method dims fileMDB
#' 
#' @export
#'
dims.fileMDB <- function(
   x, ...,
   by=1000,
   estimateThr=50000000, estimateSample=10^6,
   showWarnings=TRUE
){
   if(length(x)==0){
      return(dplyr::tibble(
         name=character(),
         format=character(),
         ncol=numeric(),
         nrow=numeric(),
         records=numeric(),
         transposed=logical()
      ))
   }
   count_rc <- function(tn, by=1000){
      f <- data_files(x)$dataFiles[[tn]]
      
      ismm <- ReDaMoR::is_MM(f)
      if(ismm){
         
         td <- ReDaMoR::read_named_MM_header(f)
         nr <- td$rows
         nc <- td$columns
         
      }else{
         
         nc <- ncol(data_tables(x, dplyr::all_of(tn), n_max=1)[[1]])
         con <- file(f, "r")
         on.exit(close(con))
         d <- c()
         nr <- cn <- length(readLines(con, n=by))
         while(cn>0){
            cn <- length(readLines(con, n=by))
            nr <- nr+cn
         }
         nr <- nr - 1
         
      }
      
      return(c(nr, nc))
      
   }
   estimate_rc <- function(tn, esamp=10^6){
      f <- data_files(x)$dataFiles[[tn]]
      rp <- data_files(x)$readParameters
      fs <- file.size(f)
      fcon <- file(f)
      fc <- summary(fcon)$class=="gzfile"
      close(fcon)
      nc <- ncol(data_tables(x, dplyr::all_of(tn), n_max=1)[[1]])
      rsamp <- round(esamp/nc)
      st <- data_tables(x, dplyr::all_of(tn), n_max=rsamp)[[1]]
      if(is.matrix(st)){
         st <- dplyr::as_tibble(st, rownames="___ROWNAMES___")
      }
      tmpf <- tempfile(fileext=ifelse(fc, ".txt.gz", ".txt"))
      on.exit(file.remove(tmpf))
      if(fc){
         tmpc <- gzfile(tmpf)
      }else{
         tmpc <- file(tmpf)
      }
      utils::write.table(st, file=tmpc, sep=rp$delim)
      sfs <- file.size(tmpf)
      nr <- round(fs * (rsamp + 1) / sfs)
      nr <- nr - 1
      return(c(nr, nc))
   }
   toTake <- tidyselect::eval_select(rlang::expr(c(...)), x)
   if(length(toTake)==0){
      toTake <- 1:length(x)
      names(toTake) <- names(x)
   }
   toTake <- names(toTake)
   do.call(dplyr::bind_rows, lapply(
      toTake, function(tn){
         f <- data_files(x)$dataFiles[[tn]]
         fs <- file.size(f)
         
         ismm <- ReDaMoR::is_MM(f)
         if(fs > estimateThr && !ismm){
            if(showWarnings){
               warning(sprintf(
                  "Estimating %s rows based on the first %s values",
                  tn,
                  estimateSample
               ))
            }
            n <- estimate_rc(tn, estimateSample)
         }else{
            n <- count_rc(tn, by)
         }
         dplyr::tibble(
            name=tn,
            format=ifelse(
               ReDaMoR::is.MatrixModel(data_model(x)[[tn]]),
               ifelse(ismm, "MatrixMarket", "matrix"),
               "table"
            ),
            ncol=n[2],
            nrow=n[1],
         ) %>% 
            dplyr::mutate(
               records=ifelse(
                  .data$format!="table",
                  as.numeric(.data$ncol) * as.numeric(.data$nrow),
                  .data$nrow
               ),
               transposed=FALSE
            )
      }
   ))
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
   fs <- file.size(df)
   if(hr){
      fs <- .format_file_size(fs)
   }
   fc <- lapply(
      df, function(x){
         fcon <- file(x)
         toRet <- summary(fcon)$class
         close(fcon)
         return(toRet)
      }
   ) %>% unlist()
   toRet <- dplyr::tibble(table=names(df), size=fs, compressed=fc=="gzfile")
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
         dataModel=ReDaMoR::RelDataModel(l=list()),
         readParameters=data_files(x)$readParameters,
         check=FALSE
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
      collectionMembers=cm,
      check=FALSE
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
#' @rdname as_fileMDB
#' @method as_fileMDB fileMDB
#' 
#' @param by the size of the batch: number of records to write
#' together (default: 10^5)
#' 
#' @export
#'
as_fileMDB.fileMDB <- function(
   x, path,
   readParameters=list(delim="\t", na="<NA>"),
   htmlModel=TRUE,
   compress=TRUE,
   by=10^5,
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
      any(unlist(lapply(
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
   ReDaMoR::write_json_data_model(dm, jModelPath)
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
         if(ReDaMoR::is_MM(ofiles[tn])){
            file.copy(ofiles[tn], dfiles[tn])
         }else{
            do.call(
               .read_td_chunked,
               c(
                  list(
                     tm=data_model(x)[[tn]],
                     f=ofiles[tn]
                  ),
                  rp,
                  list(
                     callback=readr::DataFrameCallback$new(function(y, pos){
                        readr::write_delim(
                           y, file=dfiles[tn], delim=readParameters$delim,
                           na=readParameters$na,
                           quote="all", escape="double",
                           append=file.exists(dfiles[tn])
                        )
                     }),
                     chunk_size=by
                  )
               )
            )
         }
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
   dots <- rlang::enquos(...)
   files <- data_files(x)
   rp <- files$readParameters
   files <- files$dataFiles
   for(tn in names(dots)){
      if(!tn %in% names(x)){
         stop(sprintf("%s table does not exist", tn))
      }
      if(ReDaMoR::is.MatrixModel(data_model(x)[[tn]])){
         stop("Cannot filter a matrix: start from another table")         
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
   if(ReDaMoR::is.MatrixModel(data_model(x)[[tn]])){
      stop("Cannot slice a matrix: start from another table")         
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
#' @param by the size of the batch: number of lines to process
#' together (default: 10000)
#' 
#' @rdname filter_with_tables
#' @method filter_with_tables fileMDB
#' 
#' @export
#'
filter_with_tables.fileMDB <- function(
   x, tables, checkTables=TRUE, by=10^5, ...
){
   
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
   tables <- .file_filtByConta(tables, x, fk, dm, by=by)
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
      collectionMembers=cm,
      check=FALSE
   ))
   
}


###############################################################################@
#' 
#' @param .by the size of the batch: number of lines to process
#' together (default: 10000)
#' 
#' @rdname filter_mdb_matrix
#' @method filter_mdb_matrix fileMDB
#' 
#' @export
#'
filter_mdb_matrix.fileMDB <- function(x, tableName, .by=10^5, ...){
   
   ## Checks ----
   stopifnot(
      is.fileMDB(x),
      tableName %in% names(x)
   )
   tableModel <- data_model(x)[[tableName]]
   stopifnot(ReDaMoR::is.MatrixModel(tableModel))
   iFilter <- list(...)
   stopifnot(
      length(names(iFilter)) > 0, length(iFilter) <= 2, 
      !any(duplicated(names(iFilter))),
      all(names(iFilter) %in% tableModel$fields$name)
   )
   vfield <- tableModel$fields %>%
      dplyr::filter(!.data$type %in% c("row", "column")) %>% 
      dplyr::pull("name") %>% 
      intersect(names(iFilter))
   if(length(vfield)>0){
      stop("Cannot filter a matrix on values; only on row or column names")
   }
   
   ## Select fields ----
   frc <- c()
   for(f in names(iFilter)){
      ft <- tableModel$fields %>%
         dplyr::filter(.data$name==!!f) %>%
         dplyr::pull("type")
      if(ft=="row"){
         fr <- iFilter[[f]]
         frc <- c(frc, "r")
      }else{
         fc <- iFilter[[f]]
         frc <- c(frc, "c")
      }
   }
   frc <- paste(sort(frc), collapse="")
   
   ## Get the results ----
   rp <- data_files(x)$readParameters
   if(frc==""){
      stop("Dev. error: review this part of the function")
   }
   
   dfile <- data_files(x)$dataFiles[tableName]
   if(ReDaMoR::is_MM(dfile)){
      
      fheader <- ReDaMoR::read_named_MM_header(dfile)
      fv <- list()
      if(frc=="cr"){
         fv$rows=which(fheader$rownames %in% fr)
         fv$columns=which(fheader$colnames %in% fc)
      }
      if(frc=="r"){
         fv$rows=which(fheader$rownames %in% fr)
         fv$columns=1:length(fheader$colnames)
      }
      if(frc=="c"){
         fv$rows=1:length(fheader$rownames)
         fv$columns=which(fheader$colnames %in% fc)
      }
      if(length(fv$rows)==0  || length(fv$columns)==0){
         toRet <- matrix(
            nrow=length(fv$rows),
            ncol=length(fv$columns),
            dimnames=list(
               fheader$rownames[fv$rows],
               fheader$colnames[fv$columns]
            )
         )
      }else{
         ovcs <- Sys.getenv("VROOM_CONNECTION_SIZE")
         on.exit(Sys.setenv("VROOM_CONNECTION_SIZE"=ovcs))
         Sys.setenv(
            "VROOM_CONNECTION_SIZE"=max(
               c(
                  object.size(fheader$rownames),
                  object.size(fheader$colnames),
                  as.numeric(ovcs)
               ),
               na.rm=TRUE
            )
         )
         toRet <- readr::read_delim_chunked(
            dfile,
            delim="\t",
            col_names=c("i", "j", "x"),
            col_types="iin",
            skip=fheader$header_length,
            chunk_size=.by,
            callback=readr::DataFrameCallback$new(
               function(y, pos){
                  dplyr::filter(
                     y,
                     .data$i %in% fv$rows,
                     .data$j %in% fv$columns
                  )
               }
            )
         )
         itoadd <- setdiff(fv$rows, toRet$i)
         toRet <- dplyr::bind_rows(
            toRet,
            dplyr::tibble(i=itoadd, j=rep(1, length(itoadd)), x=rep(0, length(itoadd)))
         )
         jtoadd <- setdiff(fv$columns, toRet$j)
         toRet <- dplyr::bind_rows(
            toRet,
            dplyr::tibble(i=rep(1, length(jtoadd)), j=jtoadd, x=rep(0, length(jtoadd)))
         )
         toRet <- Matrix::sparseMatrix(
            i=toRet$i, j=toRet$j, x=toRet$x,
            dimnames=list(
               fheader$rownames[1:max(fv$rows)],
               fheader$colnames[1:max(fv$columns)]
            )
         )[
            fheader$rownames[fv$rows],
            fheader$colnames[fv$columns],
            drop=FALSE
         ]
      }
      
      toRet <- Matrix::drop0(toRet)
      
   }else{
   
      toRet <- do.call(
         .read_td_chunked,
         c(
            list(
               tm=tableModel,
               f=dfile
            ),
            rp,
            list(
               callback=readr::DataFrameCallback$new(function(y, pos){
                  if(frc=="r"){
                     toRet <- y %>%
                        dplyr::filter(.data$`___ROWNAMES___` %in% fr) %>% 
                        as.data.frame()
                  }
                  if(frc=="c"){
                     toRet <-  y %>%
                        dplyr::select(
                           dplyr::all_of(intersect(
                              c("___ROWNAMES___", fc),
                              colnames(y)
                           ))
                        ) %>% 
                        as.data.frame()
                  }
                  if(frc=="cr"){
                     toRet <-  y %>%
                        dplyr::select(
                           dplyr::all_of(intersect(
                              c("___ROWNAMES___", fc),
                              colnames(y)
                           ))
                        ) %>% 
                        dplyr::filter(.data$`___ROWNAMES___` %in% fr) %>% 
                        as.data.frame()
                  }
                  rownames(toRet) <- toRet$"___ROWNAMES___"
                  toRet <- as.matrix(
                     toRet[
                        , setdiff(colnames(toRet), "___ROWNAMES___"), drop=FALSE
                     ]
                  )
                  return(toRet)
               }),
               chunk_size=.by
            )
         )
      )
   
   }
   
   if(frc=="r"){
      toRet <- toRet[intersect(fr, rownames(toRet)),, drop=FALSE]
   }
   if(frc=="c"){
      toRet <- toRet[,intersect(fc, colnames(toRet)), drop=FALSE]
   }
   if(frc=="cr"){
      toRet <- toRet[
         intersect(fr, rownames(toRet)),
         intersect(fc, colnames(toRet)),
         drop=FALSE
      ]
   }
   return(toRet)
}


###############################################################################@
## READ PARAMETERS -----
DEFAULT_READ_PARAMS <- list(delim='\t', na="NA")

.check_read_params <- function(readParameters){
   readParameters <- readParameters[intersect(
      names(readParameters), c(names(DEFAULT_READ_PARAMS), "quoted_na")
   )]
   if("delim" %in% names(readParameters)){
      stopifnot(
         length(readParameters$delim)==1,
         is.character(readParameters$delim),
         !is.na(readParameters$delim)
      )
   }
   if("quoted_na" %in% names(readParameters)){
      warning(
         "The `quoted_na` argument of `read_delim()` is deprecated",
         " as of readr 2.0.0."
      )
      stopifnot(
         length(readParameters$quoted_na)==1,
         is.logical(readParameters$quoted_na),
         !is.na(readParameters$quoted_na)
      )
   }
   if("na" %in% names(readParameters)){
      stopifnot(
         length(readParameters$na)==1,
         is.character(readParameters$na),
         !is.na(readParameters$na)
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
.write_chTables.fileMDB <- function(x, con, dbName, by=10^5, ...){
   dm <- data_model(x)
   df <- data_files(x)
   rp <- df$readParameters
   df <- df$dataFiles
   for(tn in names(x)){
      
      if(ReDaMoR::is.MatrixModel(dm[[tn]])){
         
         if(ReDaMoR::is_MM(df[tn])){
            
            ## Sparse matrix ----
            
            ## Rownames and colnames
            fheader <- ReDaMoR::read_named_MM_header(df[tn])
            rowTable <- dplyr::tibble(
               i=1:fheader$rows,
               name=fheader$rownames
            )
            colTable <- dplyr::tibble(
               j=1:fheader$columns,
               name=fheader$colnames
            )
            modTable <- dplyr::tibble(
               table=uuid::UUIDgenerate(n=3),
               info=c("rows", "columns", "values")
            )
            write_MergeTree(
               con=con,
               dbName=dbName,
               tableName=modTable$table[which(modTable$info=="rows")],
               value=rowTable,
               rtypes=c("i"="integer", name="character"),
               nullable=NULL,
               sortKey="i"
            )
            write_MergeTree(
               con=con,
               dbName=dbName,
               tableName=modTable$table[which(modTable$info=="columns")],
               value=colTable,
               rtypes=c("j"="integer", name="character"),
               nullable=NULL,
               sortKey="j"
            )
            
            ## Values
            valTable <- modTable$table[which(modTable$info=="values")]
            write_MergeTree(
               con=con,
               dbName=dbName,
               tableName=valTable,
               value=dplyr::tibble(
                  i=integer(),
                  j=integer(),
                  x=numeric()
               ),
               rtypes=c("i"="integer", "j"="integer", "x"="numeric"),
               nullable=NULL,
               sortKey=c("i", "j")
            )
            readr::read_delim_chunked(
               df[tn],
               delim="\t",
               col_names=c("i", "j", "x"),
               col_types="iin",
               skip=fheader$header_length,
               chunk_size=by,
               callback=readr::DataFrameCallback$new(
                  function(y, pos){
                     ch_insert(
                        con=con,
                        dbName=dbName,
                        tableName=valTable,
                        value=y
                     )
                  }
               )
            )
            
            ## Reference table
            ch_insert(
               con=con, dbName=dbName, tableName=tn,
               value=modTable
            )
         
            
         }else{
            
            ## Matrix ----   
            
            nullable <- dm[[tn]]$fields %>% 
               dplyr::filter(!.data$type %in% c("column", "row")) %>% 
               dplyr::pull("nullable")
            vtype <- setdiff(dm[[tn]]$fields$type, c("column", "row"))
            ddim <- dims(x, dplyr::all_of(tn), showWarnings=FALSE)
            
            if(ddim$ncol > CH_MAX_COL && ddim$nrow < ddim$ncol){
               
               transposed <- TRUE
               tlist <- do.call(
                  .read_td_chunked,
                  c(
                     list(tm=dm[[tn]], f=df[tn]),
                     rp,
                     list(
                        callback=readr::DataFrameCallback$new(function(y, pos){
                           rn <- y$"___ROWNAMES___"
                           y <- t(y[, -1]) %>%
                              magrittr::set_colnames(rn) %>% 
                              dplyr::as_tibble(rownames="___COLNAMES___")
                           tname <- uuid::UUIDgenerate(n=1)
                           nulcol <- NULL
                           if(nullable){
                              nulcol <- setdiff(colnames(y), "___COLNAMES___")
                           }
                           write_MergeTree(
                              con=con,
                              dbName=dbName,
                              tableName=tname,
                              value=y,
                              rtypes=c("character", rep(vtype, ncol(y) - 1)) %>% 
                                 magrittr::set_names(colnames(y)),
                              nullable=nulcol,
                              sortKey=colnames(y)[1]
                           )
                           return(tname)
                        }),
                        chunk_size=CH_MAX_COL
                     )
                  )
               ) %>% as.character()
               ch_insert(
                  con=con, dbName=dbName, tableName=tn,
                  value=dplyr::tibble(table=tlist, info="values")
               )
               
            }else{
               
               transposed <- FALSE
               cnames <- data_tables(x, dplyr::all_of(tn), n_max=1)[[1]] %>%
                  colnames() %>% 
                  sort()
               colList <- seq(1, ddim$ncol, by=CH_MAX_COL)
               colList <- lapply(
                  colList,
                  function(i){
                     cnames[i:min(i+CH_MAX_COL-1, ddim$ncol)]
                  }
               )
               names(colList) <- uuid::UUIDgenerate(n=length(colList))
               
               lapply(names(colList), function(tname){
                  fields <- colList[[tname]]
                  tval <- dplyr::tibble(
                     "___ROWNAMES___"=character()
                  )
                  for(field in fields){
                     toAdd <- integer()
                     class(toAdd) <- vtype
                     toAdd <- dplyr::tibble(toAdd) %>% 
                        magrittr::set_colnames(field)
                     tval <- dplyr::bind_cols(tval, toAdd)
                  }
                  nulcol <- NULL
                  if(nullable){
                     nulcol <- fields
                  }
                  write_MergeTree(
                     con=con,
                     dbName=dbName,
                     tableName=tname,
                     value=tval,
                     rtypes=c("character", rep(vtype, length(fields))) %>% 
                        magrittr::set_names(colnames(tval)),
                     nullable=nulcol,
                     sortKey=colnames(tval)[1]
                  )
               })
               
               do.call(
                  .read_td_chunked,
                  c(
                     list(tm=dm[[tn]], f=df[tn]),
                     rp,
                     list(
                        callback=readr::DataFrameCallback$new(function(y, pos){
                           lapply(names(colList), function(tname){
                              fields <- colList[[tname]]
                              tval <- y[,c("___ROWNAMES___", fields)]
                              ch_insert(
                                 con=con,
                                 dbName=dbName,
                                 tableName=tname,
                                 value=tval
                              )
                           })
                        }),
                        chunk_size=by
                     )
                  )
               )
               ch_insert(
                  con=con, dbName=dbName, tableName=tn,
                  value=dplyr::tibble(table=names(colList), info="values")
               )
               
            }
         }
         
      }else{{
         
         ## Table ----
            
         
         do.call(
            .read_td_chunked,
            c(
               list(tm=dm[[tn]], f=df[tn]),
               rp,
               list(
                  callback=readr::DataFrameCallback$new(function(y, pos){
                     toWrite <- y
                     b64_fields <- dm[[tn]]$fields %>% 
                        dplyr::filter(.data$type=="base64") %>% 
                        dplyr::pull("name")
                     for(b64f in b64_fields){
                        toWrite[[b64f]] <- lapply(
                           toWrite[[b64f]], function(v){
                              if(is.na(v)){
                                 return(character())
                              }
                              sl <- c(
                                 seq(1, nchar(v), by=CH_DOC_CHUNK),
                                 nchar(v) + 1
                              )
                              return(substring(v, sl[-length(sl)], sl[-1]-1))
                           }
                        )
                     }
                     ch_insert(
                        con=con, dbName=dbName, tableName=tn, value=toWrite
                     )
                  }),
                  chunk_size=by
               )
            )
         )
      }}
   }
}


.file_filtByConta <- function(d, fdb, fk, dm, by=10^5){
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
         dplyr::anti_join(
            dplyr::select(fkf, "from", "to"), by=c("from", "to")
         ) %>% 
         dplyr::anti_join(dplyr::select(fkt, "from", "to"), by=c("from", "to"))
      fkl <- dplyr::bind_rows(
         fkf,
         fkt %>% dplyr::rename("from"="to", "ff"="tf", "to"="from", "tf"="ff")
      ) %>% 
         dplyr::distinct()
      if(nrow(fkl)>0){
         for(i in 1:nrow(fkl)){
            ntn <- fkl$to[i]
            if(ntn %in% names(d)){
               nv <- .mdjoin(
                  d1=d[[ntn]], d2=d[[tn]],
                  by=magrittr::set_names(
                     fkl$ff[[i]], fkl$tf[[i]]
                  ),
                  tm1=dm[[ntn]], tm2=dm[[tn]]
               )
            }else{
               if(ReDaMoR::is_MM(files[ntn])){
                  
                  fv <- lapply(1:length(fkl$tf[[i]]), function(j){
                     ntm <- dm[[ntn]]
                     ntf <- fkl$tf[[i]][[j]]
                     tm <- dm[[tn]]
                     tf <- fkl$ff[[i]][[j]]
                     if(inherits(d[[tn]], c("matrix", "Matrix"))){
                        if(
                           tm$fields$type[which(tm$fields$name==tf)] ==
                           "row"
                        ){
                           tv <- rownames(d[[tn]])
                        }else{
                           if(
                              tm$fields$type[which(tm$fields$name==ntf)] ==
                              "column"
                           ){
                              tv <- colnames(d[[tn]])
                           }else{
                              stop(sprintf(
                                 paste(
                                    "The field type of %s in the %s MM",
                                    "should be row or column"
                                 ),
                                 tf, tn
                              ))
                           }
                        }
                     }else{
                        tv <- d[[tn]][[tf]]
                     }
                     toRet <- list(tv)
                     names(toRet) <- ntf
                     return(toRet)
                  })
                  fv <- do.call(c, fv)
                  nv <- do.call(
                     filter_mdb_matrix,
                     c(
                        list(x=fdb, tableName=ntn),
                        fv
                     )
                  )
                  
               }else{
                  nv <- do.call(
                     .read_td_chunked,
                     c(
                        list(tm=dm[[ntn]], f=files[ntn]),
                        rp,
                        list(
                           callback=readr::DataFrameCallback$new(
                              function(y, pos){
                                 if(ReDaMoR::is.MatrixModel(dm[[ntn]])){
                                    y <- as.data.frame(
                                       y, stringsAsFactors=FALSE
                                    )
                                    rownames(y) <- y[[1]]
                                    y <- as.matrix(y[, -1, drop=FALSE])
                                 }
                                 .mdjoin(
                                    d1=y, d2=d[[tn]],
                                    by=magrittr::set_names(
                                       fkl$ff[[i]], fkl$tf[[i]]
                                    ),
                                    tm1=dm[[ntn]], tm2=dm[[tn]]
                                 )
                              }
                           ),
                           chunk_size=by
                        )
                     )
                  )
               }
            }
            d[[ntn]] <<- nv
         }
      }

      ## Forward ----
      fkf <- fk %>% dplyr::filter(.data$from==!!tn & .data$fmin==0)
      fkt <- fk %>% dplyr::filter(.data$to==!!tn & .data$tmin==0)
      nfk <<- nfk %>%
         dplyr::anti_join(
            dplyr::select(fkf, "from", "to"), by=c("from", "to")
         ) %>% 
         dplyr::anti_join(dplyr::select(fkt, "from", "to"), by=c("from", "to"))
      fkl <- dplyr::bind_rows(
         fkf,
         fkt %>% dplyr::rename("from"="to", "ff"="tf", "to"="from", "tf"="ff")
      ) %>% 
         dplyr::distinct()
      if(nrow(fkl)>0){
         for(i in 1:nrow(fkl)){
            ntn <- fkl$to[i]
            
            if(ReDaMoR::is.MatrixModel(dm[[ntn]])){
               
               fv <- lapply(1:length(fkl$tf[[i]]), function(j){
                  ntm <- dm[[ntn]]
                  ntf <- fkl$tf[[i]][[j]]
                  tm <- dm[[tn]]
                  tf <- fkl$ff[[i]][[j]]
                  if(inherits(d[[tn]], c("matrix", "Matrix"))){
                     if(
                        tm$fields$type[which(tm$fields$name==tf)] ==
                        "row"
                     ){
                        tv <- rownames(d[[tn]])
                     }else{
                        if(
                           tm$fields$type[which(tm$fields$name==ntf)] ==
                           "column"
                        ){
                           tv <- colnames(d[[tn]])
                        }else{
                           stop(sprintf(
                              paste(
                                 "The field type of %s in the %s MM",
                                 "should be row or column"
                              ),
                              tf, tn
                           ))
                        }
                     }
                  }else{
                     tv <- d[[tn]][[tf]]
                  }
                  if(
                     ntm$fields$type[which(ntm$fields$name==ntf)] ==
                     "row"
                  ){
                     tv <- union(tv, rownames(d[[ntn]]))
                  }else{
                     tv <- union(tv, colnames(d[[ntn]]))
                  }
                  toRet <- list(tv)
                  names(toRet) <- ntf
                  return(toRet)
               })
               fv <- do.call(c, fv)
               
               d[[ntn]] <<- do.call(
                  filter_mdb_matrix,
                  c(
                     list(x=fdb, tableName=ntn),
                     fv
                  )
               )
               
            }else{
               toAdd <- do.call(
                  .read_td_chunked,
                  c(
                     list(tm=dm[[ntn]], f=files[ntn]),
                     rp,
                     list(
                        callback=readr::DataFrameCallback$new(function(y, pos){
                           if(ReDaMoR::is.MatrixModel(dm[[ntn]])){
                              y <- as.data.frame(y, stringsAsFactors=FALSE)
                              rownames(y) <- y[[1]]
                              y <- as.matrix(y[, -1, drop=FALSE])
                           }
                           .mdjoin(
                              d1=y, d2=d[[tn]],
                              by=magrittr::set_names(
                                 fkl$ff[[i]], fkl$tf[[i]]
                              ),
                              tm1=dm[[ntn]], tm2=dm[[tn]]
                           )
                        }),
                        chunk_size=by
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
   }
   for(tn in names(d)){
      if(!is.null(fk)){
         .contaminate(tn)
      }
   }
   if(!is.null(fk) && nrow(fk) > nrow(nfk)){
      d <- .file_filtByConta(d, fdb, nfk, dm)
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

.read_td <- function(
   tm,      # table model
   f,       # file name
   delim,
   skip=0,
   n_max=Inf,
   ...      # additional parameters for read_delim()
){
   
   if(ReDaMoR::is.MatrixModel(tm)){
      
      ismm <- ReDaMoR::is_MM(f)
      if(ismm){
         
         if(skip > 0 || !is.infinite(n_max)){
            warning(
               "Be careful when using skip and n_max parameters: ",
               "Subsetting a sparse matrix from  a file may return ",
               "inconsistent 0 values"
            )
         }
         
         td <- ReDaMoR::read_named_MM(
            f,
            skip=skip,
            n_max=n_max
         )
         
      }else{
      
         r1 <- attr(ismm, "r1")
         cn <- r1 %>%
            strsplit(split=delim) %>%
            unlist()
         cn <- gsub("['`]", "", cn)
         cn <- gsub('["]', "", cn)
   
         vt <- tm$fields %>%
            dplyr::filter(!.data$type %in% c("row", "column")) %>%
            dplyr::pull("type")
         ctypes <- do.call(
            readr::cols,
            structure(
               list(
                  readr::col_character(),
                  .default = switch(
                     vt,
                     "integer"=readr::col_integer(),
                     "numeric"=readr::col_double(),
                     "logical"=readr::col_logical(),
                     "character"=readr::col_character(),
                     "Date"=readr::col_date(),
                     "POSIXct"=readr::col_datetime(),
                     "base64"=readr::col_character()
                  )
               ),
               .Names=c("___ROWNAMES___", ".default")
            )
         )
         td <- readr::read_delim(
            f,
            delim=delim, n_max=n_max, skip=skip+1,
            col_types=ctypes,
            col_names=c("___ROWNAMES___", cn[-1]),
            ...
         ) %>%
            as.data.frame(stringsAsFactors=FALSE)
         stopifnot(
            !any(duplicated(colnames(td))),
            !any(duplicated(td[[1]]))
         )
         rownames(td) <- td[[1]]
         td <- as.matrix(td[, -1, drop=FALSE])
      }
         
   }else{
      
      td <- readr::read_delim(
         f,
         delim=delim, skip=skip, n_max=n_max,
         col_types=ReDaMoR::col_types(tm),
         ...
      )
      
   }
   
   return(td)
   
}


.read_td_chunked <- function(
   tm,      # table model
   f,       # file name
   delim,
   skip=0,
   ...      # additional parameters for read_delim_chunked()
){
   
   if(ReDaMoR::is.MatrixModel(tm)){
      cn <- readLines(f, n=1) %>%
         strsplit(split=delim) %>%
         unlist()
      cn <- gsub("['`]", "", cn)
      cn <- gsub('["]', "", cn)
      
      vt <- tm$fields %>%
         dplyr::filter(!.data$type %in% c("row", "column")) %>%
         dplyr::pull("type")
      ctypes <- do.call(
         readr::cols,
         structure(
            list(
               readr::col_character(),
               .default = switch(
                  vt,
                  "integer"=readr::col_integer(),
                  "numeric"=readr::col_double(),
                  "logical"=readr::col_logical(),
                  "character"=readr::col_character(),
                  "Date"=readr::col_date(),
                  "POSIXct"=readr::col_datetime()
               )
            ),
            .Names=c("___ROWNAMES___", ".default")
         )
      )
      td <- readr::read_delim_chunked(
         f,
         delim=delim, skip=skip+1,
         col_types=ctypes,
         col_names=c("___ROWNAMES___", cn[-1]),
         ...
      )
      
   }else{
      
      td <- readr::read_delim_chunked(
         f,
         delim=delim, skip=skip,
         col_types=ReDaMoR::col_types(tm),
         ...
      )
      
   }
   
   return(td)
   
}
