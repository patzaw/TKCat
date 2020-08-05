###############################################################################@
#' Create an MDB (Modeled DataBase) based on files: fileMDB
#' 
#' @param dataFiles a named vector of path to data files with
#' `all(names(dataFiles) %in% names(dataModel))`
#' @param dataModel a [ReDaMoR::RelDataModel] object
#' @param dbInfo a list with DB information:
#' "name", "title", "description", "url",
#' "version", "maintainer".
#' @param readParameters a list of parameters for reading the data file.
#' (e.g. `list(delim='\t', quoted_na=FALSE,)`)
#' @param collectionMembers the members of collections as provided to the
#' [collection_members<-] function (default: NULL ==> no member).
#' @param n_max maximum number of records to read
#' for checks purpose (default: 10). See also [ReDaMoR::confront_data()].
#' @param verbose if TRUE display the data confrontation report
#'
#' @return A [fileMDB] object
#' 
#' @export
#' 
fileMDB <- function(
   dataFiles,
   dbInfo,
   dataModel,
   readParameters=list(),
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
   readParameters <- readParameters[intersect(
      setdiff(names(readParameters), "n_max"),
      names(as.list(args(readr::read_delim)))
   )]
   
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
#' "name", "title", "description", "url" (or "reference URL"),
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
#'
#' @return A [fileMDB] object
#' 
#' @export
#' 
read_fileMDB <- function(
   path,
   dbInfo=NULL,
   dataModel=NULL,
   collectionMembers=NULL,
   n_max=10
){
   stopifnot(file.exists(path))
   
   ## DB information ----
   if(is.null(dbInfo)){
      dbInfo <- jsonlite::read_json(file.path(path, "DESCRIPTION.json"))
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
   readParameters <- allInfo[intersect(
      names(allInfo),
      names(as.list(args(readr::read_delim)))
   )]
   defaultParameters <- list(
      delim='\t',
      quoted_na=FALSE
   )
   for(p in names(defaultParameters)){
      if(!p %in% names(readParameters)){
         readParameters[[p]] <- defaultParameters[[p]]
      }
   }
   
   ## Data model ----
   if(is.null(dataModel)){
      dataModel <- ReDaMoR::read_json_data_model(
         file.path(path, "model", sprintf("%s.json", dbInfo[["name"]]))
      )
   }else{
      if(is.character(dbInfo)){
         stopifnot(
            length(dbInfo)==1,
            !is.na(dbInfo),
            file.exists(dbInfo)
         )
         dataModel <- ReDaMoR::read_json_data_model(dbInfo)
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
   ext <- unique(.getExtensions(list.files(dp)))
   if(length(ext)!=1){
      stop(
         "There should be one and only one extension for all the data files.",
         sprintf("Here the following were found: %s", paste(ext, collapse=", "))
      )
   }
   dataFiles <- file.path(dp, paste0(names(dataModel), ext)) %>% 
      normalizePath()
   names(dataFiles) <- names(dataModel)
   
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
      verbose=TRUE
   ))
}


###############################################################################@
#' Check the object is  a [fileMDB] object
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
#' @export
#' 
rename.fileMDB <- function(.data, ...){
   loc <- tidyselect::eval_rename(expr(c(...)), .data)
   names <- names(.data)
   names[loc] <- names(loc)
   set_names(.data, names)
}


###############################################################################@
#' @param x a [fileMDB] object
#' @param ... not used
#' 
#' @rdname db_info
#' @method db_info fileMDB
#' 
#' @export
#'
db_info.fileMDB <- function(x, ...){
   y <- unclass(x)
   toRet <- y$dbInfo
   return(toRet)
}


###############################################################################@
#' @param x a [fileMDB] object
#' @param value a list with DB information:
#' "name", "title", "description", "url",
#' "version", "maintainer".
#' 
#' @rdname db_info-set
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
#' @param x a [fileMDB] object
#' @param ... not used
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
#' @param x a [fileMDB] object
#' @param ... names of the collections
#' to focus on. By default, all of them are taken.
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
#' @param x a [fileMDB] object
#' 
#' @param value A data.frame with the following columns:
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
#' @rdname collection_members-set
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
         value %>% dplyr::select("collection", "table", "field")
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
#' @param x a [fileMDB] object
#' @param ... the name of the tables to get (default: all of them)
#' 
#' @rdname data_tables
#' @method data_tables fileMDB
#' 
#' @export
#'
data_tables.fileMDB <- function(x, ...){
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
         cr <- do.call(readr::read_delim, c(
            list(
               file=x$dataFiles[y],
               col_types=ReDaMoR::col_types(m[[y]])
            ),
            x$readParameters
         ))
      }
   )
   names(toRet) <- names(toTake)
   return(toRet)
}


###############################################################################@
#' @param x a [fileMDB]
#' @param ... the name of the tables to consider (default: all of them)
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
   m <- data_model(x)
   toTake <- tidyselect::eval_select(expr(c(...)), x)
   if(length(toTake)==0){
      toTake <- 1:length(x)
   }
   x <- unclass(x)
   lapply(x$dataFiles[toTake], count_lines) %>% 
      unlist() %>% 
      `-`(1)
}


###############################################################################@
#' Get the data files from an object
#' 
#' @param x an object with a dataFiles and readParameters slots
#' 
#' @return a list with "dataFiles" and "readParameters" for reading the files.
#' 
#' @export
#'
data_files <- function(x){
   x <- unclass(x)
   toTake <- c("dataFiles", "readParameters")
   stopifnot(all(toTake %in% names(x)))
   return(x[toTake])
}


###############################################################################@
#' @export
#'
'[.fileMDB' <- function(x, i){
   if(missing(i)){
      return(x)
   }
   if(length(i)==0){
      dbi <- db_info(x)
      dbi$name <- sprintf("EMPTY %s", dbi$name)
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
   dbi$name <- sprintf("SUBSET of %s", dbi$name)
   dm <- data_model(x)[i, rmForeignKeys=TRUE]
   df <- data_files(x)
   rp <- df$readParameters
   df <- df$dataFiles[i]
   cm <- collection_members(x) %>%
      dplyr::filter(.data$table %in% i) %>%
      dplyr::mutate(resource=dbi$name)
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
#' @export
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
         return(data_tables(x, i)[[1]])
      }
   }
}
#' @export
'$.fileMDB' <- `[[.fileMDB`


###############################################################################@
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
#' @param x a [fileMDB]
#' @param path the path where the MDB should be written
#' @param ... not used
#' 
#' @rdname write_MDB
#' @method write_MDB fileMDB
#' 
#' @export
#'
write_MDB.fileMDB <- function(x, path, ...){
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
   rp <- data_files(x)$readParameters
   descFile <- file.path(fullPath, "DESCRIPTION.json")
   .writeDescription(c(dbInfo, rp), descFile)
   
   ## Data model ----
   dm <- data_model(x)
   modelPath <- file.path(fullPath, "model")
   dir.create(modelPath)
   jModelPath <- file.path(modelPath, paste0(dbName, ".json"))
   hModelPath <- file.path(modelPath, paste0(dbName, ".html"))
   write_json_data_model(dm, jModelPath)
   plot(dm) %>%
      visNetwork::visSave(hModelPath)
   
   ## Collection members ----
   cm <- collection_members(x)
   if(nrow(cm)>0){
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
   ext <- .getExtensions(ofiles)
   dfiles <- file.path(dataPath, paste0(names(ext), ext))
   names(dfiles) <- names(ofiles)
   file.copy(ofiles, dfiles)
   
   ## Return fileMDB ----
   return(fileMDB(
      dataFiles=dfiles,
      dbInfo=dbInfo,
      dataModel=dm,
      readParameters=rp,
      collectionMembers=cm
   ))
}


###############################################################################@
#'
filter.fileMDB <- function(x){}

###############################################################################@
#'
slice.fileMDB <- function(x){}


###############################################################################@
###############################################################################@
## Needs metaMDB ?? ----

###############################################################################@
#'
inner_join.fileMDB <- function(x){}

###############################################################################@
#'
left_join.fileMDB <- function(x){}

###############################################################################@
#'
right_join.fileMDB <- function(x){}

###############################################################################@
#'
full_join.fileMDB <- function(x){}


###############################################################################@
## Helpers ----
.getExtensions <- function(files){
   ext <- regexpr(
      "(\\.[[:alnum:]]+)(\\.gz)?$", files, ignore.case=TRUE
   )
   ext <- substr(
      files, ext, ext+attr(ext, "match.length")-1
   )
   return(ext)
}


