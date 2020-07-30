###############################################################################@
#' Create an MDB (Modeled DataBase) based on files
#' 
#' @param path the path to a folder with data or with the following structure:
#' - **data**: a folder with the data
#' - **DESCRIPTION.json**: a file with db information
#' - **model**: a folder with the data model json file with the same name as the
#' one given in the DESCRIPTION.json file
#' @param dbInfo a list or a json file with DB information:
#' "name", "title", "description", "url" (or "reference URL"),
#' "version", "maintainer". If NULL (default), the DESCRIPTION.json file found
#' in path.
#' @param dataModel a [RelDataModel] object or json file.
#' If NULL (default), the model json file found in path/model.
#' @param collectionMembers the members of collections as provided to the
#' [collection_members<-.fileMDB] function.
#' @param verbose if TRUE display the data confrontation report
#' @param n_max maximum number of records to read
#' for checks purpose (default: 10). See also [ReDaMoR::confront_data()].
#' @param ext the file extension to consider (default: "ext"),
#' @param delim delimiter (default: '\\\\t')
#' @param quoted_na Should missing values inside quotes be treated
#' as missing values or as strings or strings (the default).
#' Be aware that the default value here is different than the one for the
#' original [readr::read_delim()] function.
#' @param ... additional parameters for the [ReDaMoR::confront_data()] function
#'
#' @return A [fileMDB] object
#' 
#' @export
#' 
fileMDB <- function(
   path,
   dbInfo=NULL,
   dataModel=NULL,
   collectionMembers=NULL,
   verbose=FALSE,
   n_max=10,
   ext='txt',
   delim='\t',
   quoted_na=FALSE,
   ...
){
   stopifnot(file.exists(path))
   fparam <- c(as.list(environment()), list(...))
   
   ## dbInfo ----
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
   mandFields <- c(
      "name", "title", "description", "url",
      "version", "maintainer"
   )
   for(f in mandFields){
      if(!is.character(dbInfo[[f]]) || length(dbInfo[[f]])!=1){
         stop(sprintf(
            "%s in dbInfo should be a character vector of length 1"
         ))
      }
   }
   dbInfo <- as.list(dbInfo[mandFields])
   
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
   
   ## Confront data to model ----
   cr <- ReDaMoR::confront_data(
      dataModel,
      paths=list.files(
         path=dp,
         pattern=paste0(".", ext, "$"),
         full.names=TRUE
      ),
      returnData=FALSE,
      verbose=verbose,
      n_max=n_max,
      delim=delim,
      quoted_na=quoted_na,
      ...
   )
   assign("confrontationReport", cr[-which(names(cr)=="data")], envir=tkcatEnv)
   if(!cr$success){
      stop(ReDaMoR::format_confrontation_report(cr, title=dbInfo[["name"]]))
   }
   if(verbose){
      cat(ReDaMoR::format_confrontation_report(cr, title=dbInfo[["name"]]))
   }
   
   ## Read parameters
   readParameters <- fparam[intersect(
      setdiff(names(fparam), "n_max"),
      names(as.list(args(readr::read_delim)))
   )]
   
   ## Object ----
   toRet <- list(
      dataPath=normalizePath(dp),
      dataModel=dataModel,
      dbInfo=dbInfo,
      readParameters=readParameters
   )
   class(toRet) <- c("fileMDB", "MDB", class(toRet))
   
   ## Collection members ----
   if(!is.null(collectionMembers)){
      collection_members(toRet) <- collectionMembers
   }else{
      for(
         cm in list.files(
            path=file.path(path, "model", "Collections"),
            pattern="[.]json$",
            full.names=TRUE
         )
      ){
         collection_members(toRet) <- dplyr::bind_rows(
            collection_members(toRet),
            read_collection_members(cm)
         )
      }
   }
   
   return(toRet)
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
#' Get DB information of a [fileMDB] object
#' 
#' @param x a [fileMDB] object
#' @param ... not used
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
db_info.fileMDB <- function(x, ...){
   y <- unclass(x)
   toRet <- y$dbInfo
   toRet$size <- sum(file.size(list.files(path=y$dataPath, full.names=TRUE)))
   return(toRet)
}

###############################################################################@
#' Get the [ReDaMoR::RelDataModel] of a [fileMDB] object
#' 
#' @param x a [fileMDB] object
#' @param ... not used
#' 
#' @return A [ReDaMoR::RelDataModel] object
#' 
#' @export
#'
data_model.fileMDB <- function(x, ...){
   unclass(x)$dataModel
}

###############################################################################@
#' Get collection members of an [fileMDB] object
#' 
#' @param x a [fileMDB] object
#' @param collections a character vector indicating the collections
#' to focus on. If NA (default), all of them are taken.
#' @param ... not used
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
collection_members.fileMDB <- function(
   x,
   collections=NA,
   ...
){
   x <- unclass(x)
   toRet <- x$"collectionMembers"
   if(!is.na(collections)){
      toRet <- toRet[which(toRet$collection %in% collections),]
   }
   return(toRet)
}

###############################################################################@
#' Set collection members of a [fileMDB] object
#' 
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
      jval <- dplyr::filter(value, .data$collection==!!collection) %>%
         write_collection_members()
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
