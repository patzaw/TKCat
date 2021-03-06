###############################################################################@
#' An [MDB] (Modeled DataBase) in memory: memoMDB
#'
#' @param dataTables a list of tibbles
#' @param dataModel a [ReDaMoR::RelDataModel] object
#' @param dbInfo a list with DB information:
#' **"name"** (only mandatory field), "title", "description", "url",
#' "version", "maintainer".
#' @param collectionMembers the members of collections as provided to the
#' [collection_members<-] function (default: NULL ==> no member).
#' @param checks a character vector with the name of optional checks to be
#' done (all of them c("unique", "not nullable", "foreign keys"))
#' @param verbose if TRUE display the data confrontation report
#' (default: FALSE)
#'
#' @return A memoMDB object
#' 
#' @seealso
#' - MDB methods:
#' [db_info], [data_model], [data_tables], [collection_members],
#' [count_records], [filter_with_tables], [as_fileMDB]
#' - Additional general documentation is related to [MDB].
#' - [filter.memoMDB], [slice.memoMDB]
#' 
#' @example inst/examples/memoMDB-examples.R
#'
#' @export
#'
memoMDB <- function(
   dataTables,
   dataModel,
   dbInfo,
   collectionMembers=NULL,
   checks=c("unique", "not nullable", "foreign keys"),
   verbose=FALSE
){
   ## DB information ----
   dbInfo <- .check_dbInfo(dbInfo)
   
   ## Confront data tables to the model ----
   cr <- ReDaMoR::confront_data(
      dataModel, data=dataTables, checks=checks, verbose=FALSE,
      returnData=FALSE
   )
   assign("confrontationReport", cr, envir=tkcatEnv)
   if(!cr$success){
      stop(ReDaMoR::format_confrontation_report(cr, title=dbInfo[["name"]]))
   }
   if(verbose){
      cat(ReDaMoR::format_confrontation_report(cr, title=dbInfo[["name"]]))
   }
   
   ## Object ----
   toRet <- list(
      dataTables=dataTables[names(dataModel)],
      dataModel=dataModel,
      dbInfo=dbInfo
   )
   class(toRet) <- c("memoMDB", "MDB", class(toRet))
   
   ## Collection members ----
   collection_members(toRet) <- collectionMembers
   
   return(toRet)
}


###############################################################################@
#' Check if the object is  a [memoMDB] object
#' 
#' @param x any object
#' 
#' @return A single logical: TRUE if x is an [memoMDB] object
#' 
#' @export
#'
is.memoMDB <- function(x){
   inherits(x, "memoMDB")
}


###############################################################################@
#' Convert  any MDB object in a [memoMDB] object
#' 
#' @param x a MDB object
#' @param ... additional parameters for the [memoMDB()] function.
#' 
#' @return A [memoMDB] object
#' 
#' @seealso [get_confrontation_report], [ReDaMoR::format_confrontation_report]
#' and [ReDaMoR::format_confrontation_report_md] for getting and formatting
#' the report confronting the data to the model.
#' 
#' @export
#'
as_memoMDB <- function(x, ...){
   stopifnot(is.MDB(x))
   cm <- collection_members(x)
   if(!is.null(cm)){
      cm$resource <- db_info(x)$name
   }
   return(memoMDB(
      dataTables=data_tables(x),
      dataModel=data_model(x),
      dbInfo=db_info(x),
      collectionMembers=cm,
      ...
   ))
}


###############################################################################@
#' 
#' @param x a [memoMDB] object
#' @param value new table names
#' 
#' @rdname memoMDB
#' 
#' @export
#'
'names<-.memoMDB' <- function(x, value){
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
   names(x$dataModel) <- names(x$dataTables) <-value
   class(x) <- c("memoMDB", class(x))
   collection_members(x) <- ncolMb
   return(x)
}


###############################################################################@
#' Rename tables of a [memoMDB] object
#'
#' @param .data a [memoMDB] object
#' @param ... Use new_name = old_name to rename selected tables
#' 
#' @rdname memoMDB
#' 
#' @export
#' 
rename.memoMDB <- function(.data, ...){
   loc <- tidyselect::eval_rename(expr(c(...)), .data)
   names <- names(.data)
   names[loc] <- names(loc)
   magrittr::set_names(.data, names)
}


###############################################################################@
#' 
#' @rdname db_info
#' @method db_info memoMDB
#' 
#' @export
#'
db_info.memoMDB <- function(x, ...){
   y <- unclass(x)
   toRet <- y$dbInfo
   return(toRet)
}

###############################################################################@
#' 
#' @rdname db_info
#' @method db_info<- memoMDB
#' 
#' @export
#'
'db_info<-.memoMDB' <- function(x, value){
   toRet <- unclass(x)
   dbInfo <- .check_dbInfo(value)
   toRet$dbInfo <- dbInfo
   if(!is.null(toRet$collectionMembers)){
      toRet$collectionMembers$resource <- dbInfo$name
   }
   class(toRet) <- c("memoMDB", "MDB", class(toRet))
   return(toRet)
}


###############################################################################@
#' 
#' @rdname data_model
#' @method data_model memoMDB
#' 
#' @export
#'
data_model.memoMDB <- function(x, ...){
   unclass(x)$dataModel
}


###############################################################################@
#' 
#' @rdname collection_members
#' @method collection_members memoMDB
#' 
#' @export
#'
collection_members.memoMDB <- function(
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
#' @method collection_members<- memoMDB
#' 
#' @export
#'
'collection_members<-.memoMDB' <- function(x, value){
   
   if(is.null(value)){
      x <- unclass(x)
      x$"collectionMembers" <- value
      class(x) <- c("memoMDB", "MDB", class(x))
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
   class(x) <- c("memoMDB", "MDB", class(x))
   return(x)
}


###############################################################################@
#' 
#' @rdname data_tables
#' @method data_tables memoMDB
#' 
#' @export
#'
data_tables.memoMDB <- function(x, ..., skip=0, n_max=Inf){
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
      x$dataTables[toTake],
      function(d){
         if(skip >= nrow(d)){
            return(d[c(),])
         }
         n <- skip+1
         m <- min(nrow(d), n_max+skip)
         return(d[n:m, , drop=FALSE])
      }
   )
   names(toRet) <- names(toTake)
   return(toRet)
}


###############################################################################@
#' 
#' @rdname count_records
#' @method count_records memoMDB
#' 
#' @export
#'
count_records.memoMDB <- function(x, ...){
   lapply(
      data_tables(x, ...),
      function(y){
         if(is.matrix(y)){
            return(nrow(y)*ncol(y))
         }else{
            return(nrow(y))
         }
      }
   ) %>% unlist()
}


###############################################################################@
#' 
#' @param x a [memoMDB] object
#' @param i index or names of the tables to take
#' 
#' @rdname memoMDB
#' 
#' @export
#'
'[.memoMDB' <- function(x, i){
   if(missing(i)){
      return(x)
   }
   if(length(i)==0){
      dbi <- db_info(x)
      return(memoMDB(
         dataTables=list(),
         dataModel=RelDataModel(l=list()),
         dbInfo=dbi
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
   dt <- data_tables(x)[i]
   cm <- collection_members(x)
   if(!is.null(cm)){
      cm <- cm %>%
         dplyr::filter(.data$table %in% !!i) %>%
         dplyr::mutate(resource=!!dbi$name)
   }
   toRet <- memoMDB(
      dataTables=dt,
      dataModel=dm,
      dbInfo=dbi,
      collectionMembers=cm
   )
   return(toRet)
}


###############################################################################@
#' 
#' @param x a [memoMDB] object
#' @param i the index or the name of the tables to take
#' 
#' @rdname memoMDB
#' 
#' @export
#'
'[[.memoMDB' <- function(x, i){
   stopifnot(
      length(i)==1
   )
   return(data_tables(x, dplyr::all_of(i))[[1]])
}
#' @rdname memoMDB
#' 
#' @export
'$.memoMDB' <- `[[.memoMDB`


###############################################################################@
#'
#' @param ... [memoMDB] objects
#' 
#' @rdname memoMDB
#' 
#' @export
#'
c.memoMDB <- function(...){
   alldb <- list(...)
   if(length(alldb)==0){
      stop("At least one memoMDB should be provided as an input")
   }
   dt <- data_tables(alldb[[1]])
   for(i in 1:length(alldb)){
      if(!is.memoMDB(alldb[[i]])){
         stop("All objects should be memoMDB")
      }
   }
   dbi <- db_info(alldb[[1]])
   dm <- data_model(alldb[[1]])
   cm <- collection_members(alldb[[1]])
   if(length(alldb)>1) for(i in 2:length(alldb)){
      dm <- c(dm, data_model(alldb[[i]]))
      dt <- c(dt, data_tables(alldb[[i]]))
      cm <- dplyr::bind_rows(
         cm,
         collection_members(alldb[[i]]) %>%
            dplyr::mutate(resource=dbi$name)
      )
   }
   memoMDB(
      dataTables=dt,
      dataModel=dm,
      dbInfo=dbi,
      collectionMembers=cm
   )
}


###############################################################################@
#' 
#' @rdname as_fileMDB
#' @method as_fileMDB memoMDB
#' 
#' @export
#'
as_fileMDB.memoMDB <- function(
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
   rp <- .check_read_params(readParameters)
   descFile <- file.path(fullPath, "DESCRIPTION.json")
   .writeDescription(c(dbInfo, rp), descFile)
   
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
   ext <- ".txt.gz"
   dfiles <- file.path(dataPath, paste0(names(x), ext))
   names(dfiles) <- names(x)
   for(tn in names(x)){
      if(is.matrix(x[[tn]])){
         tw <- dplyr::as_tibble(x[[tn]], rownames="___ROWNAMES___")
      }else{
         tw <- x[[tn]]
      }
      readr::write_delim(tw, file=dfiles[tn], delim=rp$delim)
   }
   
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
#' Filter a [memoMDB] object
#' 
#' @param .data a [memoMDB] object
#' @param ... each argument should have the name of one of the tables of the
#' [memoMDB] object and contain a simple logical expression involving
#' the names of the corresponding table.
#' @param .preserve not used
#' 
#' @return a filtered [memoMDB] object
#' 
#' @export
#'
filter.memoMDB <- function(.data, ..., .preserve=FALSE){
   
   x <- .data
   
   ## Apply rules
   toRet <- list()
   dots <- enquos(...)
   for(tn in names(dots)){
      if(!tn %in% names(x)){
         stop(sprintf("%s table does not exist", tn))
      }
      if(ReDaMoR::is.MatrixModel(data_model(x)[[tn]])){
         stop("Cannot filter a matrix: start from another table")         
      }
      toRet[[tn]] <- dplyr::filter(x[[tn]], !!dots[[tn]])
   }
   
   ## Filter with tables
   return(filter_with_tables(x, toRet, checkTables=FALSE))
   
}


###############################################################################@
#' Subset a [memoMDB] object according to row position in one table
#' 
#' @param .data a [memoMDB] object
#' @param ... a single argument. The name of this argument should be a table
#' name of x and the value of this argument should be vector of integers
#' corresponding to row indexes.
#' @param .preserve not used
#' 
#' @return a [memoMDB] object
#' 
#' @export
#'
slice.memoMDB <- function(.data, ..., .preserve=FALSE){
   
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
   toRet[[tn]] <- dplyr::slice(x[[tn]], i)
   
   ## Filter with tables
   return(filter_with_tables(x, toRet, checkTables=FALSE))
   
}


###############################################################################@
#' 
#' @rdname filter_with_tables
#' @method filter_with_tables memoMDB
#' 
#' @export
#'
filter_with_tables.memoMDB <- function(x, tables, checkTables=TRUE){
   
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
   tables <- .memo_filtByConta(tables, data_tables(x), fk, dm)
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

## Helpers ----
.write_chTables.memoMDB <- function(x, con, dbName){
   dm <- data_model(x)
   for(tn in names(x)){
      if(ReDaMoR::is.MatrixModel(dm[[tn]])){
         if(ncol(x[[tn]]) > CH_MAX_COL && nrow(x[[tn]]) < ncol(x[[tn]])){
            tw <- t(x[[tn]])
            transposed <- TRUE
         }else{
            tw <- x[[tn]]
            transposed <- FALSE
         }
         colList <- seq(1, ncol(tw), by=CH_MAX_COL)
         colList <- lapply(
            colList,
            function(i){
               colnames(tw)[i:min(i+CH_MAX_COL-1, ncol(tw))]
            }
         )
         names(colList) <- uuid::UUIDgenerate(n=length(colList))
         nullable <- dm[[tn]]$fields %>% 
            dplyr::filter(!.data$type %in% c("column", "row")) %>% 
            dplyr::pull("nullable")
         vtype <- dm[[tn]]$fields %>% 
            dplyr::filter(!.data$type %in% c("column", "row")) %>% 
            dplyr::pull("type")
         for(stn in names(colList)){
            stw <- tw[,colList[[stn]]] %>% 
               as_tibble(
                  rownames=ifelse(
                     transposed,
                     "___COLNAMES___",
                     "___ROWNAMES___"
                  )
               )
            nulcol <- NULL
            if(nullable){
               nulcol <- colList[[stn]]
            }
            write_MergeTree(
               con=con,
               dbName=dbName,
               tableName=stn,
               value=stw,
               rtypes=c("character", rep(vtype, ncol(stw) - 1)) %>% 
                  magrittr::set_names(colnames(stw)),
               nullable=nulcol,
               sortKey=colnames(stw)[1]
            )
            rm(stw)
            gc()
         }
         ch_insert(
            con=con, dbName=dbName, tableName=tn,
            value=tibble(table=names(colList))
         )
      }else{
         ch_insert(con=con, dbName=dbName, tableName=tn, value=x[[tn]])
      }
   }
}

.memo_filtByConta <- function(d, all, fk, dm){
   nfk <- fk
   .contaminate <- function(tn){
      
      ## Backward ----
      fkf <- fk %>% dplyr::filter(.data$from==!!tn & .data$fmin>0)
      fkt <- fk %>% dplyr::filter(.data$to==!!tn & .data$tmin>0)
      nfk <<- nfk %>%
         dplyr::anti_join(
            dplyr::select(fkf, "from", "to"), by=c("from", "to")
         ) %>% 
         dplyr::anti_join(
            dplyr::select(fkt, "from", "to"), by=c("from", "to")
         )
      fkl <- dplyr::bind_rows(
         fkf,
         fkt %>% dplyr::rename("from"="to", "ff"="tf", "to"="from", "tf"="ff")
      ) %>% 
         distinct()
      if(nrow(fkl)>0){
         for(i in 1:nrow(fkl)){
            ntn <- fkl$to[i]
            if(ntn %in% names(d)){
               nv <- d[[ntn]]
            }else{
               nv <- all[[ntn]]
            }
            d[[ntn]] <<- .mdjoin(
               d1=nv, d2=d[[tn]],
               by=magrittr::set_names(
                  fkl$ff[[i]], fkl$tf[[i]]
               ),
               tm1=dm[[ntn]], tm2=dm[[tn]]
            )
         }
      }
      
      ## Forward ----
      fkf <- fk %>% dplyr::filter(.data$from==!!tn & .data$fmin==0)
      fkt <- fk %>% dplyr::filter(.data$to==!!tn & .data$tmin==0)
      nfk <<- nfk %>%
         dplyr::anti_join(
            dplyr::select(fkf, "from", "to"), by=c("from", "to")
         ) %>% 
         dplyr::anti_join(
            dplyr::select(fkt, "from", "to"), by=c("from", "to")
         )
      fkl <- dplyr::bind_rows(
         fkf,
         fkt %>% dplyr::rename("from"="to", "ff"="tf", "to"="from", "tf"="ff")
      ) %>% 
         distinct()
      if(nrow(fkl)>0){
         for(i in 1:nrow(fkl)){
            
            ntn <- fkl$to[i]
            
            toAdd <- .mdjoin(
               d1=all[[ntn]], d2=d[[tn]],
               by=magrittr::set_names(
                  fkl$ff[[i]], fkl$tf[[i]]
               ),
               tm1=dm[[ntn]], tm2=dm[[tn]]
            )
            
            if(is.matrix(toAdd)){
               d[[ntn]] <<- all[[ntn]][
                  c(rownames(d[[ntn]]), rownames(toAdd)),
                  c(colnames(d[[ntn]]), colnames(toAdd)),
                  drop=FALSE
               ]
            }else{
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
      d <- .memo_filtByConta(d, all, nfk, dm)
   }
   return(d)
}


.norm_data <- function(dataTables, dataModel){
   toRet <- dataTables
   fk <- ReDaMoR::get_foreign_keys(dataModel)
   if(is.null(fk)){
      return(toRet)
   }
   fk <- fk %>%
      dplyr::filter(
         .data$from %in% names(dataTables) | .data$to %in% names(dataTables)
      )
   .norm_table <- function(tn){
      fkf <- fk %>% dplyr::filter(.data$from==!!tn & .data$fmin>0)
      fkt <- fk %>% dplyr::filter(.data$to==!!tn & .data$tmin>0)
      fkl <- dplyr::bind_rows(
         fkf,
         fkt %>% dplyr::rename("from"="to", "ff"="tf", "to"="from", "tf"="ff")
      )
      if(nrow(fkl)>0){
         for(i in 1:nrow(fkl)){
            ntn <- fkl$to[i]
            # nv <- dplyr::semi_join(
            #    toRet[[ntn]], toRet[[tn]],
            #    by=magrittr::set_names(
            #       fkl$ff[[i]], fkl$tf[[i]]
            #    )
            # )
            nv <- .mdjoin(
               d1=toRet[[ntn]], d2=toRet[[tn]],
               by=magrittr::set_names(
                  fkl$ff[[i]], fkl$tf[[i]]
               ),
               tm1=dataModel[[ntn]], tm2=dataModel[[tn]]
            )
            if(is.matrix(nv)){
               if(
                  nrow(nv) < nrow(toRet[[ntn]]) ||
                  ncol(nv) < ncol(toRet[[ntn]])
               ){
                  toRet[[ntn]] <<- nv
                  .norm_table(ntn)
               }
            }else{
               if(nrow(nv) < nrow(toRet[[ntn]])){
                  toRet[[ntn]] <<- nv
                  .norm_table(ntn)
               }
            }
         }
      }
   }
   for(tn in names(toRet)){
      .norm_table(tn)
   }
   return(toRet)
}

.mdjoin <- function(d1, d2, by, tm1, tm2){
   
   ## Two tibbles ----
   if(!is.matrix(d1) && !is.matrix(d2)){
      return(dplyr::semi_join(d1, d2, by=by))
   }
   
   getfvalues <- function(d, fields, tm){
      lapply(fields, function(field){
         if(tm$fields$type[which(tm$fields$name==field)]=="row"){
            toRet <- rownames(d)
            if(is.null(toRet)){
               toRet <- character()
            }
            attr(toRet, "f") <- "row"
            return(toRet)
         }
         if(tm$fields$type[which(tm$fields$name==field)]=="column"){
            toRet <- colnames(d)
            if(is.null(toRet)){
               toRet <- character()
            }
            attr(toRet, "f") <- "column"
            return(toRet)
         }
         toRet <- d
         if(is.null(toRet)){
            toRet <- character()
         }
         attr(toRet, "f") <- "value"
         return(toRet)
      }) %>%
         magrittr::set_names(fields)
   }
   
   ## A matrix to filter ----
   if(is.matrix(d1)){
      d1val <-  getfvalues(d1, names(by), tm1)
      if(is.matrix(d2)){
         d2val <-  getfvalues(d2, as.character(by), tm2)
      }else{
         d2val <- lapply(as.character(by), function(f){d2[[f]]}) %>% 
            magrittr::set_names(as.character(by))
      }
      toTake <- lapply(1:length(d1val), function(fi){
         toTake <- which(d1val[[fi]] %in% d2val[[fi]])
         if(attr(d1val[[fi]], "f")=="column"){
            colToTake <- toTake
            rowToTake <- 1:nrow(d1)
         }
         if(attr(d1val[[fi]], "f")=="row"){
            colToTake <- 1:ncol(d1)
            rowToTake <- toTake
         }
         if(attr(d1val[[fi]], "f")=="value"){
            colToTake <- ((toTake - 1) %/% nrow(d1)) + 1
            rowToTake <- ((toTake - 1) %% nrow(d1)) + 1
         }
         return(list(rows= rowToTake, cols=colToTake))
      })
      rowToTake <- Reduce(intersect, lapply(toTake, function(x){x$rows}))
      colToTake <- Reduce(intersect, lapply(toTake, function(x){x$cols}))
      toRet <- d1[rowToTake, colToTake, drop=FALSE]
      return(toRet)
   }
   
   ## A tibble to filter with a matrix ----
   if(is.matrix(d2)){
      d2val <-  getfvalues(d2, as.character(by), tm2)
      toRet <- d1
      for(fi in 1:length(by)){
         toTake <- which(toRet[[names(by)[fi]]] %in% d2val[[fi]])
         toRet <- toRet[toTake,]
      }
      return(toRet)
   }
   
}
