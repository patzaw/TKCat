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
#' @param check logical: if TRUE (default) the data are confronted to the
#' data model
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
#' [count_records], [dims], [filter_with_tables], [as_fileMDB]
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
   check=TRUE,
   checks=c("unique", "not nullable", "foreign keys"),
   verbose=FALSE
){
   ## DB information ----
   dbInfo <- .check_dbInfo(dbInfo)
   
   ## Confront data tables to the model ----
   if(check){
      cr <- ReDaMoR::confront_data(
         dataModel, data=dataTables, checks=checks, verbose=FALSE,
         returnData=FALSE
      )
      assign("confrontationReport", cr, envir=tkcatEnv)
      if(!cr$success){
         cat(ReDaMoR::format_confrontation_report(cr, title=dbInfo[["name"]]))
         stop("Data do not fit the data model")
      }
      if(verbose){
         cat(ReDaMoR::format_confrontation_report(cr, title=dbInfo[["name"]]))
      }
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
   loc <- tidyselect::eval_rename(rlang::expr(c(...)), .data)
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
      x$dataTables[toTake],
      function(d){
         if(skip >= nrow(d)){
            return(d[integer(0),, drop=FALSE])
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
#' @rdname heads
#' @method heads memoMDB
#' 
#' @export
#'
heads.memoMDB <- function(x, ..., n=6L){
   
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
   x <- unclass(x)
   
   toRet <- lapply(
      names(toTake),
      function(name){
         if(ReDaMoR::is.MatrixModel(m[[name]])){
            d <- x$dataTables[[name]]
            if(n >= length(d)){
               return(d)
            }else{
               mn <- min(floor(sqrt(n)), ncol(d))
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
               return(d[1:nr, 1:nc])
            }
         }else{
            return(utils::head(x$dataTables[[name]], n))
         }
      }
   )
   names(toRet) <- names(toTake)
   
   return(toRet)
   
}


###############################################################################@
#' 
#' @rdname dims
#' @method dims memoMDB
#' 
#' @export
#'
dims.memoMDB <- function(x, ...){
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
   dl <- data_tables(x, ...)
   do.call(dplyr::bind_rows, lapply(
      names(dl),
      function(n){
         y <- dl[[n]]
         return(
            dplyr::tibble(
               name=n,
               format=ifelse(is.data.frame(y), "table", class(y)[1]),
               ncol=ncol(y),
               nrow=nrow(y),
               records=ifelse(
                  is.data.frame(y), nrow(y),
                  as.numeric(ncol(y)) * as.numeric(nrow(y))
               ),
               bytes=as.numeric(object.size(y)),
               transposed=FALSE
            )
         )
      }
   ))
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
         dataModel=ReDaMoR::RelDataModel(l=list()),
         dbInfo=dbi,
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
      collectionMembers=cm,
      check=FALSE
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
         invisible()
      }else{
         return(data_tables(x, dplyr::all_of(i))[[1]])
      }
   }
   # return(data_tables(x, dplyr::all_of(i))[[1]])
}
#' @rdname memoMDB
#' 
#' @export
'$.memoMDB' <- `[[.memoMDB`


###############################################################################@
#' 
#' @rdname as_fileMDB
#' @method as_fileMDB memoMDB
#' 
#' @export
#'
as_fileMDB.memoMDB <- function(
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
   rp <- .check_read_params(readParameters)
   descFile <- file.path(fullPath, "DESCRIPTION.json")
   .writeDescription(c(dbInfo, rp), descFile)
   
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
   ext <- ifelse(compress, ".txt.gz", ".txt")
   dfiles <- file.path(dataPath, paste0(names(x), ext))
   names(dfiles) <- names(x)
   for(tn in names(x)){
      if(inherits(x[[tn]], c("dgCMatrix", "dgTMatrix"))){
         rntw <- rownames(x[[tn]])
         cntw <- colnames(x[[tn]])
         tw <- dplyr::as_tibble(Matrix::summary(x[[tn]]))
         readr::write_delim(
            dplyr::tibble(h=c(
               "%%MatrixMarket matrix coordinate real general",
               paste0("%%Rownames: ", paste(rntw, collapse="\t")),
               paste0("%%Colnames: ", paste(cntw, collapse="\t")),
               paste(length(rntw), length(cntw), nrow(tw), sep=" ")
            )),
            file=dfiles[tn],
            delim="\t",
            quote="none",
            col_names=FALSE
         )
         readr::write_delim(
            tw, file=dfiles[tn],
            delim="\t",
            col_names=FALSE,
            append=TRUE
         )
      }else{
         if(inherits(x[[tn]], c("matrix", "Matrix"))){
            tw <- dplyr::as_tibble(
               as.matrix(x[[tn]]), rownames="___ROWNAMES___"
            )
         }else{
            tw <- x[[tn]]
         }
         readr::write_delim(
            tw, file=dfiles[tn],
            delim=rp$delim,
            na=rp$na,
            quote="all", escape="double"
         )
      }
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
   dots <- rlang::enquos(...)
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
filter_with_tables.memoMDB <- function(x, tables, checkTables=TRUE, ...){
   
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
      collectionMembers=cm,
      check=FALSE
   ))
   
}


###############################################################################@
#' 
#' @rdname filter_mdb_matrix
#' @method filter_mdb_matrix memoMDB
#' 
#' @export
#'
filter_mdb_matrix.memoMDB <- function(x, tableName, ...){
   
   ## Checks ----
   stopifnot(
      is.memoMDB(x),
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
         fr <- intersect(iFilter[[f]], rownames(x[[tableName]]))
         frc <- c(frc, "r")
      }else{
         fc <- intersect(iFilter[[f]], colnames(x[[tableName]]))
         frc <- c(frc, "c")
      }
   }
   frc <- paste(sort(frc), collapse="")
   
   ## Get the results ----
   if(frc==""){
      stop("Dev. error: review this part of the function")
   }
   if(frc=="r"){
      toRet <- x[[tableName]][fr, , drop=FALSE]
   }
   if(frc=="c"){
      toRet <- x[[tableName]][, fc, drop=FALSE]
   }
   if(frc=="cr"){
      toRet <- x[[tableName]][fr, fc, drop=FALSE]
   }
   return(toRet)
   
}


###############################################################################@
## Helpers ----
.write_chTables.memoMDB <- function(x, con, dbName, by, ...){
   dm <- data_model(x)
   for(tn in names(x)){
      if(ReDaMoR::is.MatrixModel(dm[[tn]])){
         
         if(inherits(x[[tn]], c("dgCMatrix", "dgTMatrix"))){

            ## Sparse matrix ----
            
            ## Rownames and colnames
            rowTable <- dplyr::tibble(
               i=1:nrow(x[[tn]]),
               name=rownames(x[[tn]])
            )
            colTable <- dplyr::tibble(
               j=1:ncol(x[[tn]]),
               name=colnames(x[[tn]])
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
            nullable <- dm[[tn]]$fields %>% 
               dplyr::filter(!.data$type %in% c("column", "row")) %>% 
               dplyr::pull("nullable")
            if(nullable){
               nullable <- "x"
            }else{
               nullable <- NULL
            }
            valTable <- modTable$table[which(modTable$info=="values")]
            tw <- dplyr::as_tibble(Matrix::summary(x[[tn]]))
            write_MergeTree(
               con=con,
               dbName=dbName,
               tableName=valTable,
               value=tw,
               rtypes=c("i"="integer", "j"="integer", "x"="numeric"),
               nullable=nullable,
               sortKey=c("i", "j")
            )
            
            ## Reference table
            ch_insert(
               con=con, dbName=dbName, tableName=tn,
               value=modTable
            )
            
         }else{
            
            ## Matrix ----
         
            if(ncol(x[[tn]]) > CH_MAX_COL && nrow(x[[tn]]) < ncol(x[[tn]])){
               tw <- x[[tn]]
               gc()
               transposed <- TRUE
               scn <- sort(rownames(tw))
               colList <- seq(1, nrow(tw), by=CH_MAX_COL)
               colList <- lapply(
                  colList,
                  function(i){
                     scn[i:min(i+CH_MAX_COL-1, nrow(tw))]
                  }
               )
            }else{
               tw <- x[[tn]]
               transposed <- FALSE
               scn <- sort(colnames(tw))
               colList <- seq(1, ncol(tw), by=CH_MAX_COL)
               colList <- lapply(
                  colList,
                  function(i){
                     scn[i:min(i+CH_MAX_COL-1, ncol(tw))]
                  }
               )
            }
            names(colList) <- uuid::UUIDgenerate(n=length(colList))
            nullable <- dm[[tn]]$fields %>% 
               dplyr::filter(!.data$type %in% c("column", "row")) %>% 
               dplyr::pull("nullable")
            vtype <- dm[[tn]]$fields %>% 
               dplyr::filter(!.data$type %in% c("column", "row")) %>% 
               dplyr::pull("type")
            for(stn in names(colList)){
               if(transposed){
                  stw <- tw[colList[[stn]], , drop=FALSE] %>% 
                     t() %>% 
                     dplyr::as_tibble(rownames="___COLNAMES___")
               }else{
                  stw <- tw[, colList[[stn]], drop=FALSE] %>% 
                     dplyr::as_tibble(rownames="___ROWNAMES___")
               }
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
               value=dplyr::tibble(table=names(colList), info="values")
            )
            
         }
      }else{{
         
         ## Table ----
         
         toWrite <- x[[tn]]
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
         ch_insert(con=con, dbName=dbName, tableName=tn, value=toWrite)
      }}
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
         dplyr::distinct()
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
         dplyr::distinct()
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
            
            if(inherits(toAdd, c("matrix", "Matrix"))){
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
            nv <- .mdjoin(
               d1=toRet[[ntn]], d2=toRet[[tn]],
               by=magrittr::set_names(
                  fkl$ff[[i]], fkl$tf[[i]]
               ),
               tm1=dataModel[[ntn]], tm2=dataModel[[tn]]
            )
            if(inherits(nv, c("matrix", "Matrix"))){
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
   if(
      !inherits(d1, c("matrix", "Matrix")) &&
      !inherits(d2, c("matrix", "Matrix"))
   ){
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
   if(inherits(d1, c("matrix", "Matrix"))){
      d1val <-  getfvalues(d1, names(by), tm1)
      if(inherits(d2, c("matrix", "Matrix"))){
         d2val <-  getfvalues(d2, as.character(by), tm2)
      }else{
         d2val <- lapply(as.character(by), function(f){d2[[f]]}) %>% 
            magrittr::set_names(as.character(by))
      }
      toTake <- lapply(1:length(d1val), function(fi){
         toTake <- which(d1val[[fi]] %in% d2val[[fi]])
         if(attr(d1val[[fi]], "f")=="column"){
            colToTake <- toTake
            if(nrow(d1)>0){
               rowToTake <- 1:nrow(d1)
            }else{
               rowToTake <- integer(0)
            }
         }
         if(attr(d1val[[fi]], "f")=="row"){
            if(ncol(d1)>0){
               colToTake <- 1:ncol(d1)
            }else{
               colToTake <- integer(0)
            }
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
   if(inherits(d2, c("matrix", "Matrix"))){
      d2val <-  getfvalues(d2, as.character(by), tm2)
      toRet <- d1
      for(fi in 1:length(by)){
         toTake <- which(toRet[[names(by)[fi]]] %in% d2val[[fi]])
         toRet <- toRet[toTake, , drop=FALSE]
      }
      return(toRet)
   }
   
}
