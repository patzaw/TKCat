###############################################################################@
# @example inst/examples/metaMDB-examples.R
#
#' A metaMDB object
#' 
#' A metaMDB object is an [MDB] gathering several other MDBs glued by
#' relational tables.
#'
#' @param MDBs a list of [MDB] objects
#' @param relationalTables a list of tibbles corresponding to the relational
#' tables between the different MDBs
#' @param dataModel a [ReDaMoR::RelDataModel] object gathering all the
#' data model of all the MDBs plus the relational tables
#' @param dbInfo a list with DB information:
#' **"name"** (only mandatory field), "title", "description", "url",
#' "version", "maintainer".
#' @param check logical: if TRUE (default) the data are confronted to the
#' data model
#'
#' @return A metaMDB object
#' 
#' @seealso
#' - MDB methods:
#' [db_info], [data_model], [data_tables], [collection_members],
#' [count_records], [dims], [filter_with_tables], [as_fileMDB]
#' - Additional general documentation is related to [MDB].
#' - [filter.metaMDB], [slice.metaMDB]
#' - [get_confrontation_report], [ReDaMoR::format_confrontation_report]
#' and [ReDaMoR::format_confrontation_report_md] for getting and formatting
#' the report confronting the data to the model.
#'
#' @export
#'
metaMDB <- function(
   MDBs,
   relationalTables,
   dataModel,
   dbInfo,
   check=TRUE
){
   
   ## DB information ----
   dbInfo <- .check_dbInfo(dbInfo)
   
   ## Ambiguous or inconsistent information ----
   udbn <- unique(names(MDBs))
   if(length(udbn)!=length(MDBs)){
      stop("Check MDBs names: there should be one and only one for each MDB")
   }
   for(dbn in names(MDBs)){
      if(dbn!=db_info(MDBs[[dbn]])$name){
         stop(sprintf(
            'Check db_info(MDBs[[%s]] for name inconsistency: %s',
            dbn, db_info(MDBs[[dbn]])$name
         ))
      }
   }
   tn <- unlist(lapply(MDBs, names))
   if(any(duplicated(tn))){
      stop(
         "The following table names are used by several MDBs:",
         paste(unique(tn[which(duplicated(tn))]))
      )
   }
   
   ## Data models ----
   if(check){
      for(mdb in names(MDBs)){
         if(
            !ReDaMoR::identical_RelDataModel(
               data_model(MDBs[[mdb]]),
               dataModel[names(MDBs[[mdb]]), rmForeignKeys=TRUE],
               includeDisplay=FALSE
            )
         ){
            stop("Incompatible datamodels")
         }
      }
   }
   
   ## Relational tables
   atn <- c(unlist(lapply(MDBs, names)), names(relationalTables))
   if(!all(names(dataModel) %in% atn)){
      stop("Inconsistent data model")
   }
   
   if(check){
      cr <- ReDaMoR::confront_data(
         dataModel[names(relationalTables), rmForeignKeys=TRUE],
         data=relationalTables,
         verbose=FALSE,
         returnData=FALSE
      )
      assign("confrontationReport", cr, envir=tkcatEnv)
      if(!cr$success){
         cat(ReDaMoR::format_confrontation_report(
            cr, title="Relational tables"
         ))
         stop("Data do not fit the data model")
      }
   }
   
   ## Object ----
   toRet <- list(
      MDBs=MDBs,
      relationalTables=relationalTables,
      dataModel=dataModel[atn],
      dbInfo=dbInfo
   )
   class(toRet) <- c("metaMDB", "MDB", class(toRet))
   return(toRet)
}


###############################################################################@
#' Check if the object is  a [metaMDB] object
#' 
#' @param x any object
#' 
#' @return A single logical: TRUE if x is an [metaMDB] object
#' 
#' @export
#'
is.metaMDB <- function(x){
   inherits(x, "metaMDB")
}


###############################################################################@
#' Get a list of MDB from [metaMDB] object
#' 
#' @param x a [metaMDB] object
#' 
#' @return A list of MDB objects
#' 
#' @export
#'
MDBs <- function(x){
   stopifnot(is.metaMDB(x))
   return(unclass(x)$MDBs)
}


###############################################################################@
#' 
#' @rdname db_disconnect
#' @method db_disconnect metaMDB
#' 
#' @export
#'
db_disconnect.metaMDB <- function(x){
   mdbs <- MDBs(x)
   for(i in 1:length(mdbs)){
      if(is.chMDB(mdbs[[i]]) || is.metaMDB(mdbs[[i]])){
         db_disconnect(mdbs[[i]])
      }
   }
   invisible()
}


###############################################################################@
#' 
#' @rdname db_reconnect
#' @method db_reconnect metaMDB
#' 
#' @export
#'
db_reconnect.metaMDB <- function(x, user, password, ntries=3, ...){
   
   xn <- deparse(substitute(x))
   mdbs <- MDBs(x)
   rtab <- relational_tables(x)
   
   mdbs <- lapply(mdbs, function(y){
      if(is.chMDB(y) || is.metaMDB(y)){
         toRet <- db_reconnect(y, ...)
      }else{
         toRet <- y
      }
      return(toRet)
   })
   
   ## Object ----
   toRet <- list(
      MDBs=mdbs,
      relationalTables=rtab,
      dataModel=data_model(x),
      dbInfo=db_info(x)
   )
   class(toRet) <- c("metaMDB", "MDB", class(toRet))
   assign(xn, toRet, envir=parent.frame(n=1))
   invisible(toRet)
}


###############################################################################@
#' 
#' @rdname get_hosts
#' @method get_hosts metaMDB
#' 
#' @export
#'
get_hosts.metaMDB <- function(x, ...){
   mdbs <- MDBs(x)
   toRet <- lapply(mdbs, function(y){
      if(is.chMDB(y) || is.metaMDB(y)){
         toRet <- get_hosts(y, ...)
      }else{
         toRet <- NULL
      }
      return(toRet)
   })
   return(unique(unlist(toRet)))
}



###############################################################################@
#' Get a list of relational tables
#' 
#' @param x a [metaMDB] object
#' @param recursive if TRUE, function returns also the
#' relational tables from embedded metaMDBs.
#' 
#' @return A list of relational tables (tibbles)
#' 
#' @export
#'
relational_tables <- function(x, recursive=FALSE){
   stopifnot(is.metaMDB(x))
   toRet <- unclass(x)$relationalTables
   if(recursive){
      toRet <- c(toRet, do.call(c, magrittr::set_names(lapply(
         MDBs(x),
         function(y){
            if(is.metaMDB(y)){
               return(relational_tables(y, recursive=TRUE))
            }else{
               return(NULL)
            }
         }
      ), NULL)))
   }
   return(toRet)
}


###############################################################################@
#' 
#' @param x a [metaMDB] object
#' @param value new table names
#' 
#' @rdname metaMDB
#' 
#' @export
#'
'names<-.metaMDB' <- function(x, value){
   stopifnot(
      length(value)==length(x),
      sum(duplicated(value))==0
   )
   if(length(x)==0){
      return(x)
   }
   x <- unclass(x)
   orinames <- names(x$dataModel)
   nnames <- value
   names(nnames) <- orinames
   names(x$dataModel) <- value
   for(mdb in names(x$MDBs)){
      if(length(x$MDBs[[mdb]]) > 0){
         names(x$MDBs[[mdb]]) <- as.character(nnames[names(x$MDBs[[mdb]])])
      }
   }
   if(length(x$relationalTables)>0){
      names(x$relationalTables) <- as.character(
         nnames[names(x$relationalTables)]
      )
   }
   class(x) <- c("metaMDB", "MDB", class(x))
   return(x)
}


###############################################################################@
#' Rename tables of a [metaMDB] object
#'
#' @param .data a [metaMDB] object
#' @param ... Use new_name = old_name to rename selected tables
#' 
#' @rdname metaMDB
#' 
#' @export
#' 
rename.metaMDB <- function(.data, ...){
   loc <- tidyselect::eval_rename(rlang::expr(c(...)), .data)
   names <- names(.data)
   names[loc] <- names(loc)
   magrittr::set_names(.data, names)
}


###############################################################################@
#' 
#' @rdname db_info
#' @method db_info metaMDB
#' 
#' @export
#'
db_info.metaMDB <- function(x, ...){
   y <- unclass(x)
   toRet <- y$dbInfo
   return(toRet)
}

###############################################################################@
#' 
#' @rdname db_info
#' @method db_info<- metaMDB
#' 
#' @export
#'
'db_info<-.metaMDB' <- function(x, value){
   toRet <- unclass(x)
   dbInfo <- .check_dbInfo(value)
   toRet$dbInfo <- dbInfo
   class(toRet) <- c("metaMDB", "MDB", class(toRet))
   return(toRet)
}


###############################################################################@
#' 
#' @param rtOnly if TRUE, the function only returns the relational tables
#' and the corresponding foreign tables (default: FALSE)
#' @param recursive if TRUE and rtOnly, the function returns also the
#' relational tables from embedded metaMDBs.
#' 
#' @rdname data_model
#' @method data_model metaMDB
#' 
#' @export
#'
data_model.metaMDB <- function(x, rtOnly=FALSE, recursive=FALSE, ...){
   toRet <- unclass(x)$dataModel
   if(rtOnly){
      rt <- names(relational_tables(x, recursive=recursive))
      fk <- ReDaMoR::get_foreign_keys(toRet)
      toTake <- fk %>%
         dplyr::filter(
            .data$from %in% rt
         ) %>%
         dplyr::pull("to") %>% 
         c(
            fk %>%
               dplyr::filter(
                  .data$to %in% rt
               ) %>%
               dplyr::pull("from")
         ) %>% 
         c(rt) %>% 
         unique()
      toRet <- toRet[toTake, rmForeignKeys=TRUE]
   }
   return(toRet)
}


###############################################################################@
#' 
#' @rdname collection_members
#' @method collection_members metaMDB
#' 
#' @export
#'
collection_members.metaMDB <- function(
   x,
   ...
){
   x <- unclass(x)
   toRet <- do.call(rbind, lapply(x$MDBs, collection_members))
   toTake <- unlist(list(...))
   if(length(toTake)>0){
      stopifnot(is.character(toTake))
      toRet <- toRet[which(toRet$collection %in% toTake),]
   }
   return(toRet)
}


###############################################################################@
#' 
#' @rdname data_tables
#' @method data_tables metaMDB
#' 
#' @export
#'
data_tables.metaMDB <- function(x, ..., skip=0, n_max=Inf){
   if(length(x)==0){
      return(list())
   }
   toTake <- tidyselect::eval_select(rlang::expr(c(...)), x)
   if(length(toTake)==0){
      toTake <- 1:length(x)
      names(toTake) <- names(x)
   }
   toTake <- names(toTake)
   x <- unclass(x)
   toRet <- list()
   for(mdb in names(x$MDBs)){
      lToTake <- intersect(toTake, names(x$MDBs[[mdb]]))
      if(length(lToTake)>0){
         toRet <- c(
            toRet,
            data_tables(
               x$MDBs[[mdb]], dplyr::all_of(lToTake),
               skip=skip, n_max=n_max
            )
         )
      }
   }
   lToTake <- intersect(toTake, names(x$relationalTables))
   if(length(lToTake)>0){
      toRet <- c(
         toRet,
         lapply(
            x$relationalTables[lToTake],
            function(d){
               if(skip >= nrow(d)){
                  return(d[integer(0),])
               }
               n <- skip+1
               m <- min(nrow(d), n_max+skip)
               return(d[n:m, , drop=FALSE])
            }
         )
      )
   }
   toRet <- toRet[toTake]
   return(toRet)
}


###############################################################################@
#' 
#' @rdname heads
#' @method heads metaMDB
#' 
#' @export
#'
heads.metaMDB <- function(x, ..., n=6L){
   if(length(x)==0){
      return(list())
   }
   toTake <- tidyselect::eval_select(rlang::expr(c(...)), x)
   if(length(toTake)==0){
      toTake <- 1:length(x)
      names(toTake) <- names(x)
   }
   toTake <- names(toTake)
   x <- unclass(x)
   toRet <- list()
   for(mdb in names(x$MDBs)){
      lToTake <- intersect(toTake, names(x$MDBs[[mdb]]))
      if(length(lToTake)>0){
         toRet <- c(toRet, heads(x$MDBs[[mdb]], dplyr::all_of(lToTake), n=n))
      }
   }
   lToTake <- intersect(toTake, names(x$relationalTables))
   if(length(lToTake)>0){
      toRet <- c(toRet, lapply(x$relationalTables[lToTake], utils::head, n=n))
   }
   toRet <- toRet[toTake]
   return(toRet)
}


###############################################################################@
#' 
#' @rdname dims
#' @method dims metaMDB
#' 
#' @export
#'
dims.metaMDB <- function(x, ...){
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
   toTake <- tidyselect::eval_select(rlang::expr(c(...)), x)
   if(length(toTake)==0){
      toTake <- 1:length(x)
      names(toTake) <- names(x)
   }
   toTake <- names(toTake)
   x <- unclass(x)
   toRet <- c()
   for(mdb in names(x$MDBs)){
      lToTake <- intersect(toTake, names(x$MDBs[[mdb]]))
      if(length(lToTake)>0){
         toRet <- dplyr::bind_rows(
            toRet,
            dims(x$MDBs[[mdb]], dplyr::all_of(lToTake))
         )
      }
   }
   lToTake <- intersect(toTake, names(x$relationalTables))
   if(length(lToTake)>0){
      toAdd <- do.call(dplyr::bind_rows, lapply(
         lToTake,
         function(n){
            y <- x$relationalTables[[n]]
            return(
               dplyr::tibble(
                  name=n,
                  format=ifelse(is.matrix(y), "matrix", "table"),
                  ncol=ncol(y),
                  nrow=nrow(y),
                  records=ifelse(is.matrix(y), ncol(y)*nrow(y), nrow(y)),
                  bytes=as.numeric(object.size(y)),
                  transposed=FALSE
               )
            )
         }
      ))
      toRet <- dplyr::bind_rows(toRet, toAdd)
   }
   toRet <- toRet[match(toTake, toRet$name),]
   return(toRet)
}


###############################################################################@
#' 
#' @param x a [metaMDB] object
#' @param i index or names of the tables to take
#' 
#' @rdname metaMDB
#' 
#' @export
#'
'[.metaMDB' <- function(x, i){
   if(missing(i)){
      return(x)
   }
   if(length(i)==0){
      dbi <- db_info(x)
      return(metaMDB(
         MDBs=list(),
         relationalTables=list(),
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
   
   mdbs <- MDBs(x)
   fmdbs <- list()
   for(mdb in names(mdbs)){
      lToTake <- intersect(i, names(mdbs[[mdb]]))
      if(length(lToTake)>0){
         fmdbs[[mdb]] <- mdbs[[mdb]][lToTake]
      }
   }
   lToTake <- intersect(i, names(relational_tables(x)))
   frt <- relational_tables(x)[lToTake]
   
   toRet <- metaMDB(
      MDBs=fmdbs,
      relationalTables=frt,
      dataModel=dm,
      dbInfo=dbi,
      check=FALSE
   )
   return(toRet)
}


###############################################################################@
#' 
#' @param x a [metaMDB] object
#' @param i the index or the name of the tables to take
#' 
#' @rdname metaMDB
#' 
#' @export
#'
'[[.metaMDB' <- function(x, i){
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
#' @rdname metaMDB
#' 
#' @export
'$.metaMDB' <- `[[.metaMDB`


###############################################################################@
#' 
#' @rdname as_fileMDB
#' @method as_fileMDB metaMDB
#' 
#' @export
#'
as_fileMDB.metaMDB <- function(
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
      cm$resource <- dbName
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
   
   adfiles <- c()
   for(mdb in MDBs(x)){
      tmp <- as_fileMDB(
         mdb, path=dataPath, readParameters=rp, compress=compress, by=by
      )
      ofiles <- data_files(tmp)$dataFiles
      dfiles <- file.path(dataPath, basename(ofiles)) %>%
         magrittr::set_names(names(ofiles))
      file.rename(ofiles, dfiles)
      unlink(file.path(dataPath, db_info(tmp)$name), recursive=TRUE)
      adfiles <- c(adfiles, dfiles)
   }
   frdb <- as_memoMDB(x[names(relational_tables(x))])
   tmp <- as_fileMDB(
      frdb, path=dataPath, readParameters=rp, compress=compress, by=by
   )
   ofiles <- data_files(tmp)$dataFiles
   dfiles <- file.path(dataPath, basename(ofiles)) %>%
      magrittr::set_names(names(ofiles))
   file.rename(ofiles, dfiles)
   unlink(file.path(dataPath, db_info(tmp)$name), recursive=TRUE)
   adfiles <- c(adfiles, dfiles)
   
   ## Return fileMDB ----
   return(fileMDB(
      dataFiles=adfiles,
      dbInfo=dbInfo,
      dataModel=dm,
      readParameters=rp,
      collectionMembers=cm
   ))
}


###############################################################################@
#' Filter a [metaMDB] object
#' 
#' @param .data a [metaMDB] object
#' @param ... each argument should have the name of one of the tables of the
#' [metaMDB] object and contain a simple logical expression involving
#' the names of the corresponding table.
#' @param .preserve not used
#' 
#' @return a filtered [memoMDB] object
#' 
#' @export
#'
filter.metaMDB <- function(.data, ..., .preserve=FALSE){
   
   x <- .data
   
   ## Useful information ----
   oriRT <- relational_tables(x)
   rtNames <- names(oriRT)
   
   ## Filter each MDB ----
   dots <- rlang::enquos(...)
   fdt <- lapply(
      MDBs(x),
      function(y){
         mdbt <- intersect(names(dots), names(y))
         if(length(mdbt)>0){
            sdots <- dots[mdbt]
            toRet <- dplyr::filter(y, !!!sdots) %>% data_tables()
         }else{
            toRet <- NULL
         }
         return(toRet)
      }
   )
   names(fdt) <- NULL
   fdt <- do.call(c, fdt)
   mdbt <- intersect(names(dots), rtNames)
   if(length(mdbt)>0){
      toAdd <- memoMDB(
         dataTables=oriRT,
         dataModel=data_model(x)[rtNames, rmForeignKeys=TRUE],
         dbInfo=list(name="reltables"),
         check=FALSE
      ) %>%
         dplyr::filter(dots[mdbt]) %>% 
         data_tables()
      fdt <- c(fdt, toAdd)
   }
   
   ## Filter with tables
   return(filter_with_tables(x, fdt, checkTables=FALSE))
   
}


###############################################################################@
#' Subset a [metaMDB] object according to row position in one table
#' 
#' @param .data a [metaMDB] object
#' @param ... a single argument. The name of this argument should be a table
#' name of x and the value of this argument should be vector of integers
#' corresponding to row indexes.
#' @param .preserve not used
#' 
#' @return a [memoMDB] object
#' 
#' @export
#'
slice.metaMDB <- function(.data, ..., .preserve=FALSE){
   
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
#' @method filter_with_tables metaMDB
#' 
#' @export
#'
filter_with_tables.metaMDB <- function(x, tables, checkTables=TRUE, ...){
   
   ## Dump all the data in memory for filtering
   ##    ==> not efficient ==> dedicated usage
   filter_with_tables(as_memoMDB(x), tables, checkTables=checkTables, ...)
   
}


###############################################################################@
#' 
#' @rdname filter_mdb_matrix
#' @method filter_mdb_matrix metaMDB
#' 
#' @export
#'
filter_mdb_matrix.metaMDB <- function(x, tableName, ...){
   
   ## Checks ----
   stopifnot(
      is.metaMDB(x),
      tableName %in% names(x)
   )
   tableModel <- data_model(x)[[tableName]]
   stopifnot(ReDaMoR::is.MatrixModel(tableModel))
   
   ## Get relevant MDB ----
   mdbs <- MDBs(x)
   tmdb <- lapply(mdbs, function(y) tableName %in% names(y)) %>%
      unlist() %>% 
      which()
   return(filter_mdb_matrix(x=mdbs[[tmdb]], tableName=tableName, ...))
}


##############################################################################@
#' Get collections shared by 2 objects and return member combinations
#'
#' @param x an MDB object
#' @param y an MDB object
#' 
#' @return A tibble with the following fields:
#' - **collection** the name of the collection
#' - **mid.x** the collection member identifier in x
#' - **table.x** the table of the collection member in x
#' - **mid.y** the collection member identifier in y
#' - **table.y** the table of the collection member in y
#'
#' @export
#'
get_shared_collections <- function(x, y){
   xcm <- collection_members(x)
   ycm <- collection_members(y)
   return(dplyr::inner_join(
      dplyr::distinct(xcm, .data$collection, .data$table, .data$mid),
      dplyr::distinct(ycm, .data$collection, .data$table, .data$mid),
      by="collection"
   ))
}


###############################################################################@
## Helpers ----
.write_chTables.metaMDB <- function(x, con, dbName, by, ...){
   for(mdb in MDBs(x)){
      .write_chTables(mdb, con, dbName, by, ...)
   }
   .write_chTables(
      as_memoMDB(x[names(relational_tables(x))]),
      con, dbName, by, ...
   )
}

