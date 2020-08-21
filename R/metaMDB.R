###############################################################################@
# @example inst/examples/metaMDB-examples.R
#
#' Create a metaMDB object
#' 
#' A metaMDB object is an MDB gathering several other MDBs glued by
#' relational tables.
#'
#' @param MDBs a list of MDB objects
#' @param relationalTables a list of tibbles corresponding to the relational
#' tables between the different MDBs
#' @param dataModel a [ReDaMoR::RelDataModel] object gathering all the
#' data model of all the MDBs plus the relational tables
#' @param dbInfo a list with DB information:
#' **"name"** (only mandatory field), "title", "description", "url",
#' "version", "maintainer".
#'
#' @return A metaMDB object
#' 
#'
#' @export
#'
metaMDB <- function(
   MDBs,
   relationalTables,
   dataModel,
   dbInfo
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
   for(mdb in names(MDBs)){
      if(
         !ReDaMoR::identical_RelDataModel(
            data_model(MDBs[[mdb]]),
            dataModel[names(MDBs[[mdb]])],
            includeDisplay=FALSE
         )
      ){
         stop("Incompatible datamodels")
      }
   }
   
   ## Relational tables
   atn <- c(unlist(lapply(MDBs, names)), names(relationalTables))
   if(!all(names(dataModel) %in% atn)){
      stop("Inconsistent data model")
   }
   cr <- ReDaMoR::confront_data(
      dataModel[names(relationalTables)],
      data=relationalTables,
      verbose=FALSE,
      returnData=FALSE
   )
   assign("confrontationReport", cr, envir=tkcatEnv)
   if(!cr$success){
      stop(ReDaMoR::format_confrontation_report(cr, title="Relational tables"))
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
#' Check the object is  a [metaMDB] object
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
#' @export
#'
'names<-.metaMDB' <- function(x, value){
   stopifnot(
      length(value)!=length(x),
      sum(duplicated(value)>0)
   )
   x <- unclass(x)
   toupdate <- value
   for(mdb in names(x$MDBs)){
      nt <- length(x$MDBs[[mdb]])
      if(nt>0){
         names(x$MDBs[[mdb]]) <- toupdate[1:nt]
         toupdate <- toupdate[-c(1:nt)]
      }
   }
   if(length(x$relationalTables)>0){
      names(x$relationalTables) <- toupdate
   }
   class(x) <- c("metaMDB", class(x))
   return(x)
}


###############################################################################@
#' @export
#' 
rename.metaMDB <- function(.data, ...){
   loc <- tidyselect::eval_rename(expr(c(...)), .data)
   names <- names(.data)
   names[loc] <- names(loc)
   magrittr::set_names(.data, names)
}


###############################################################################@
#' @param x a [metaMDB] object
#' @param ... not used
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
#' @param x a [metaMDB] object
#' @param value a list with DB information:
#' "name", "title", "description", "url",
#' "version", "maintainer".
#' 
#' @rdname db_info-set
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
#' @param x a [metaMDB] object
#' @param ... not used
#' 
#' @rdname data_model
#' @method data_model metaMDB
#' 
#' @export
#'
data_model.metaMDB <- function(x, ...){
   unclass(x)$dataModel
}


###############################################################################@
#' @param x a [metaMDB] object
#' @param ... names of the collections
#' to focus on. By default, all of them are taken.
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
#' @param x a [metaMDB] object
#' @param ... the name of the tables to get (default: all of them)
#' 
#' @rdname data_tables
#' @method data_tables metaMDB
#' 
#' @export
#'
data_tables.metaMDB <- function(x, ...){
   m <- data_model(x)
   toTake <- tidyselect::eval_select(expr(c(...)), x)
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
         toRet <- c(toRet, data_tables(x$MDBs[[mdb]], dplyr::all_of(lToTake)))
      }
   }
   lToTake <- intersect(toTake, names(x$relationalTables))
   if(length(lToTake)>0){
      toRet <- c(toRet, x$relationalTables[[lToTake]])
   }
   toRet <- toRet[toTake]
   return(toRet)
}


###############################################################################@
#' @param x a [metaMDB]
#' @param ... the name of the tables to consider (default: all of them)
#' 
#' @rdname count_records
#' @method count_records metaMDB
#' 
#' @export
#'
count_records.metaMDB <- function(x, ...){
   lapply(data_tables(x, ...), nrow) %>% unlist()
}


###############################################################################@
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
   
   mdbs <- MDBs(x)
   fmdbs <- list()
   for(mdb in names(mdbs)){
      lToTake <- intersect(i, names(mdbs[[mdb]]))
      if(length(lToTake)>0){
         fmdbs[[mdb]] <- mdbs[[mdb]][lToTake]
      }
   }
   lToTake <- intersect(i, names(unclass(x)$relationalTables))
   frt <- unclass(x)$relationalTables[lToTake]
   
   toRet <- metaMDB(
      MDBs=fmdbs,
      relationalTables=frt,
      dataModel=dm,
      dbInfo=dbi
   )
   return(toRet)
}


###############################################################################@
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
         return(data_tables(x, i)[[1]])
      }
   }
}
#' @export
'$.metaMDB' <- `[[.metaMDB`


###############################################################################@
#' @export
#'
c.metaMDB <- function(...){
   stop("c() not availble for metaMDB objects. Use join_mdb() instead.")
}


###############################################################################@
#' Filter [metaMDB] object according to provided tables
#' 
#' @param x a [metaMDB] object
#' @param tables a named list of tibbles to filter with. The names should
#' correspond to the table names in x and the tibbles should fit the
#' data model.
#' @param checkTables if TRUE, the tables are confronted to their model
#' in the data model of x.
#' 
#' @return a [metaMDB] object
#' 
#' @export
#'
filter_with_tables.metaMDB <- function(x, tables, checkTables=TRUE){
   
   ## Check the tables ----
   if(checkTables){
      for(tn in names(tables)){
         cr <- ReDaMoR::confront_table_data(data_model(x)[[tn]], tables[[tn]])
         if(!cr$sucess){
            stop(sprintf("The %s table does not fit the data model"), tn)
         }
      }
   }
   
   ## Useful information ----
   oriRT <- unclass(x)$relationalTables
   rtNames <- names(oriRT)
   
   ## Filter the MDBs ----
   fmdbs <- lapply(
      MDBs(x),
      function(y){
         mdbt <- intersect(names(tables), names(y))
         if(length(mdbt)>0){
            toRet <- filter_with_tables(y, tables[mdbt], checkTables=FALSE)
         }else{
            toRet <- NULL
         }
         return(toRet)
      }
   )
   fmdbs <- fmdbs[which(unlist(lapply(fmdbs, is.MDB)))]
   
   ## Get relevant relational tables ----
   dm <- data_model(x)
   fk <- ReDaMoR::get_foreign_keys(dm)
   toTake <- fk %>%
      dplyr::filter(
         from %in% rtNames
      ) %>%
      dplyr::pull(to) %>% 
      c(
         fk %>%
            dplyr::filter(
               to %in% rtNames
            ) %>%
            dplyr::pull(from)
      ) %>% 
      intersect(unlist(lapply(fmdbs, names))) %>% 
      union(intersect(names(tables), rtNames))
   
   ## No relational table to filter on ----
   if(length(toTake)==0){
      return(metaMDB(
         MDBs=fmdbs,
         relationalTables=list(),
         dataModel=do.call(c, lapply(fmdbs, data_model)),
         dbInfo=db_info(x)
      ))
   }
   
   ## Filter relational tables ----
   frdb <- c(
      oriRT,
      data_tables(x, intersect(names(x), toTake))
   )
   frdb <- memoMDB(
      dataTables=frdb,
      dataModel=data_model(x)[names(frdb), rmForeignKeys=TRUE],
      dbInfo=list(name="reltables"),
      checks=c()
   )
   frdb <- filter_with_tables(
      x=frdb,
      tables=c(
         tables[intersect(toTake, rtNames)],
         do.call(c, lapply(
            fmdbs, function(y) data_tables(y, intersect(names(y), toTake))
         ))
      )
   )
   
   ## Propagate filter -----
   tables <- data_tables(
      frdb,
      intersect(names(frdb), unlist(lapply(MDBs(x), names)))
   )
   tables <- c(
      tables,
      do.call(c, lapply(fmdbs, function(y){
         data_tables(y, setdiff(names(y), names(tables)))
      }))
   )
   fmdbs <- lapply(
      MDBs(x),
      function(y){
         mdbt <- intersect(names(tables), names(y))
         if(length(mdbt)>0){
            toRet <- filter_with_tables(y, tables[mdbt], checkTables=FALSE)
         }else{
            toRet <- NULL
         }
         return(toRet)
      }
   )
   fmdbs <- fmdbs[which(unlist(lapply(fmdbs, is.MDB)))]
   
   ## Final filter of relational tables ----
   toTake <- fk %>%
      dplyr::filter(
         from %in% rtNames
      ) %>%
      dplyr::pull(to) %>% 
      c(
         fk %>%
            dplyr::filter(
               to %in% rtNames
            ) %>%
            dplyr::pull(from)
      ) %>% 
      intersect(unlist(lapply(fmdbs, names)))
   frdb <- c(
      oriRT,
      data_tables(x, intersect(names(x), toTake))
   )
   frdb <- memoMDB(
      dataTables=frdb,
      dataModel=data_model(x)[names(frdb), rmForeignKeys=TRUE],
      dbInfo=list(name="reltables"),
      checks=c()
   )
   frdb <- filter_with_tables(
      x=frdb,
      tables=do.call(c, lapply(
         fmdbs, function(y) data_tables(y, intersect(names(y), toTake))
      ))
   )
   
   ## Final object ----
   toRet <- metaMDB(
      MDBs=fmdbs,
      relationalTables=data_tables(
         frdb,
         setdiff(names(frdb), unlist(lapply(MDBs(x), names)))
      ),
      dataModel=do.call(
         c,
         data_model(x)[union(unlist(lapply(fmdbs, names)), names(frdb))]
      ),
      dbInfo=db_info(x)
   )
   return(toRet)
   
}
