###############################################################################@
#' MDB
#' 
#' The class "MDB" provides general functions for handling
#' modeled databases. 
#' The MDB classes implemented in the TKCat package
#' are: [fileMDB], [memoMDB], chMDB and [metaMDB].
#' These classes provide additional functions.
#' 
#' @seealso MDB methods:
#' [db_info], [data_model], [data_tables], [collection_members],
#' [count_records], [filter_with_tables], [as_fileMDB]
#' Additional documentation is provided for each specific class:
#' [fileMDB], [memoMDB], chMDB and [metaMDB].
#' 
#' @name MDB
#' 
NULL


###############################################################################@
#' Check if the object is  an MDB object
#' 
#' @param x any object
#' 
#' @return A single logical: TRUE if x is an MDB object.
#' 
#' @export
#'
is.MDB <- function(x){
   inherits(x, "MDB")
}


###############################################################################@
#'
#' @param x an MDB object
#'
#' @return  `names()` returns the table names.
#' 
#' @rdname MDB
#' 
#' @export
#'
names.MDB <- function(x){
   names(data_model(x))
}


###############################################################################@
#'
#' @param x an MDB object
#' 
#' @return `length()` returns the number of tables in x.
#' 
#' @rdname MDB
#' 
#' @export
#'
length.MDB <- function(x){
   length(data_model(x))
}


###############################################################################@
#'
#' @param x an MDB object
#' @param use.names return the names of the tables
#' 
#' @return `lengths()` returns the number of fields for each table in x.
#' 
#' @rdname MDB
#' 
#' @export
#'
lengths.MDB <- function(x, use.names=TRUE){
   lengths(data_model(x), use.names=use.names)
}


###############################################################################@
#' @export
#'
format.MDB <- function(x, ...){
   cm <- collection_members(x)
   dbi <- db_info(x)
   maintainer <- db_info(x)$maintainer
   return(sprintf(
      paste(
         "%s %s%s%s",
         "   - %s tables with %s fields",
         "",
         "%s",
         "",
         "%s",
         "%s",
         sep="\n"
      ),
      class(x)[1],
      db_info(x)$name,
      ifelse(
         (is.na(dbi$version) || dbi$version=="") &&
         (is.na(dbi$maintainer) || dbi$maintainer==""),
         "",
         sprintf(
            " (%s%s%s)",
            ifelse(
               is.na(dbi$version) || dbi$version=="", "",
               sprintf('version %s', dbi$version)
            ),
            ifelse(
               !is.na(dbi$version) && dbi$version!="" &&
               !is.na(dbi$maintainer) && dbi$maintainer!="",
               ', ', ''
            ),
            ifelse(
               is.na(dbi$maintainer) || dbi$maintainer=="", "",
               dbi$maintainer
            )
         )
      ),
      ifelse(
         is.na(dbi$title) || dbi$title=="",
         '',
         sprintf(': %s', dbi$title)
      ),
      length(x),
      sum(lengths(x)),
      if(!is.null(cm) && nrow(cm)>0){
         sprintf(
            "Collection members: \n%s",
            paste(
               unlist(lapply(
                  unique(cm$collection),
                  function(y){
                     return(sprintf(
                        "   - %s %s member%s",
                        length(unique(cm$table[which(cm$collection==y)])),
                        y,
                        ifelse(
                           length(unique(cm$table[which(cm$collection==y)]))>1,
                           "s", ""
                        )
                     ))
                  }
               )),
               collapse="\n"
            )
         )
      }else{
         "No collection member"
      },
      ifelse(
         is.na(dbi$description) || dbi$description=="",
         '',
         dbi$description
      ),
      ifelse(
         is.na(dbi$url) || dbi$url=="",
         '',
         sprintf('(%s)', dbi$url)
      )
   ))
}


###############################################################################@
#' @export
#'
print.MDB <- function(x, ...){
   cat(format(x, ...), "\n")
}


###############################################################################@
#' 
#' @param x an MDB object
#' @param ... additional parameters
#' 
#' @return `as.list.MDB()` returns a simple list of tibbles with all the
#' data from the tables in x.
#' 
#' @rdname MDB
#' 
#' @export
#'
as.list.MDB <- function(x, ...){
   data_tables(x, ...)
}


###############################################################################@
#' @export
#'
str.MDB <- function(object, ...){
   str(unclass(object))
}


###############################################################################@
#' 
#' @param .data an MDB object
#' @param ... additional parameters
#' 
#' @rdname MDB
#' 
#' @export
#'
select.MDB <- function(.data, ...){
   i <- tidyselect::eval_select(expr(c(...)), .data)
   .data[i]
}


###############################################################################@
#'
#' @param .data an MDB object
#' @param var a variable specified as in [dplyr::pull]
#' @param name not used but kept for compatibility with the generic function
#' @param ... additional parameters
#' 
#' @rdname MDB
#' 
#' @export
#'
pull.MDB <- function(.data, var=-1, name=NULL, ...){
   if(!is.null(name)){
      warning("name parameter not used by the pull.MDB function")
   }
   var <- tidyselect::vars_pull(names(.data), !!rlang::enquo(var))
   return(.data[[var]])
}

###############################################################################@
#' Compare two MDB objects
#' 
#' @param former an MDB object
#' @param new an MDB object
#' 
#' @return A tibble with 4 columns:
#' - **Information**: Compared information
#' - **Former**: value for the former object
#' - **New**: value for the new object
#' - **Identical**: a logical indicating if the 2 values are identical
#'
#' @export
#'
compare_MDB <- function(former, new){
   
   stopifnot(is.MDB(former), is.MDB(new))
   
   ## DB information ----
   fdbi <- db_info(former)
   ndbi <- db_info(new)
   dbif <- union(names(fdbi), names(ndbi))
   toRet <- dplyr::tibble(
      "Information"=dbif,
      "Former"=unlist(fdbi)[dbif],
      "New"=unlist(ndbi)[dbif]
   ) %>% mutate(
      "Identical"=.data$Former==.data$New
   )
   
   ## Data model ----
   toRet <- dplyr::bind_rows(
      toRet,
      dplyr::tibble(
         "Information"=c("Model", "Model display"),
         "Former"=c(sprintf("%s tables", length(former)), ""),
         "New"=c(sprintf("%s tables", length(new)), ""),
         "Identical"=c(
            ReDaMoR::identical_RelDataModel(
               data_model(former), data_model(new), includeDisplay=FALSE
            ),
            ReDaMoR::identical_RelDataModel(
               data_model(former), data_model(new), includeDisplay=TRUE
            )
         )
      )
   )
   
   ## Records ----
   fnr <- count_records(former)
   fnr <- fnr[sort(names(fnr))]
   nnr <- count_records(new)
   nnr <- nnr[sort(names(nnr))]
   if(
      length(fnr)==length(nnr) && length(fnr) > 0 &&
      all(names(fnr)==names(nnr))
   ){
      toRet <- dplyr::bind_rows(
         toRet,
         dplyr::tibble(
            "Information"=c(sprintf("Table %s", names(fnr)), "Total"),
            "Former"=format(c(fnr, sum(fnr)), big.mark=",", trim=FALSE),
            "New"=format(c(nnr, sum(nnr)), big.mark=",", trim=FALSE)
         ) %>% mutate(
            "Identical"=c(fnr, sum(fnr))==c(nnr, sum(nnr))
         )
      )
   }
   
   ## Collection members ----
   ccm <- collection_members(former)
   if(!is.null(ccm)){
      ccm <- ccm %>% dplyr::select(-"resource") %>% dplyr::arrange_all()
      ccoll <- ccm %>%
         dplyr::distinct(
            .data$collection, .data$table, .data$mid
         ) %>%
         nrow
   }else{
      ccoll <- 0
   }
   ncm <- collection_members(new)
   if(!is.null(ncm)){
      ncm <- ncm %>% dplyr::select(-"resource") %>% dplyr::arrange_all()
      ncoll <- ncm %>%
         dplyr::distinct(
            .data$collection, .data$table, .data$mid
         ) %>%
         nrow
   }else{
      ncoll <- 0
   }
   toRet <- dplyr::bind_rows(
      toRet,
      dplyr::tibble(
         "Information"="Collections",
         "Former"=format(ccoll, big.mark=",", trim=FALSE),
         "New"=format(ncoll, big.mark=",", trim=FALSE),
         "Identical"=(
            ccoll==ncoll && (
               ccoll==0 || identical(ccm, ncm)
            )
         )
      )
   )
   
   return(toRet)
}

###############################################################################@
#' @export
#'
'[<-.MDB' <- function(x, i, value){
   stop("'[<-' is not supported for .MDB")
}

###############################################################################@
#' @export
#'
'[[<-.MDB' <- function(x, i, value){
   stop("'[[<-' is not supported for .MDB")
}

###############################################################################@
#' @export
#'
'$<-.MDB' <- function(x, i, value){
   stop("'$<-' is not supported for .MDB")
}

##############################################################################@
#' Merge 2 MDBs
#' 
#' @param x an MDB object
#' @param y an MDB object
#' @param by a tibble as returned by the [get_shared_collections()] function
#' which indicates which collection members should be merged through
#' a relational table. If the collection is `NA`, the relational table is built
#' by merging identical columns in table.x and table.y. If the collection
#' is provided, the relational table is build using
#' the [map_collection_members()] function.
#' @param dbInfo a list with DB information:
#' **"name"** (only mandatory field), "title", "description", "url",
#' "version", "maintainer".
#' @param dmAutoLayout if TRUE (default) the layout of the merged data model
#' is automatically adjusted.
#' @param rtColor the color of the relational tables in the merged data model
#' (default: "yellow")
#' @param funs a named list of functions (default: list()). If there is
#' no function for mapping a collection in this list, it is taken
#' automatically using the [get_collection_mapper()] function.
#' @param ... additional parameters
#' 
#' @return `merge()` returns a [metaMDB] object gathering x and y along
#' with relational tables between them created using collection members
#' and mapping functions automatically chosen or provided by
#' the `funs` parameter. `...` can be used to send parameters to the mapper
#' functions.
#' 
#' @rdname MDB
#' 
#' @export
#' 
merge.MDB <- function(
   x, y,
   by=get_shared_collections(x, y),
   dbInfo=list(name=paste(db_info(x)$name, db_info(y)$name, sep="_")),
   dmAutoLayout=TRUE, rtColor="yellow",
   funs=list(),
   ...
){
   
   ## Checks ----
   stopifnot(
      is.MDB(x), is.MDB(y),
      is.data.frame(by),
      nrow(by)>0,
      all(
         c("collection", "mid.x", "table.x", "mid.y", "table.y") %in% 
            colnames(by)
      )
   )
   by <- dplyr::distinct(by)
   ic <- dplyr::setdiff(by, get_shared_collections(x, y))
   if(nrow(ic)>0){
      if(any(!is.na(ic$collection))){
         print(ic)
         stop("Cannot merge using the provided collection members")
      }
      mt <- setdiff(c(ic$table.x, ic$table.y), c(names(x), names(y)))
      if(length(mt)>0){
         stop(
            "The following tables do not exist: ",
            paste(mt, collapse=", ")
         )
      }
   }
   dbInfo <- .check_dbInfo(dbInfo)
   
   ## Building the relational tables ----
   by <- by %>% 
      dplyr::mutate(
         rt=ifelse(
            !is.na(.data$collection),
            paste(
               .data$collection,
               .data$mid.x, .data$table.x,
               .data$mid.y, .data$table.y,
               sep="_"
            ),
            paste(
               .data$table.x,
               .data$table.y,
               sep="_"
            )
         )
      )
   xcm <- collection_members(x)
   ycm <- collection_members(y)
   relationalTables <- list()
   dm <- c(data_model(x), data_model(y))
   fdm <- unclass(dm)
   for(i in 1:nrow(by)){
      byi <- by[i,]
      
      ## 
      if(is.na(byi$collection)){
         tmx <- dm[[byi$table.x]]$fields
         tmy <- dm[[byi$table.y]]$fields
         tmxy <- dplyr::intersect(
            dplyr::select(tmx, "name", "type"),
            dplyr::select(tmy, "name", "type")
         )
         if(nrow(tmxy)==0){
            stop(
               "There is no common field with the same type in",
               sprintf(
                  " the provided tables: %s and %s",
                  byi$table.x, byi$table.y
               )
            )
         }
         tmxy$nullable <- tmx$nullable[match(tmxy$name, tmx$name)] |
            tmy$nullable[match(tmxy$name, tmy$name)]
         tmxy$unique <- tmx$unique[match(tmxy$name, tmx$name)] &
            tmy$unique[match(tmxy$name, tmy$name)]
         tmxy$comment <- as.character(NA)
         
         relationalTables[[byi$rt]] <- dplyr::bind_rows(
            x[[byi$table.x]][,tmxy$name],
            y[[byi$table.y]][,tmxy$name],
         ) %>% 
            dplyr::distinct()
         
         tm <- ReDaMoR::RelTableModel(list(
            tableName=byi$rt,
            fields=tmxy,
            primaryKey=NULL,
            foreignKeys=list(
               list(
                  refTable=byi$table.x,
                  key=dplyr::tibble(
                     from=tmxy$name,
                     to=tmxy$name
                  ),
                  cardinality=c(fmin=0L, fmax=-1L, tmin=0L, tmax=-1L)
               ),
               list(
                  refTable=byi$table.y,
                  key=dplyr::tibble(
                     from=tmxy$name,
                     to=tmxy$name
                  ),
                  cardinality=c(fmin=0L, fmax=-1L, tmin=0L, tmax=-1L)
               )
            ),
            indexes=NULL,
            "display"=list(
               x=as.numeric(NA), y=as.numeric(NA),
               color=rtColor,
               comment=as.character(NA)
            )
         ))
         
      }
      
      else{
         
         xcmi <- xcm %>% 
            dplyr::filter(
               .data$collection==byi$collection,
               .data$mid==byi$mid.x,
               .data$table==byi$table.x
            ) %>% 
            dplyr::select("field", "static", "value", "type")
         ycmi <- ycm %>% 
            dplyr::filter(
               .data$collection==byi$collection,
               .data$mid==byi$mid.y,
               .data$table==byi$table.y
            ) %>% 
            dplyr::select("field", "static", "value", "type")
         
         dxcmi <- xcmi %>% 
            dplyr::filter(!.data$static) %>% 
            dplyr::select("name"="value") %>%
            dplyr::left_join(dm[[byi$table.x]]$fields, by="name") %>% 
            dplyr::rename("to"="name") %>% 
            dplyr::mutate(
               name=paste(.data$to, byi$table.x, sep="_"),
               nullable=TRUE, unique=FALSE,
               refTable=byi$table.x
            )
         dycmi <- ycmi %>% 
            dplyr::filter(!.data$static) %>% 
            dplyr::select("name"="value") %>%
            dplyr::left_join(dm[[byi$table.y]]$fields, by="name") %>% 
            dplyr::rename("to"="name") %>% 
            dplyr::mutate(
               name=paste(.data$to, byi$table.y, sep="_"),
               nullable=TRUE, unique=FALSE,
               refTable=byi$table.y
            )
         tm <- ReDaMoR::RelTableModel(list(
            tableName=byi$rt,
            fields=dplyr::bind_rows(
               select(dxcmi, "name", "type", "nullable", "unique", "comment"),
               select(dycmi, "name", "type", "nullable", "unique", "comment")
            ),
            primaryKey=NULL,
            foreignKeys=list(
               list(
                  refTable=byi$table.x,
                  key=dplyr::tibble(
                     from=dxcmi$name,
                     to=dxcmi$to
                  ),
                  cardinality=c(fmin=0L, fmax=-1L, tmin=0L, tmax=-1L)
               ),
               list(
                  refTable=byi$table.y,
                  key=dplyr::tibble(
                     from=dycmi$name,
                     to=dycmi$to
                  ),
                  cardinality=c(fmin=0L, fmax=-1L, tmin=0L, tmax=-1L)
               )
            ),
            indexes=NULL,
            "display"=list(
               x=as.numeric(NA), y=as.numeric(NA),
               color=rtColor,
               comment=as.character(NA)
            )
         ))
         
         ## The relational table
         nrt <- map_collection_members(
            x=x[[byi$table.x]],
            y=y[[byi$table.y]],
            collection=byi$collection,
            xm=xcmi,
            ym=ycmi,
            suffix=paste0("_", c(byi$table.x, byi$table.y)),
            fun=if(
               is.null(funs[[byi$collection]]) || is.na(funs[[byi$collection]])
            ){
               NA
            }else{
               funs[[byi$collection]]
            },
            ...
         )
         for(cn in colnames(nrt)){
            nrt[,cn] <- as_type(
               dplyr::pull(nrt, !!cn),
               tm$fields$type[which(tm$fields$name==cn)]
            )
         }
         relationalTables[[byi$rt]] <- nrt
         
      }
      
      fdm[[byi$rt]] <- tm
   }
   dm <- ReDaMoR::RelDataModel(fdm)
   
   
   ## Final object ----
   if(dmAutoLayout){
      dm <- ReDaMoR::auto_layout(dm)
   }
   
   # return(dm)
   return(metaMDB(
      MDBs=list(x, y) %>%
         magrittr::set_names(c(db_info(x)$name, db_info(y)$name)),
      relationalTables=relationalTables,
      dataModel=dm,
      dbInfo=dbInfo
   ))
   
}


###############################################################################@
#' Get the last generated MDB confrontation report
#' 
#' @return A confrontation report generated by [ReDaMoR::confront_data()]
#' 
#' @export
#'
get_confrontation_report <- function(){
   return(tkcatEnv$confrontationReport)
}


###############################################################################@
## Helpers ----
.check_dbInfo <- function(dbInfo){
   mandFields <- c(
      "name"
   )
   for(f in mandFields){
      if(
         !is.character(dbInfo[[f]]) || length(dbInfo[[f]])!=1 ||
         is.na(dbInfo[[f]]) || dbInfo[[f]]==""
      ){
         stop(sprintf(
            "%s in dbInfo should be a non-empty character"
         ))
      }
   }
   optfields <- c(
      "title", "description", "url",
      "version", "maintainer"
   )
   for(f in optfields){
      fv <- dbInfo[[f]]
      if(length(fv)==0){
         dbInfo[[f]] <- as.character(NA)
      }else{
         if(length(fv)>1 || !is.atomic(fv)){
            stop(sprintf("Invalid value for %s", f))
         }
         dbInfo[[f]] <- as.character(fv)
      }
   }
   dbInfo <- as.list(dbInfo[c(mandFields, optfields)])
   return(dbInfo)
}

.writeDescription <- function(x, file){
   toWrite <- jsonlite::toJSON(lapply(x, jsonlite::unbox), pretty=TRUE)
   writeLines(toWrite, file)
}
