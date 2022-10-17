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
#' Check if the object is an [MDB] object
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
#' @rdname count_records
#' @method count_records MDB
#' 
#' @export
#'
count_records.MDB <- function(x, ...){
   d <- dims(x, ...)
   d$records %>% magrittr::set_names(d$name)
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
         "%s",
         "",
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
      ),
      ifelse(
         is.na(dbi$timestamp),
         '',
         sprintf('\nTimesamp: %s', dbi$timestamp)
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
   utils::str(unclass(object))
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
   i <- tidyselect::eval_select(rlang::expr(c(...)), .data)
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
#' Add a collection member to an MDB
#' 
#' @param x an [MDB] object
#' @param collection a collection title in [list_local_collections()]
#' @param table the table providing the collection member
#' @param ... definition of the collection fields as lists
#' (e.g. `be=list(static=TRUE, value="Gene")`
#' or
#' `organism=list(static=TRUE, value="Homo sapiens", type="Scientific name")`
#' )
#' 
#' @export
#' 
add_collection_member <- function(
   x,
   collection,
   table,
   ...
){
   fd <- list(...)
   stopifnot(
      is.MDB(x),
      is.character(collection), length(collection)==1, !is.na(collection),
      collection %in% list_local_collections()$title,
      is.character(table), length(table)==1, !is.na(table),
      table %in% names(x),
      length(fd) > 0, length(names(fd))==length(fd),
      all(unlist(lapply(
         fd, function(y){
            all(names(y) %in% c("static", "value", "type")) &&
               all(c("value", "static") %in% names(y))
         }
      )))
   )
   resource <- db_info(x)$name
   cm <- collection_members(x)
   if(!is.null(cm) && nrow(cm)>0){
      if(collection %in% cm$collection){
         cid <- cm$cid[which(cm$collection==collection)][1]
         mid <- max(cm$mid[which(cm$collection==collection)])+1
      }else{
         cid <- paste(resource, collection, "1.0", sep="_")
         mid <- 1
      }
   }else{
      cid <- paste(resource, collection, "1.0", sep="_")
      mid <- 1
   }
   toAdd <- c()
   for(field in names(fd)){
      def <- fd[[field]]
      toAdd <- rbind(
         toAdd,
         dplyr::tibble(
            field=field, static=def[["static"]], value=def[["value"]],
            type=ifelse(
               length(def[["type"]])!=1,
               as.character(NA),
               def[["type"]]
            )
         )
      )
   }
   toAdd$collection <- collection
   toAdd$cid <- cid
   toAdd$resource <- resource
   toAdd$mid <- as.integer(mid)
   toAdd$table <- table
   toAdd <- dplyr::select(
      toAdd,
      "collection", "cid", "resource", "mid", "table",
      "field", "static", "value", "type"
   )
   if(!is.null(cm)){
      toAddf <- dplyr::anti_join(
         toAdd, cm,
         by=c("collection", "table", "field", "value")
      )
      if(nrow(toAddf)==0){
         warning(
            "This member is already recorded: it won't be added nor modified"
         )
         toAdd <- NULL
      }
   }
   cm <- rbind(
      cm,
      toAdd
   )
   toRet <- x
   collection_members(toRet) <- cm
   return(toRet)
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
   ) %>% dplyr::mutate(
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
         ) %>% dplyr::mutate(
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
   stop("'[<-' is not supported for MDB")
}

###############################################################################@
#' @export
#'
'[[<-.MDB' <- function(x, i, value){
   stop("'[[<-' is not supported for MDB")
}

###############################################################################@
#' @export
#'
'$<-.MDB' <- function(x, i, value){
   stop("'$<-' is not supported for MDB")
}


###############################################################################@
#'
#' @param ... [MDB] objects
#'
#' @rdname MDB
#' 
#' @export
#'
c.MDB <- function(...){
   MDBs <- list(...)
   dbNames <- unlist(lapply(
      MDBs, function(x) db_info(x)$name
   ))
   if(any(duplicated(dbNames))){
      "MDBs to combine cannot have the same names"
   }
   names(MDBs) <- dbNames
   dbModels <- lapply(MDBs, data_model) %>% 
      magrittr::set_names(NULL)
   dataModel <- do.call(c, dbModels)
   return(metaMDB(
      MDBs=MDBs,
      relationalTables=NULL,
      dataModel=dataModel,
      dbInfo=db_info(MDBs[[1]]),
      check=FALSE
   ))
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
#' @return A [metaMDB] object gathering x and y along
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
         
         tm <- ReDaMoR::RelTableModel(
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
         )
         
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
         tm <- ReDaMoR::RelTableModel(
            tableName=byi$rt,
            fields=dplyr::bind_rows(
               dplyr::select(
                  dxcmi, "name", "type", "nullable", "unique", "comment"
               ),
               dplyr::select(
                  dycmi, "name", "type", "nullable", "unique", "comment"
               )
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
         )
         
         ## The relational table
         nrt <- map_collection_members(
            x=x[[byi$table.x]],
            y=y[[byi$table.y]],
            collection=byi$collection,
            xm=xcmi,
            ym=ycmi,
            suffix=paste0("_", c(byi$table.x, byi$table.y)),
            fun=if(
               is.null(funs[[byi$collection]]) ||
               !is.function(funs[[byi$collection]])
            ){
               NA
            }else{
               funs[[byi$collection]]
            },
            ...
         )
         
         cc <- unlist(lapply(nrt, function(x) class(x)[1]))
         fcc <- dplyr::mutate(tm$fields, cc=cc[.data$name])
         wc <- dplyr::filter(fcc, .data$type!=.data$cc) %>% dplyr::pull("name")
         
         for(cn in wc){
            nrt[,cn] <- ReDaMoR::as_type(
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
   
   return(metaMDB(
      MDBs=list(x, y) %>%
         magrittr::set_names(c(db_info(x)$name, db_info(y)$name)),
      relationalTables=relationalTables,
      dataModel=dm,
      dbInfo=dbInfo
   ))
   
}


##############################################################################@
#' Join connected tables 
#' 
#' @param x an MDB object
#' @param ... at least 2 names of tables to join
#' @param type the type of join among:
#' - `"left"`: includes all rows of the first provided table
#' - `"right"`: includes all rows of the last provided table
#' - `"inner"`: includes all rows in all provided tables
#' - `"full"`: includes all rows in at least one provide table
#' @param jtName the name of the joint. IF NA (default), the name
#' is then the name is the first provided table name.
#' 
#' @return A [metaMDB] corresponding to x with the
#' joined tables replaced by the joint.
#' If less than 2 table names are provided, the function returns
#' the original x MDB.
#' 
#' @export
#' 
join_mdb_tables <- function(
   x,
   ...,
   type=c("left", "right", "inner", "full"),
   jtName=NA
){
   stopifnot(
      is.MDB(x)
   )
   ttj <- as.character(c(...)) # tables to join
   if(any(duplicated(ttj))){
      dupttj <- unique(ttj[which(duplicated(ttj))])
      warning(sprintf(
         "Table%s '%s' %s going to be joined several times",
         ifelse(length(dupttj)>1, "s", ""),
         paste(dupttj, collapse="', '"),
         ifelse(length(dupttj)>1, "are", "is")
      ))
   }
   if(!all(ttj %in% names(x))){
      stop("Tables not found: ", setdiff(ttj, names(x)))
   }
   type <- match.arg(type)
   jfun <-
      if(type=="left"){ dplyr::left_join
      }else if(type=="right"){ dplyr::right_join
      }else if(type=="inner"){ dplyr::inner_join
      }else if(type=="full"){ dplyr::full_join
      }

   ## Simple case ----
   if(length(ttj)<2){
      return(x)
   }
   
   ## Join table name ----
   if(is.na(jtName)){
      jtName <- ttj[1]
   }
   if(jtName %in% setdiff(names(x), ttj)){
      stop("Wrong jtName: this name will still be used after the join.")
   }
   techname <- paste0("T", sample.int(10^6, 1), "T")
   while(techname %in% names(x)){
      techname <- paste0("T", sample.int(10^6, 1), "T")
   }
   
   ## Join the 2 first tables ----
   
   ## _+ Subset tables ----
   toJoin <- ttj[1:2]
   ttj <- ttj[-(1:2)]
   rx <- x[union(setdiff(names(x), toJoin), ttj)]
   
   ## _+ Check foreign keys ----
   dm <- data_model(x)
   fk <- ReDaMoR::get_foreign_keys(dm)
   fkt <- dplyr::bind_rows(
      dplyr::filter(
         fk,
         .data$from==!!toJoin[1] & .data$to==!!toJoin[2]
      ),
      dplyr::filter(
         fk,
         .data$from==!!toJoin[2] & .data$to==!!toJoin[1]
      ) %>%
         dplyr::rename(
            "to"="from", "tf"="ff", "from"="to", "ff"="tf",
            "tmin"="fmin", "tmax"="fmax", "fmin"="tmin", "fmax"="tmax"
         )
   )
   if(nrow(fkt)==0){
      stop(sprintf("There is no relationship with %s", toJoin[2]))
   }
   
   ## _+ Set suffix for ambiguous fields ----
   if(nrow(fkt)==1){
      fkt$suffix <- ""
   }else{
      ff <- unlist(lapply(fkt$ff, function(x) paste(sort(x), collapse="_")))
      if(!any(duplicated(ff))){
         fkt$suffix <- ff
      }else{
         tf <- unlist(lapply(fkt$tf, function(x) paste(sort(x), collapse="_")))
         if(!any(duplicated(tf))){
            fkt$suffix <- tf
         }else{
            fftf <- paste(ff, tf, sep="__")
            if(!any(duplicated(fftf))){
               fkt$suffix <- fftf
            }else{
               fkt$suffix <- paste(fftf, 1:length(fftf), sep="__")
            }
         }
      }
   }
   
   ## _+ Initiate the new data model ----
   jt <- x[[toJoin[1]]]
   jdm <- data_model(rx)
   toAdd <- dm[toJoin[1], rmForeignKeys=TRUE][[1]]
   toAdd$tableName <- techname
   toAdd$fields <- toAdd$fields %>% 
      dplyr::mutate(
         nullable=TRUE,
         unique=FALSE
      )
   toAdd$primaryKey <- character()
   toAdd$indexes <- list()
   toAdd <- list(toAdd) %>% magrittr::set_names(techname)
   toAdd <- ReDaMoR::RelDataModel(toAdd)
   jdm <- c(jdm, toAdd)
   fkToAdd <- dplyr::bind_rows(
      dplyr::filter(
         fk,
         .data$from==!!toJoin[1] & .data$to %in% names(rx)
      ) %>% 
         dplyr::mutate(from=techname, fmin=0L, fmax=-1L),
      dplyr::filter(
         fk,
         .data$from %in% names(rx) & .data$to==!!toJoin[1]
      ) %>% 
         dplyr::mutate(to=techname, tmin=0L, tmax=-1L)
   )
   if(nrow(fkToAdd)>0) for(j in 1:nrow(fkToAdd)){
      fkj <- dplyr::slice(fkToAdd, j)
      jdm <- ReDaMoR::add_foreign_key(
         jdm,
         fromTable=dplyr::pull(fkj, "from"),
         fromFields=dplyr::pull(fkj, "ff")[[1]],
         toTable=dplyr::pull(fkj, "to"),
         toFields=dplyr::pull(fkj, "tf")[[1]],
         fmin=dplyr::pull(fkj, "fmin"),
         fmax=dplyr::pull(fkj, "fmax"),
         tmin=dplyr::pull(fkj, "tmin"),
         tmax=dplyr::pull(fkj, "tmax")
      )
   }
   
   ## _+ Join the tables ----
   t2 <- x[[toJoin[2]]]
   for(i in 1:nrow(fkt)){
      t2i <- t2
      cn <- setdiff(colnames(t2i), fkt$tf[[i]])
      if(fkt$suffix[[i]]==""){
         nn <- cn
      }else{
         nn <- paste(cn, fkt$suffix[[i]], sep=".")
      }
      if(any(nn %in% colnames(jt))){
         nn <- paste(toJoin[2], nn, sep=".")
      }
      cn <- c(cn, fkt$tf[[i]])
      nn <- c(nn, fkt$ff[[i]])
      rnv <- cn %>% magrittr::set_names(nn)
      t2i <- dplyr::rename(t2i, dplyr::all_of(rnv))
      jt <- jfun(jt, t2i, by=fkt$ff[[i]])
      
      rnv <- nn %>% magrittr::set_names(cn)
      toAdd <- dm[[toJoin[2]]]$fields %>% 
         dplyr::mutate(
            name=as.character(rnv[.data$name]),
            nullable=TRUE,
            unique=FALSE
         ) %>% 
         dplyr::filter(!.data$name %in% !!jdm[[techname]]$fields$name)
      for(j in 1:nrow(toAdd)){
         toAddj <- dplyr::slice(toAdd, j)
         jdm <- ReDaMoR::add_field(
            jdm,
            tableName=techname,
            name=dplyr::pull(toAddj, "name"),
            type=dplyr::pull(toAddj, "type"),
            nullable=dplyr::pull(toAddj, "nullable"),
            unique=dplyr::pull(toAddj, "unique"),
            comment=dplyr::pull(toAddj, "comment")
         )
      }
      
      fkToAdd <- dplyr::bind_rows(
         dplyr::filter(
            fk,
            .data$from==!!toJoin[2] & .data$to %in% names(rx)
         ) %>% 
            dplyr::mutate(
               from=techname, fmin=0L, fmax=-1L,
               tmin=0L, tmax=-1L,
               ff=lapply(.data$ff, function(f) as.character(rnv[f]))
            ),
         dplyr::filter(
            fk,
            .data$from %in% names(rx) & .data$to==!!toJoin[2]
         ) %>% 
            dplyr::mutate(
               to=techname, tmin=0L, tmax=-1L,
               fmin=0L, fmax=-1L,
               tf=lapply(.data$tf, function(f) as.character(rnv[f]))
            )
      )
      if(nrow(fkToAdd)>0) for(j in 1:nrow(fkToAdd)){
         fkj <- dplyr::slice(fkToAdd, j)
         jdm <- ReDaMoR::add_foreign_key(
            jdm,
            fromTable=dplyr::pull(fkj, "from"),
            fromFields=dplyr::pull(fkj, "ff")[[1]],
            toTable=dplyr::pull(fkj, "to"),
            toFields=dplyr::pull(fkj, "tf")[[1]],
            fmin=dplyr::pull(fkj, "fmin"),
            fmax=dplyr::pull(fkj, "fmax"),
            tmin=dplyr::pull(fkj, "tmin"),
            tmax=dplyr::pull(fkj, "tmax")
         )
      }
   }
   
   ## metaMDB object ----
   MDBs <- list(rx) %>% magrittr::set_names(db_info(rx)$name)
   rt <- list(jt) %>% magrittr::set_names(techname)
   toRet <- metaMDB(
      MDBs=MDBs,
      relationalTables=rt,
      dataModel=jdm,
      dbInfo=db_info(x)
   )
   
   ## Recursion ----
   ttj <- c(techname, ttj)
   toRet <- join_mdb_tables(
      x=toRet,
      ttj,
      type=type,
      jtName=techname
   ) %>% 
      dplyr::rename(dplyr::all_of(magrittr::set_names(techname,jtName)))
   return(toRet)
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
      "title"="character", "description"="character", "url"="character",
      "version"="character", "maintainer"="character",
      "timestamp"="POSIXct"
   )
   for(f in names(optfields)){
      fv <- dbInfo[[f]]
      if(length(fv)==0){
         if(optfields[f]=="character"){
            dbInfo[[f]] <- as.character(NA)
         }
         if(optfields[f]=="POSIXct"){
            dbInfo[[f]] <- as.POSIXct(NA)
         }
      }else{
         if(length(fv)>1 || !is.atomic(fv)){
            stop(sprintf("Invalid value for %s", f))
         }
         if(optfields[f]=="character"){
            dbInfo[[f]] <- as.character(fv)
         }
         if(optfields[f]=="POSIXct"){
            dbInfo[[f]] <- as.POSIXct(fv)
         }
      }
   }
   dbInfo <- as.list(dbInfo[c(mandFields, names(optfields))])
   return(dbInfo)
}

.writeDescription <- function(x, file){
   toWrite <- jsonlite::toJSON(lapply(x, jsonlite::unbox), pretty=TRUE)
   writeLines(toWrite, file)
}
