###############################################################################@
#' Add empty KM specification tables to an [MDB] object
#' 
#' @param x an [MDB] object to update with specification tables
#' @param kmr an [MDB] object with KM requirements
#'
#' @return An [MDB] object with empty KM specification tables
#'
#' @export
#'
add_km_spec <- function(x, kmr){
   
   stopifnot(is.MDB(x), is.KMR(kmr))
   
   dm <- KMSPEC_DM
   kmn <- db_info(kmr)$name
   names(dm) <- sprintf(names(dm), kmn)
   if(any(names(dm) %in% names(x))){
      stop(sprintf(
         "%s specification are already documented in this MDB", 
         kmn
      ))
   }
   
   ## Adapt display of spec. data model ----
   xpos <- do.call(
      rbind,
      lapply(data_model(x), function(tm) unlist(tm$display[c("x", "y")]))
   )
   xmin <- min(xpos[,"x"])
   xmax <- max(xpos[,"x"])
   ymin <- min(xpos[,"y"])
   ymax <- max(xpos[,"y"])
   ##
   dmpos <- xpos <- do.call(
      rbind,
      lapply(dm, function(tm) unlist(tm$display[c("x", "y")]))
   )
   dmxmin <- min(xpos[,"x"])
   dmymin <- min(xpos[,"y"])
   xshift <- xmax - dmxmin + ((xmax-xmin)/10)
   yshift <- ymax - dmymin + ((ymax-ymin)/10)
   ##
   dm <- unclass(dm)
   for(tn in names(dm)){
      dm[[tn]]$display$x <- dm[[tn]]$display$x + xshift
      dm[[tn]]$display$y <- dm[[tn]]$display$y + yshift
      dm[[tn]]$display$color <- "white"
   }
   dm <- ReDaMoR::RelDataModel(dm, checkFK=FALSE)

   
   ## Spec. tables ----
   dtl <- lapply(dm, function(tm){
      rtypes <- tm$fields$type
      names(rtypes) <- tm$fields$name
      toRet <- dplyr::tibble()
      for(i in 1:nrow(tm$fields)){
         toAdd <- integer()
         if(tm$fields$type[i]=="base64"){
            class(toAdd) <- "character"
         }else{
            class(toAdd) <- tm$fields$type[i]
         }
         toRet[,tm$fields$name[i]] <- toAdd
      }
      return(toRet)
   })
   
   ## Combined MDBs ----
   return(c(
      x,
      memoMDB(
         dataTables=dtl,
         dataModel=dm,
         dbInfo=list(
            name="spec"
         ),
         check=FALSE
      )
   ))
   
}

###############################################################################@
#' Check if KM specifications are available in an [MDB] object
#' 
#' @param x an [MDB] object with specification tables
#' @param kmr an [MDB] object with KM requirements
#'
#' @return A logical: TRUE if the MDB has KM specifications
#'
#' @export
#'
has_km_spec  <- function(x, kmr){
   stopifnot(
      is.MDB(x), is.KMR(kmr)
   )
   dm <- KMSPEC_DM
   kmn <- db_info(kmr)$name
   names(dm) <- sprintf(names(dm), kmn)
   return(all(names(dm) %in% names(x)))
}

###############################################################################@
#' Get KM specifications from an [MDB] object
#' 
#' @param x an [MDB] object with specification tables
#' @param kmr an [MDB] object with KM requirements
#'
#' @return An [MDB] object with kmr specification tables from x
#'
#' @export
#'
get_km_spec  <- function(x, kmr){
   stopifnot(
      is.MDB(x), is.KMR(kmr)
   )
   dm <- KMSPEC_DM
   kmn <- db_info(kmr)$name
   names(dm) <- sprintf(names(dm), kmn)
   if(!all(names(dm) %in% names(x))){
      stop(sprintf(
         "%s specification are not available in this MDB", 
         kmn
      ))
   }
   toRet <- as_memoMDB(x[names(dm)], check=FALSE)
   db_info(toRet)$name <- sprintf(
      "%s specification for %s",
      kmn, db_info(x)$name
   )
   return(toRet)
}

###############################################################################@
#' Add KM table specifications to an [MDB] object
#' 
#' @param x an [MDB] object to update with specification tables
#' @param kmr an [MDB] object with KM requirements
#' @param name the name of an existing table in x
#' @param type the name of an existing table type in kmr
#' @param features a list of feature definitions. Each element of a list
#' is a list of parameters for the [add_km_feature()] function.
#'
#' @return An [MDB] object with additional KM table specification
#'
#' @export
#'
add_km_table <- function(x, kmr, name, type, features=list()){
   stopifnot(
      is.MDB(x), is.KMR(kmr),
      name %in% names(x),
      type %in% kmr$Tables$name
   )
   
   ## Splitting information ----
   kms <- get_km_spec(x, kmr)
   toRet <- x[setdiff(names(x), names(kms))]
   kmson <- names(kms)
   kmssn <- sub(
      "___.*-Tables___", "Tables",
      sub(
         "___.*-Features___", "Features",
         sub(
            "___.*-Helpers___", "Helpers",
            kmson
         )
      )
   )
   names(kms) <- kmssn[match(names(kms), kmson)]
   
   ## Check if not existing ----
   tables <- kms$Tables
   features_sp <- kms$Features
   if(name %in% kms$Tables$name){
      stop(sprintf(
         "The %s table is already documented",
         name
      ))
   }
   ## Check collection ----
   tc <- dplyr::filter(
      kmr$Tables, .data$name==!!type
   ) %>% 
      dplyr::pull("collection")
   if(!is.na(tc)){
      dbc <- collection_members(toRet)
      if(is.null(dbc)){
         stop(sprintf(
            "Tables of type '%s' must be '%s' collection members", type, tc
         ))
      }
      dbc <- dbc %>% 
         dplyr::filter(.data$collection==!!tc & .data$table==!!name)
      if(nrow(dbc)==0){
         stop(sprintf(
            "Tables of type '%s' must be '%s' collection members", type, tc
         ))
      }
   }
   
   ## Adding table specification ----
   tables <- dplyr::bind_rows(
      tables,
      dplyr::tibble(name=name, type=type)
   )
   
   ## Adding features ----
   dm <- data_model(toRet)
   fk <- get_foreign_keys(dm)
   dtm <- dm[[name]]
   if(length(features)>0) for(i in 1:length(features)){
      features_sp <- do.call(
         .update_feature_spec,
         c(
            list(
               x=toRet, kmr=kmr, table=name, features=features_sp,
               dm=dm, dtm=dtm, fk=fk
            ),
            features[[i]]
         )
      )
   }
   
   ## Finalizing spec
   kms <- memoMDB(
      dataTables=list(Tables=tables, Features=features_sp, Helpers=kms$Helpers),
      dataModel=data_model(kms),
      dbInfo=db_info(kms),
      check=FALSE
   )
   
   ## Messages about the new specification ----
   f <- dplyr::filter(
      kms$Features, .data$table==!!name
   ) %>% 
      dplyr::pull("feature")
   pf <- dplyr::filter(kmr$Table_features, .data$table==!!type)
   mf <- pf$feature[which(pf$mandatory)]
   if(length(f)==0){
      warning(
         "There is no feature specification for this table.",
         " Some feature specifications must be added for",
         " allowing their integration."
      )
   }
   mmf <- setdiff(mf, f)
   if(length(mmf) > 0){
      warning(
         "The following features are mandatory and must be documented",
         " to allow the integration of the table: ",
         sprintf("'%s'", paste(mmf, collapse="', '"))
      )
   }
   
   ## Gathering information to return ----
   names(kms) <- kmson[match(names(kms), kmssn)]
   toRet <- c(toRet, kms)
   
   return(toRet)
}

###############################################################################@
#' Remove KM table specifications from an [MDB] object
#' 
#' @param x an [MDB] object to update with specification tables
#' @param kmr an [MDB] object with KM requirements
#' @param table the name of an existing table in x specification
#'
#' @return An [MDB] object with relevant KM table specification removed
#'
#' @export
#'
rm_km_table <- function(
   x, kmr, table
){
   
   stopifnot(
      is.MDB(x), is.KMR(kmr)
   )
   
   ## Splitting and preparing information ----
   kms <- get_km_spec(x, kmr)
   toRet <- x[setdiff(names(x), names(kms))]
   kmson <- names(kms)
   kmssn <- sub(
      "___.*-Tables___", "Tables",
      sub(
         "___.*-Features___", "Features",
         sub(
            "___.*-Helpers___", "Helpers",
            kmson
         )
      )
   )
   names(kms) <- kmssn[match(names(kms), kmson)]
   
   ## Expected information ----
   tables <- kms$Tables
   features <- kms$Features
   ftables <- dplyr::filter(
      tables,
      .data$name!=!!table
   )
   if(nrow(ftables)==nrow(tables)){
      warning("The table is not in specifications ==> no change")
   }
   ffeatures <- dplyr::filter(
      features,
      .data$table!=!!table
   )
   if(nrow(ffeatures)==nrow(features)){
      warning("The feature is not in specifications ==> no change")
   }
   
   kms <- memoMDB(
      dataTables=list(Tables=ftables, Features=ffeatures, Helpers=kms$Helpers),
      dataModel=data_model(kms),
      dbInfo=db_info(kms),
      check=FALSE
   )
   
   ## Gathering information to return ----
   names(kms) <- kmson[match(names(kms), kmssn)]
   toRet <- c(toRet, kms)
   
   return(toRet)
   
}


###############################################################################@
#' Add KM feature specifications to an [MDB] object
#' 
#' @param x an [MDB] object to update with specification tables
#' @param kmr an [MDB] object with KM requirements
#' @param table the name of an existing table in x
#' @param feature the name of an existing feature in kmr
#' @param fields Either a single character providing the name of an existing
#' field in table or a list named with feature property names from kmr. Each
#' element of the list should provide a "field" slot with the name of the
#' corresponding field and a "unit" slot with the name of the unit if relevant.
#' @param unit a single character providing the unit if relevant. Unit
#' information provided in fields override this parameter value.
#' 
#'
#' @return An [MDB] object with additional KM table feature specification
#'
#' @export
#'
add_km_feature <- function(
   x, kmr, table, feature, fields, unit=as.character(NA)
){
   
   stopifnot(
      is.MDB(x), is.KMR(kmr),
      feature %in% kmr$Features$name,
      is.character(unit), length(unit)==1
   )
   
   ## Splitting and preparing information ----
   kms <- get_km_spec(x, kmr)
   toRet <- x[setdiff(names(x), names(kms))]
   kmson <- names(kms)
   kmssn <- sub(
      "___.*-Tables___", "Tables",
      sub(
         "___.*-Features___", "Features",
         sub(
            "___.*-Helpers___", "Helpers",
            kmson
         )
      )
   )
   names(kms) <- kmssn[match(names(kms), kmson)]
   
   ## Expected information ----
   tables <- kms$Tables
   features <- kms$Features
   if(!table %in% kms$Tables$name){
      stop(sprintf(
         "The %s table is not documented yet",
         table
      ))
   }
   
   ## Provided information ----
   dm <- data_model(toRet)
   fk <- get_foreign_keys(dm)
   dtm <- dm[[table]]
   
   ## Features specifications ----
   features <- .update_feature_spec(
      x=toRet, kmr=kmr, table=table, features=features,
      dm=dm, dtm=dtm, fk=fk,
      feature=feature, fields=fields, unit=unit
   )
   kms <- memoMDB(
      dataTables=list(Tables=tables, Features=features, Helpers=kms$Helpers),
      dataModel=data_model(kms),
      dbInfo=db_info(kms),
      check=FALSE
   )
   
   ## Gathering information to return ----
   names(kms) <- kmson[match(names(kms), kmssn)]
   toRet <- c(toRet, kms)
   
   return(toRet)
   
}

.update_feature_spec <- function(
   x, kmr, table, features,
   dm, dtm, fk,
   feature, fields, unit=as.character(NA)
){

   ef <- dplyr::filter(features, .data$table==!!table)
   fp <- dplyr::filter(kmr$Feature_properties, .data$feature==!!feature)
   mprop <- fp$property[which(fp$mandatory)]
   
   ## Simple feature ----
   if(is.character(fields)){
      if(length(fields)!=1 || is.na(fields)){
         stop("fields must be a single non NA character or a list")
      }
      if(length(mprop) > 1){
         stop("This feature is defined by more than one mandatory property")
      }
      fields <- list(
         list(
            field=fields,
            unit=unit
         )
      )
      names(fields) <- mprop
   }
   if(!is.list(fields)){
      stop("fields must be a single non NA character or a list")
   }
   
   ## Generic feature ----
   
   props <- names(fields)
   
   ### Check mandatory and supported properties ----
   mmprops <- setdiff(mprop, props)
   if(length(mmprops)>0){
      stop(
         "The following properties are mandatory: ",
         sprintf("'%s'", paste(mmprops, collapse="', '"))
      )
   }
   addprops <- setdiff(props, fp$property)
   if(length(addprops)>0){
      stop(
         "The following properties are not not supported: ",
         sprintf("'%s'", paste(addprops, collapse="', '"))
      )
   }
   
   ### Check property specifications ----
   fields <- lapply(props, function(p){
      
      #### Field name ----
      f <- fields[[p]]
      toRet <- list()
      if(!"field" %in%  names(f)){
         stop("A 'field' slot must be provided in each fields element")
      }
      if(
         length(f$field)!=1 ||
         !is.character(f$field) ||
         is.na(f$field)
      ){
         stop("There must be on and only one field per property")
      }
      if(!f$field %in% dtm$fields$name){
         stop(sprintf("The '%s' field does not exist", f$field))
      }
      toRet$field <- f$field
      
      #### Unit ----
      m <- dplyr::filter(
         fp,
         .data$property==!!p
      ) %>%
         pull("measurement")
      if(is.na(m) && (!"unit" %in% names(f) || is.na(f$unit))){
         toRet$unit <- as.character(NA)
      }
      if(is.na(m) && "unit" %in% names(f) && !is.na(f$unit)){
         warning(sprintf(
            "Units are not supported for %s %s property",
            feature, p
         ))
         toRet$unit <- as.character(NA)
      }
      if(!is.na(m) && (!"unit" %in% names(f) || is.na(f$unit))){
         if(is.na(unit)){
            stop(sprintf(
               "Unit must be provided for %s %s property",
               feature, p
            ))
         }
         toRet$unit <- unit
      }
      if(!is.na(m) && "unit" %in% names(f)){
         u <- dplyr::filter(kmr$Units, .data$measurement==!!m) %>% 
            pull("unit")
         if(!is.character(f$unit) || length(f$unit)!=1){
            stop(
               "One and only one unit should be provided as a character",
               " for each property"
            )
         }
         if(!f$unit %in% u){
            stop(sprintf(
               "%s unit is not supported for %s %s property",
               f$unit, feature, p
            ))
         }
         toRet$unit <- f$unit
      }
      
      #### Field type ----
      pt <- dplyr::filter(
         fp,
         .data$property==!!p
      ) %>%
         pull("type")
      if(
         pt %in% c(
            "integer", "numeric", "logical", "character", "Date",
            "POSIXct", "base64"
         )
      ){
         ft <- dplyr::filter(dtm$fields, .data$name==!!f$field) %>% 
            dplyr::pull("type")
         if(ft != pt){
            stop(sprintf(
               "%s field should be of type %s",
               f$field, pt
            ))
         }
      }
      if(pt == "table"){
         missingTables <- setdiff(
            x[[table]][, f$field, drop=TRUE],
            names(dm)
         )
         if(length(missingTables)>0){
            stop(sprintf(
               "The following tables are missing: '%s'",
               paste(missingTables, collapse="', '")
            ))
         }
      }
      if(pt == "field"){
         ffk <- dplyr::filter(
            fk,
            (.data$from==!!table & .data$ff==f$field) |
               (.data$to==!!table & .data$tf==f$field)
         )
         if(nrow(fk)==0){
            stop(sprintf(
               "%s field should be involved in a foreign key",
               f$field
            ))
         }
      }
      
      return(toRet)
      
   })
   names(fields) <- props
   
   ### Get specifications ----
   fnames <- unlist(lapply(fields, function(f) f$field))
   if(any(duplicated(fnames))){
      stop("Fields cannot correspond to several properties")
   }
   units <- unlist(lapply(fields, function(f) f$unit))
   
   
   toRet <- rbind(
      features,
      dplyr::tibble(
         table=table, field=fnames,
         feature=feature, property=props, unit=units
      )
   )
   
   return(toRet)
}

###############################################################################@
#' Remove KM feature specifications from an [MDB] object
#' 
#' @param x an [MDB] object to update with specification tables
#' @param kmr an [MDB] object with KM requirements
#' @param table the name of an existing table in x
#' @param feature the name of a feature with specification in x table
#' 
#'
#' @return An [MDB] object with relevant KM table feature specification removed
#'
#' @export
#'
rm_km_feature <- function(
   x, kmr, table, feature
){
   
   stopifnot(
      is.MDB(x), is.KMR(kmr)
   )
   
   ## Splitting and preparing information ----
   kms <- get_km_spec(x, kmr)
   toRet <- x[setdiff(names(x), names(kms))]
   kmson <- names(kms)
   kmssn <- sub(
      "___.*-Tables___", "Tables",
      sub(
         "___.*-Features___", "Features",
         sub(
            "___.*-Helpers___", "Helpers",
            kmson
         )
      )
   )
   names(kms) <- kmssn[match(names(kms), kmson)]
   
   ## Expected information ----
   tables <- kms$Tables
   features <- kms$Features
   ffeatures <- dplyr::filter(
      features,
      .data$table!=!!table | .data$feature!=!!feature
   )
   if(nrow(ffeatures)==nrow(features)){
      warning("The feature is not in specifications ==> no change")
   }
   
   
   kms <- memoMDB(
      dataTables=list(Tables=tables, Features=ffeatures, Helpers=kms$Helpers),
      dataModel=data_model(kms),
      dbInfo=db_info(kms),
      check=FALSE
   )
   
   ## Gathering information to return ----
   names(kms) <- kmson[match(names(kms), kmssn)]
   toRet <- c(toRet, kms)
   
   return(toRet)
   
}

###############################################################################@
#'
#' @rdname add_helpers
#' @method add_helpers MDB
#' 
#' @param kmr an [MDB] object with KM requirements
#'
#' @export
#'
add_helpers.MDB <- function(x, code, name, language, kmr, ...){
   
   stopifnot(
      is.KMR(kmr),
      is.character(name), length(name)==1, !is.na(name),
      length(code)==1, file.exists(code),
      is.character(language), length(language)==1, !is.na(language)
   )
   
   ## Splitting and preparing information ----
   kms <- get_km_spec(x, kmr)
   toRet <- x[setdiff(names(x), names(kms))]
   kmson <- names(kms)
   kmssn <- sub(
      "___.*-Tables___", "Tables",
      sub(
         "___.*-Features___", "Features",
         sub(
            "___.*-Helpers___", "Helpers",
            kmson
         )
      )
   )
   names(kms) <- kmssn[match(names(kms), kmson)]
   
   ## Load the code -----
   code <- encode_bin(code)
   if(name %in% kms$Helpers$name){
      warning("Existing helpers have been replaced")
   }
   helpers <- dplyr::bind_rows(
      kms$Helpers %>% 
         dplyr::filter(.data$name!=!!name),
      dplyr::tibble(name=name, code=code, language=language)
   )
   
   ## Finalizing spec
   kms <- memoMDB(
      dataTables=list(
         Tables=kms$Tables, Features=kms$Features, Helpers=helpers
      ),
      dataModel=data_model(kms),
      dbInfo=db_info(kms),
      check=FALSE
   )
   names(kms) <- kmson[match(names(kms), kmssn)]
   toRet <- c(toRet, kms)
   
   return(toRet)
   
}

###############################################################################@
#' 
#' @rdname get_R_helpers
#' @method get_R_helpers MDB
#' 
#' @param kmr a [KMR] object
#' @param tkcat A [TKCat] or [chTKCat] object to make available in
#' helper environment
#'
#' @details x, kmr and tkcat objects are made available in helpers environment
#' as 'THISMDB', 'THISKMR' and 'THISTKCAT' objects respectively and can be used
#' as such within helpers code.
#' 
#' @export
#'
get_R_helpers.MDB <- function(x, hnames=NA, kmr, tkcat=NULL, ...){
   
   stopifnot(
      is.KMR(kmr)
   )
   
   ## Splitting information ----
   kms <- get_km_spec(x, kmr)
   kmson <- names(kms)
   kmssn <- sub(
      "___.*-Tables___", "Tables",
      sub(
         "___.*-Features___", "Features",
         sub(
            "___.*-Helpers___", "Helpers",
            kmson
         )
      )
   )
   names(kms) <- kmssn[match(names(kms), kmson)]
   
   ## Get the code binary ----
   if(is.chMDB(kms)){
      scode <- get_query(
         kms, 
         sprintf(
            "SELECT name, code FROM %s WHERE language='R'",
            db_tables(kms)$dbTables["Helpers"]
         )
      )
   }else{
      scode <- kms$Helpers %>% 
         dplyr::filter(.data$language=="R")
         
   }
   if(!is.na(hnames)){
      scode <- scode %>% 
         dplyr::filter(.data$name %in% hnames)
   }
   code <- ""
   if(nrow(scode) > 0){
      for(i in 1:nrow(scode)){
         code <- paste(code, rawToChar(decode_bin(scode$code[i])), sep="\n")
      }
   }
   if(is.null(tkcat) && is.chMDB(x)){
      tkcat <- unclass(x)$tkcon
   }
   toRet <- parse_R_helpers(code, THISMDB=x, THISKMR=kmr, THISTKCAT=tkcat)
   return(toRet)
}
