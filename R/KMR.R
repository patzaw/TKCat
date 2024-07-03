###############################################################################@
#' Create an [MDB] object with Knowledge Management Requirements (KMR)
#' 
#' @aliases KMR
#' 
#' @param name the name of the requirements
#' @param title the title of the requirements
#' @param description the description of the requirements
#' @param version version of the requirements
#' @param maintainer maintainer of the requirements
#'
#' @return An MDB object with KM requirements data model
#' 
#' @export
#'
create_KMR <- function(
   name, title, description, version, maintainer
){
   stopifnot(
      is.character(name), length(name)==1, !is.na(name),
      is.character(title), length(title)==1, !is.na(title),
      is.character(description), length(description)==1, !is.na(description),
      is.character(version), length(version)==1, !is.na(version),
      is.character(maintainer), length(maintainer)==1, !is.na(maintainer)
   )
   dataTables <- list()
   for(n in names(KMR_DM)){
      tm <- KMR_DM[[n]]
      rtypes <- tm$fields$type
      names(rtypes) <- tm$fields$name
      value <- dplyr::tibble()
      for(i in 1:nrow(tm$fields)){
         toAdd <- integer()
         dcl <- tm$fields$type[i]
         dcl <- ifelse(dcl == "base64", "character", dcl)
         class(toAdd) <- dcl
         value[,tm$fields$name[i]] <- toAdd
      }
      dataTables[[n]] <- value
   }
   toRet <- memoMDB(
      dataTables=dataTables,
      dataModel=KMR_DM,
      dbInfo=list(
         name=name, title=title, description=description,
         version=version, maintainer=maintainer
      )
   )
   class(toRet) <- unique(c("KMR", class(toRet)))
   return(toRet)
}


###############################################################################@
#' Check if an object represents Knowledge Management Requirements (KMR)
#' 
#' @param x an object
#'
#' @return TRUE if x is a KMR, FALSE if not
#' 
#' @export
#'
is.KMR <- function(x){
   return(
      inherits(x, "KMR") ||
         (
            is.MDB(x) &&
               ReDaMoR::identical_RelDataModel(
                  data_model(x), KMR_DM, includeDisplay=FALSE
               )
         )
   )
}

###############################################################################@
#' Convert in a [KMR] object when possible
#' 
#' @param x an object
#'
#' @return A [KMR] object
#' 
#' @export
#'
as_KMR <- function(x){
   if(
      inherits(x, "KMR") ||
         (
            is.MDB(x) &&
            ReDaMoR::identical_RelDataModel(
               data_model(x), KMR_DM, includeDisplay=FALSE
            )
         )
   ){
      x <- as_memoMDB(x)
      class(x) <- unique(c("KMR", class(x)))
      return(x)
   }else{
      stop("This is object cannot be a KMR")
   }
}


###############################################################################@
#' Read [KMR] from a path
#' 
#' @param ... parameters for [read_fileMDB()]
#'
#' @return A [KMR] object
#' 
#' @export
#'
read_KMR <- function(...){
   read_fileMDB(...) %>% 
      as_KMR()
}


###############################################################################@
#' Add a unit definition to Knowledge Management Requirements (KMR)
#' 
#' @param kmr an [KMR] object
#' @param measurement the name of the measurement (e.g. "volume")
#' @param unit the name of the unit (e.g. "ml")
#' @param description a description of the unit (e.g. "milliliter) 
#' 
#' @return The modified [KMR] object 
#' 
#' @export
#' 
add_unit_def <- function(kmr, measurement, unit, description){
   stopifnot(
      is.KMR(kmr),
      is.character(measurement), length(measurement)==1, !is.na(measurement),
      measurement != "",
      is.character(unit), length(unit)==1, !is.na(unit), unit != "",
      is.character(description), length(description)==1, !is.na(description)
   )
   ##
   toChange <- c("Units")
   newInfo <- data_tables(kmr[toChange])
   kmr <- kmr[setdiff(names(kmr), toChange)]
   ##
   toAdd <- dplyr::tibble(
      "measurement"=measurement,
      "unit"=unit,
      "description"=description
   )
   if(
      nrow(
         dplyr::inner_join(toAdd, newInfo$Units, by=c("measurement", "unit"))
      ) > 0
   ){
      stop("The unit already exists")
   }
   newInfo$Units <- dplyr::bind_rows(
      newInfo$Units,
      toAdd
   )
   newInfo <- memoMDB(
      newInfo,
      dataModel=KMR_DM[names(newInfo), rmForeignKeys=TRUE],
      dbInfo=list(name="newInfo"),
      check=FALSE
   )
   ##
   MDBs <- list(kmr, newInfo)
   names(MDBs) <- unlist(lapply(MDBs, function(x) db_info(x)$name))
   toRet <- metaMDB(
      MDBs=MDBs,
      relationalTables=list(),
      dataModel=KMR_DM,
      dbInfo=db_info(kmr),
      check=FALSE
   ) %>% 
      as_memoMDB()
   class(toRet) <- unique(c("KMR", class(toRet)))
   return(toRet)
}


###############################################################################@
#' Add a feature definition to Knowledge Management Requirements (KMR)
#' 
#' @param kmr a [KMR] object
#' @param name the name of the feature type
#' @param description description of the feature type
#' @param properties properties of the feature.
#' A list named with property names. For each property, a list with:
#' - **type**: among "integer", "numeric", "logical", "character", "Date",
#' "POSIXct", "base64", "table" and "field" ("table" and "field" type are used 
#' for referencing tables and fields; the other types come from ReDaMoR).
#' - **description** (optional): a description of the feature property. Useful
#' when the feature has more than one property
#' - **mandatory**: a logical indicating the property is mandatory for the
#' feature
#' - **measurement** (optional and only for "integer" and "numeric" types):
#' the name of the measurement for checking units
#' 
#' @return The modified KMR [MDB] object
#' 
#' @export
#' 
add_feature_def <- function(
   kmr, name, description, properties
){
   stopifnot(
      is.KMR(kmr),
      is.character(name), length(name)==1, !is.na(name),
      is.character(description), length(description)==1, !is.na(description),
      is.list(properties), length(properties) > 0,
      length(properties)==length(names(properties)),
      all(!duplicated(names(properties))),
      all(unlist(lapply(names(properties), function(pn){
         p <- properties[[pn]]
         res <- all(
            names(p) %in% c("type", "mandatory", "measurement", "description")
         )
         if(!res) message(sprintf("%s: Wrong property info", pn))
         res <- res &&
            is.character(p$type) && length(p$type)==1 &&
            p$type %in% c(
               "integer", "numeric", "logical", "character", "Date",
               "POSIXct", "base64", "table", "field"
            )
         if(!res) message(sprintf("%s: Wrong type", pn))
         res <- res &&
            length(p$mandatory)==1 && is.logical(p$mandatory) &&
            !is.na(p$mandatory)
         if(!res) message(sprintf("%s: Wrong mandatory value", pn))
         res <- res && (
            length(p$measurement)==0 || (
               length(p$measurement==1) &&
                  (is.na(p$measurement) || is.character(p$measurement))
            )
         )
         if(!res) message(sprintf("%s: Wrong measurement value", pn))
         res <- res && (
            length(p$description)==0 || (
               length(p$description==1) &&
                  (is.na(p$description) || is.character(p$description))
            )
         )
         if(!res) message(sprintf("%s: Wrong description value", pn))
         return(res)
      })))
   )
   ##
   toChange <- c("Features", "Feature_properties")
   newInfo <- data_tables(kmr[toChange])
   kmr <- kmr[setdiff(names(kmr), toChange)]
   ##
   ##
   toAdd <- dplyr::tibble(
      "name"=name,
      "description"=description
   )
   if(
      nrow(
         dplyr::inner_join(toAdd, newInfo$Features, by="name")
      ) > 0
   ){
      stop("The feature already exists")
   }
   newInfo$Features  <- dplyr::bind_rows(newInfo$Features, toAdd)
   measurements <- unique(kmr$Units$measurement)
   pToAdd <- lapply(properties, function(p){
      if(length(p$measurement)==0){
         p$measurement <- as.character(NA)
      }
      if(!is.na(p$measurement) && !p$measurement %in% measurements){
         stop(sprintf("Unknown measurement: %s", p$measurement))
      }
      if(length(p$description)==0){
         p$description <- as.character(NA)
      }
      return(dplyr::as_tibble(
         p[c("type", "mandatory", "measurement", "description")]
      ))
   })
   pToAdd <- dplyr::bind_rows(pToAdd)
   pToAdd <- dplyr::bind_cols(
      tibble(feature=name, property=names(properties)),
      pToAdd
   )
   newInfo$Feature_properties  <- dplyr::bind_rows(
      newInfo$Feature_properties, pToAdd
   )
   newInfo <- memoMDB(
      newInfo,
      dataModel=KMR_DM[names(newInfo), rmForeignKeys=TRUE],
      dbInfo=list(name="newInfo"),
      check=FALSE
   )
   ##
   MDBs <- list(kmr, newInfo)
   names(MDBs) <- unlist(lapply(MDBs, function(x) db_info(x)$name))
   toRet <- metaMDB(
      MDBs=MDBs,
      relationalTables=list(),
      dataModel=KMR_DM,
      dbInfo=db_info(kmr),
      check=FALSE
   ) %>% 
      as_memoMDB()
   class(toRet) <- unique(c("KMR", class(toRet)))
   return(toRet)
}


###############################################################################@
#' Add possible values to an integer or a character feature property in [KMR]
#' 
#' @param kmr a [KMR] object
#' @param feature the name of the feature type
#' @param property the name of the property
#' @param values a vector of character or integer or a named vector with the
#' description of the values
#' 
#' @return The modified KMR [MDB] object
#' 
#' @export
#' 
add_property_values <- function(
   kmr, feature, property, values
){
   stopifnot(
      is.KMR(kmr),
      is.character(feature), length(feature)==1, !is.na(feature),
      is.character(property), length(property)==1, !is.na(property)
   )
   foi <- dplyr::filter(
      kmr$Feature_properties,
      .data$feature==!!feature,
      .data$property==!!property
   )
   if(nrow(foi)!=1){
      stop("The feature property does not exist")
   }
   if(!foi$type %in% c("integer", "character")){
      stop(paste(
         "Possible values can be recorded for",
         "integer and character properties only"
      ))
   }
   ##
   if(foi$type=="integer"){
      toChange <- c("Integer_values")
      if(length(names(values))==0){
         values <- as.integer(values)
         description <- as.character(rep(NA, length(values)))
      }else{
         description <- as.character(values)
         values <- as.integer(names(values))
      }
   }else{
      toChange <- c("Character_values")
      if(length(names(values))==0){
         values <- as.character(values)
         description <- as.character(rep(NA, length(values)))
      }else{
         description <- as.character(values)
         values <- as.character(names(values))
      }
   }
   if(any(is.na(values))){
      stop("values cannot contain NA")
   }
   if(any(duplicated(values))){
      stop("values cannot be duplicated")
   }
   toAdd <- dplyr::tibble(
      feature=feature, property=property,
      value=values,
      description=description
   )
   newInfo <- data_tables(kmr[toChange])
   if(
      nrow(dplyr::inner_join(
         toAdd, newInfo[[1]], by=c("feature", "property", "value")
      )) > 0
   ){
      warning("Description of existing values will be overwritten")
   }
   newInfo[[1]] <- dplyr::distinct(
      dplyr::bind_rows(toAdd, newInfo[[1]]),
      .data$feature, .data$property, .data$value,
      .keep_all=TRUE
   )
   ##
   newInfo <- memoMDB(
      newInfo,
      dataModel=KMR_DM[names(newInfo), rmForeignKeys=TRUE],
      dbInfo=list(name="newInfo"),
      check=FALSE
   )
   kmr <- kmr[setdiff(names(kmr), toChange)]
   ##
   MDBs <- list(kmr, newInfo)
   names(MDBs) <- unlist(lapply(MDBs, function(x) db_info(x)$name))
   toRet <- metaMDB(
      MDBs=MDBs,
      relationalTables=list(),
      dataModel=KMR_DM,
      dbInfo=db_info(kmr),
      check=FALSE
   ) %>% 
      as_memoMDB()
   class(toRet) <- unique(c("KMR", class(toRet)))
   return(toRet)
}

###############################################################################@
#' Add a table definition to Knowledge Management Requirements (KMR)
#' 
#' @param kmr a [KMR] object
#' @param name the name of the table type
#' @param description description of the table type
#' @param collection the name of the collection of which this table type
#' must be a member (default: NA)
#' @param mandatory_features a character vector with mandatory features for this
#' table type
#' 
#' @return The modified KMR [MDB] object 
#' 
#' @export
#' 
add_table_def <- function(
   kmr, name, description, collection=as.character(NA), mandatory_features
){
   stopifnot(
      is.KMR(kmr),
      is.character(name), length(name)==1, !is.na(name),
      !name %in% kmr$Tables$name,
      is.character(description), length(description)==1, !is.na(description),
      is.character(collection), length(collection)==1,
      is.character(mandatory_features), length(mandatory_features) > 0,
      all(!is.na(mandatory_features)),
      all(mandatory_features %in% kmr$Features$name)
   )
   mandatory_features <- unique(mandatory_features)
   ##
   toChange <- c("Tables", "Table_features")
   newInfo <- data_tables(kmr[toChange])
   toAdd <- dplyr::tibble(
      name=name, description=description, collection=collection
   )
   newInfo$Tables <- dplyr::bind_rows(
      newInfo$Tables,
      toAdd
   )
   ftoAdd <- dplyr::tibble(
      table=name, feature=mandatory_features, mandatory=TRUE
   )
   newInfo$Table_features <- dplyr::bind_rows(
      newInfo$Table_features,
      ftoAdd
   )
   ##
   newInfo <- memoMDB(
      newInfo,
      dataModel=KMR_DM[names(newInfo), rmForeignKeys=TRUE],
      dbInfo=list(name="newInfo"),
      check=FALSE
   )
   kmr <- kmr[setdiff(names(kmr), toChange)]
   ##
   MDBs <- list(kmr, newInfo)
   names(MDBs) <- unlist(lapply(MDBs, function(x) db_info(x)$name))
   toRet <- metaMDB(
      MDBs=MDBs,
      relationalTables=list(),
      dataModel=KMR_DM,
      dbInfo=db_info(kmr),
      check=FALSE
   ) %>% 
      as_memoMDB()
   class(toRet) <- unique(c("KMR", class(toRet)))
   return(toRet)
}

###############################################################################@
#' Add possible features to table type in [KMR]
#' 
#' @param kmr a [KMR] object
#' @param table the name of the table type
#' @param features a character vector with optional features for this table type
#' 
#' @return The modified KMR [MDB] object 
#' 
#' @export
#' 
add_table_features <- function(
   kmr, table, features
){
   stopifnot(
      is.KMR(kmr),
      is.character(table), length(table)==1, !is.na(table),
      table %in% kmr$Tables$name,
      is.character(features), length(features) > 0,
      all(!is.na(features)),
      all(features %in% kmr$Features$name)
   )
   features <- unique(features)
   ##
   toChange <- c("Table_features")
   newInfo <- data_tables(kmr[toChange])
   features <- setdiff(
      features,
      newInfo$Table_features$feature[
         which(newInfo$Table_features$table==table)
      ]
   )
   if(length(features) > 0){
      ftoAdd <- dplyr::tibble(
         table=table, feature=features, mandatory=FALSE
      )
      newInfo$Table_features <- dplyr::bind_rows(
         newInfo$Table_features,
         ftoAdd
      )
   }else{
      return(kmr)
   }
   ##
   newInfo <- memoMDB(
      newInfo,
      dataModel=KMR_DM[names(newInfo), rmForeignKeys=TRUE],
      dbInfo=list(name="newInfo"),
      check=FALSE
   )
   kmr <- kmr[setdiff(names(kmr), toChange)]
   ##
   MDBs <- list(kmr, newInfo)
   names(MDBs) <- unlist(lapply(MDBs, function(x) db_info(x)$name))
   toRet <- metaMDB(
      MDBs=MDBs,
      relationalTables=list(),
      dataModel=KMR_DM,
      dbInfo=db_info(kmr),
      check=FALSE
   ) %>% 
      as_memoMDB()
   class(toRet) <- unique(c("KMR", class(toRet)))
   return(toRet)
}


###############################################################################@
#' Get a [KMR] object from a [TKCat] or a [chTKCat] object
#' 
#' @param ... parameters for the [get_MDB()] function
#' 
#' @return A [KMR] object
#' 
#' @export
#' 
get_KMR <- function(...){
   as_KMR(get_MDB(...))
}


###############################################################################@
#' List types of tables defined in a [KMR] object
#' 
#' @param kmr a [KMR] object
#' 
#' @return A [dplyr::tibble] with the names of table types, their descriptions and
#' the related collections
#' 
#' @export
#' 
list_table_types <- function(kmr){
   stopifnot(is.KMR(kmr))
   return(arrange(kmr$Tables, .data$name))
}


###############################################################################@
#' List the features provided by a set of tables
#'
#' @param kmr a [KMR] object
#' @param tables the name of the tables. If NULL (default), all the features
#' are listed.
#' 
#' @return A [dplyr::tibble] with feature description and properties
#' 
#' @export
#' 
list_table_features <- function(kmr, tables=NULL){
   stopifnot(is.KMR(kmr))
   getfp <- function(x){
      dplyr::group_by(x$Feature_properties, .data$feature) %>% 
         dplyr::arrange(
            .data$feature, desc(.data$mandatory), .data$property
         ) %>% 
         dplyr::summarise(
            properties=paste(
               sprintf(
                  ifelse(.data$mandatory, "%s", "(%s)"),
                  .data$property
               ),
               collapse=", "
            )
         ) %>% 
         dplyr::ungroup()
   }
   if(length(tables)==0){
      fp <- getfp(kmr)
      toRet <- dplyr::left_join(kmr$Features, fp, by=c("name"="feature")) %>% 
         dplyr::rename(
            "feature"="name", "feature.description"="description"
         ) %>% 
         dplyr::arrange(.data$feature)
   }else{
      stopifnot(all(tables %in% kmr$Tables$name))
      fkmr <- dplyr::select(
         kmr,
         "Tables", "Table_features", "Features", "Feature_properties"
      )
      name <- "" # declaring "name" variable
      fkmr <- dplyr::filter(fkmr, Tables=name %in% tables)
      fp <- getfp(fkmr)
      toRet <- join_mdb_tables(
         fkmr, setdiff(names(fkmr), "Feature_properties")
      )
      toRet <- toRet %>% 
         dplyr::pull(setdiff(names(toRet), "Feature_properties")) %>% 
         dplyr::left_join(
            fp, by="feature"
         ) %>% 
         dplyr::select(
            "table"="name",
            "feature", "mandatory",
            "feature.description"="Features.description",
            "properties"
         ) %>% 
         dplyr::arrange(
            .data$table, dplyr::desc(.data$mandatory), .data$feature
         )
   }
   return(toRet)
}


###############################################################################@
#' List properties of a feature
#'
#' @param kmr a [KMR] object
#' @param feature the name of the feature
#' 
#' @return A [dplyr::tibble] with the description of feature properties
#' 
#' @export
#' 
list_feature_properties <- function(kmr, feature){
   stopifnot(
      is.KMR(kmr),
      length(feature)==1,
      feature %in% kmr$Features$name
   )
   return(dplyr::arrange(
      dplyr::filter(kmr$Feature_properties, .data$feature==!!feature),
      dplyr::desc(.data$mandatory), .data$property
   ))
}


###############################################################################@
#' List supported types of measurement
#'
#' @param kmr a [KMR] object
#' 
#' @return A vector of character with measurement names
#' 
#' @export
#' 
list_measurements <- function(kmr){
   stopifnot(is.KMR(kmr))
   return(sort(unique(dplyr::pull(kmr$Units, "measurement"))))
}


###############################################################################@
#' List possible units for a type of measurement
#'
#' @param kmr a [KMR] object
#' @param measurement the type of measurement
#' 
#' @return A [dplyr::tibble] with the description of supported units
#' 
#' @export
#' 
list_measurement_units <- function(kmr, measurement){
   stopifnot(
      is.KMR(kmr),
      length(measurement)==1,
      measurement %in% kmr$Units$measurement
   )
   return(dplyr::arrange(
      dplyr::filter(kmr$Units, .data$measurement==!!measurement),
      .data$unit
   ))
}


###############################################################################@
#' List of supported values for an integer or a character property
#'
#' @param kmr a [KMR] object
#' @param feature the name of the feature
#' @param property the name of the property
#' 
#' @return A [dplyr::tibble] with value and their description (if available)
#' 
#' @export
#' 
list_property_values <- function(kmr, feature, property){
   stopifnot(
      is.KMR(kmr),
      length(feature)==1, length(property)==1
   )
   fp <- dplyr::filter(
      kmr$Feature_properties,
      .data$feature==!!feature & .data$property==!!property
   )
   if(nrow(fp)==0){
      stop("The feature property does not exist")
   }
   if(!fp$type %in% c("character", "integer")){
      stop("Property must be of type character or integer")
   }
   if(fp$type=="character"){
      toRet <- dplyr::filter(
         kmr$Character_values,
         .data$feature==!!feature & .data$property==!!property
      )
   }
   if(fp$type=="integer"){
      toRet <- dplyr::filter(
         kmr$Integer_values,
         .data$feature==!!feature & .data$property==!!property
      )
   }
   toRet <- dplyr::arrange(toRet, .data$value)
   return(toRet)
}

###############################################################################@
#'
#' @rdname add_helpers
#' @method add_helpers KMR
#' 
#'
#' @export
#'
add_helpers.KMR <- function(x, code, name, language, ...){
   
   stopifnot(
      is.character(name), length(name)==1, !is.na(name),
      length(code)==1, file.exists(code),
      is.character(language), length(language)==1, !is.na(language)
   )
   
   ## Load the code -----
   code <- encode_bin(code)
   if(name %in% x$Helpers$name){
      warning("Existing helpers have been replaced")
   }
   helpers <- dplyr::bind_rows(
      x$Helpers %>% 
         dplyr::filter(.data$name!=!!name),
      dplyr::tibble(name=name, code=code, language=language)
   )
   
   ## Finalizing spec
   toRet <- memoMDB(
      dataTables=c(
         data_tables(x, setdiff(names(x), "Helpers")),
         list(Helpers=helpers)
      ),
      dataModel=data_model(x),
      dbInfo=db_info(x),
      check=FALSE
   ) %>% 
      as_KMR()
   
   return(toRet)
   
}

###############################################################################@
#' 
#' @rdname get_R_helpers
#' @method get_R_helpers KMR
#' 
#' @param tkcat A [TKCat] or [chTKCat] object to make available in
#' helper environment
#' @param mdb An [MDB] object to make available in helper environment
#' 
#' @details x, tkcat and mdb objects are made available in helpers environment
#' as 'THISKMR', 'THISTKCAT' and 'THISMDB' objects respectively and can be used
#' as such within helpers code.
#' 
#' @export
#'
get_R_helpers.KMR <- function(x, hnames=NA, tkcat=NULL, mdb=NULL, ...){
   
   stopifnot(
      is.MDB(x)
   )
   
   ## Get the code binary ----
   scode <- x$Helpers %>% 
      dplyr::filter(.data$language=="R")
      
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
   toRet <- parse_R_helpers(code, THISKMR=x, THISTKCAT=tkcat, THISMDB=mdb)
   return(toRet)
}
