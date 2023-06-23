###############################################################################@
###############################################################################@
## Collibra ----
###############################################################################@

collibra_tables <- c("___Collibra___", "___Collibra_dds___")

###############################################################################@
#' Add Collibra metadata to an MDB. This MDB should already have KM specification tables (see [TKCat::add_km_spec()])
#' 
#' @param x the MDB object to update
#' @param kmr the KMR object with TBKM specifications (by default, the KMR to which the function is attached).
#' @param overwrite a logical indicating if existing Collibra metadata should be overwritten (default: FALSE)
#' @param `Alias` Short name of the asset. This can be an acronym or abbreviation.
#' @param `Domain` Scientific domain covered by the data
#' @param `Drug development stage` Stages in drug development process where the asset may be relevant or valuable.
#' @param `Primary Use Case` UCB primary reason the data was generated or accessed
#' @param `Restrictions` Limitations (e.g., geography, function, contract, etc.) on data access and usage
#' @param `License` UCB relationship with the data provider e.g. public access, academic partnership, commercial license
#' @param `Source of data` Main source(s) of the captured data or method of data collection/generation
#' @param `Nature of data` Type(s) of data within the asset. This can include clarification on the entities captured in the asset (e.g. genes, proteins).
#' @param `Refresh Frequency` How often the asset is updated
#' @param `Community` Collibra community of users
#' 
#' @return An MDB with additional tables with Collibra metadata
#' 
#' @import TKCat ReDaMoR
#'   
#' @export
#' 
add_collibra_metadata <- function(
   x,
   kmr=THISKMR,
   overwrite=FALSE,
   `Alias`=as.character(NA),
   `Domain`,
   `Drug development stage`,
   `Primary Use Case`,
   `Restrictions`,
   `License`,
   `Source of data`=as.character(NA),
   `Nature of data`,
   `Refresh Frequency`=as.character(NA),
   `Community`
){
   stopifnot(
      TKCat::is_KMR(kmr), is.MDB(x)
   )
   kms <- get_km_spec(x, kmr)
   toRet <- x
   if(any(collibra_tables %in% names(x))){
      if(!overwrite){
         stop(
            "Collibra reserved table names are already used in the provided MDB"
         )
      }else{
         for(tn in collibra_tables){
            toRet <- TKCat::rm_km_table(toRet, kmr, tn)
         }
         toRet <- toRet[setdiff(names(toRet), collibra_tables)]
      }
   }
   
   `___Collibra___` <- tibble(
      `Alias`=`Alias`,
      `Domain`=`Domain`,
      `Primary Use Case`=`Primary Use Case`,
      `Restrictions`=`Restrictions`,
      `License`=`License`,
      `Source of data`=`Source of data`,
      `Nature of data`=`Nature of data`,
      `Refresh Frequency`=`Refresh Frequency`,
      `Community`=`Community`
   )
   `___Collibra_dds___` <- tibble(
      `Drug development stage`=`Drug development stage`
   )
   
   
   dm <- ReDaMoR::df_to_model(
      list = collibra_tables,
      envir=environment()
   )
   dm$`___Collibra___`$display$comment <-
      dm$`___Collibra_dds___`$display$comment <-
      "MetaData for the Collibra catalog"
   dm$`___Collibra___`$display$x <- 0
   dm$`___Collibra_dds___`$display$x <- 0
   dm$`___Collibra___`$display$y <- -60
   dm$`___Collibra_dds___`$display$y <- 60
   
   finfo <- list_table_features(kmr, "collibra")
   dm$`___Collibra___`$fields$comment <- finfo$feature.description[match(
      paste(dm$`___Collibra___`$fields$name, "(Collibra)"), finfo$feature
   )]
   dm$`___Collibra___`$fields$nullable <- !finfo$mandatory[match(
      paste(dm$`___Collibra___`$fields$name, "(Collibra)"), finfo$feature
   )]
   dm$`___Collibra___`$fields$unique <- TRUE
   
   finfo <- list_table_features(kmr, "collibra drug development stage")
   dm$`___Collibra_dds___`$fields$comment <- finfo$feature.description[match(
      paste(dm$`___Collibra_dds___`$fields$name, "(Collibra)"), finfo$feature
   )]
   dm$`___Collibra_dds___`$fields$nullable <- !finfo$mandatory[match(
      paste(dm$`___Collibra_dds___`$fields$name, "(Collibra)"), finfo$feature
   )]
   dm$`___Collibra_dds___`$fields$unique <- TRUE
   
   ## Adapt display of spec. data model
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
   # dmxmin <- min(xpos[,"x"])
   dmxmax <- max(xpos[,"x"])
   dmymin <- min(xpos[,"y"])
   # xshift <- xmax - dmxmin + ((xmax-xmin)/10)
   xshift <- -(dmxmax - xmin + ((xmax-xmin)/10))
   yshift <- ymax - dmymin + ((ymax-ymin)/10)
   ##
   dm <- unclass(dm)
   for(tn in names(dm)){
      dm[[tn]]$display$x <- dm[[tn]]$display$x + xshift
      dm[[tn]]$display$y <- dm[[tn]]$display$y + yshift
      dm[[tn]]$display$color <- "white"
   }
   dm <- ReDaMoR::RelDataModel(dm, checkFK=FALSE)
   
   toRet <- c(
      toRet,
      memoMDB(
         dataTables=list(
            "___Collibra___"=`___Collibra___`,
            "___Collibra_dds___"=`___Collibra_dds___`
         ),
         dataModel=dm,
         dbInfo=list(
            name="cspec"
         ),
         check=FALSE
      )
   )
   
   toRet <- add_km_table(
      toRet, kmr,
      name="___Collibra___", type="collibra",
      features=lapply(
         list_table_features(kmr, "collibra")$feature,
         function(x){
            list(feature=x, fields=sub(" [(]Collibra[)]$", "", x))
         }
      )
   )
   
   toRet <- add_km_table(
      toRet, kmr,
      name="___Collibra_dds___", type="collibra drug development stage",
      features=lapply(
         list_table_features(kmr, "collibra drug development stage")$feature,
         function(x){
            list(feature=x, fields=sub(" [(]Collibra[)]$", "", x))
         }
      )
   )
   
   return(toRet)
}

###############################################################################@
#' Get Collibra metadata from an MDB
#'
#' @param x the MDB object with Collibra metadata
#' 
#' @return An MDB with Collibra metadata only
#' 
#' @import TKCat
#'   
#' @export
#' 
get_collibra_mdb <- function(x){
   stopifnot(
      is.MDB(x)
   )
   toRet <- TKCat::as_memoMDB(x[collibra_tables])
   TKCat::db_info(toRet)$name <- sprintf(
      "Collibra metadata for %s",
      TKCat::db_info(x)$name
   )
   return(toRet)
}

###############################################################################@
#' Get Collibra metadata from all MDBs
#' 
#' @param kmr the KMR object with TBKM specifications (by default, the KMR to which the function is attached)
#' 
#' @return A tibble with Collibra metadata from relevant MDBs
#' 
#' @import TKCat ReDaMoR
#'   
#' @export
#' 
get_collibra_metadata <- function(
      kmr=THISKMR
){
   stopifnot(
      TKCat::is_KMR(kmr)
   )
   allMdbs <- TKCat::list_MDBs(unclass(kmr)$tkcon)
   allTables <- TKCat::list_tables(unclass(kmr)$tkcon)
   toTake <- allTables %>%
      dplyr::filter(name=="___Collibra___") %>% 
      pull("database") %>% 
      unique()
   toRet <- get_query(
      kmr, 
      paste(
         sprintf(
            "SELECT * FROM (
               SELECT '%s' AS MDB, * FROM `%s`.`___Collibra___`
               CROSS JOIN (
                  SELECT arrayCompact(groupArray(`Drug development stage`))
                  AS `Drug development stage`
                  FROM `%s`.`___Collibra_dds___`
               )
            )
            ",
            toTake, toTake, toTake
         ),
         collapse = " UNION ALL "
      )
   ) %>% 
      dplyr::as_tibble() %>% 
      dplyr::mutate(
         "Drug development stage"=unlist(lapply(
            `Drug development stage`, paste, collapse=","
         ))
      ) %>% left_join(
         allMdbs %>%
            mutate(size=TKCat:::.format_bytes(total_size)) %>% 
            select(
               "MDB"="name",
               "Description"="description",
               "Last update"="timestamp",
               "Owner"="maintainer",
               "Size of asset"="size"
            ),
         by="MDB"
      ) %>% 
      mutate(
         "Full Name"=paste(MDB, "in TKCat"),
         "Last Review Date"=NA,
         "Asset Type"="Data Set",
         "Location"=sprintf(
            "chTKCat on %s:%s (contact: %s)",
            unclass(kmr)$tkcon$chcon@host, unclass(kmr)$tkcon$chcon@port,
            unclass(kmr)$tkcon$contact
         )
      ) %>% 
      select(all_of(c(
         "MDB",
         "Full Name",
         "Alias",
         "Domain",
         "Description",
         "Drug development stage",
         "Primary Use Case",
         "Restrictions",
         "Location",
         "License",
         "Source of data",
         "Nature of data",
         "Last update",
         "Last Review Date",
         "Size of asset",
         "Refresh Frequency",
         "Asset Type",
         "Community",
         "Owner"
      )))
   
   return(toRet)
}

###############################################################################@
###############################################################################@
## Tests ----
###############################################################################@


###############################################################################@
#' List MDB with DE analyses
#' 
#' @param kmr the KMR object with TBKM specifications (by default, the KMR to which the function is attached). This KMR must also be a chMDB object.
#' 
#' @return  a tibble with DE analyses description tables
#'   
#' @import TKCat dplyr
#' 
#' @export
#' 
list_MDB_with_DE_analyses <- function(kmr=THISKMR){
   stopifnot(
      TKCat::is_KMR(kmr), TKCat::is.chMDB(kmr)
   )
   k <- unclass(kmr)$tkcon
   n <- TKCat::db_info(kmr)$name
   mdbNames <- TKCat::get_query(
      k,
      sprintf(
         "SELECT database FROM system.tables WHERE name='%s'",
         sprintf("___%s-Tables___", n)
      )
   )$database
   query <- paste(
      sprintf(
         "SELECT *, '%s' as mdb FROM `%s`.`%s` WHERE type='DE analyses'",
         mdbNames, mdbNames, sprintf("___%s-Tables___", n)
      ),
      collapse=" UNION ALL "
   )
   toRet <- TKCat::get_query(k, query)
   return(toRet)
}
