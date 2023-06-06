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

###############################################################################@
#' Add Collibra metadata to an MDB This MDB should already have KM specification tables (see [TKCat::add_km_spec()])
#' 
#' @param kmr the KMR object with TBKM specifications (by default, the KMR to which the function is attached).
#' @param x the MDB objet to update
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
   kmr=THISKMR,
   x,
   `Alias`=NA,
   `Domain`,
   `Drug development stage`,
   `Primary Use Case`,
   `Restrictions`,
   `License`,
   `Source of data`=NA,
   `Nature of data`,
   `Refresh Frequency`=NA,
   `Community`
){
   stopifnot(
      TKCat::is_KMR(kmr), is.MDB(x)
   )
   if(any(c("Collibra", "Collibra_drug_dev_stage") %in% names(x))){
      stop("Collibra reserved table names are already used in the provided MDB")
   }
   kms <- get_km_spec(x, kmr)
   toRet <- x
   
   Collibra <- tibble(
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
   Collibra_drug_dev_stage <- tibble(
      `Drug development stage`=`Drug development stage`
   )
   
   
   dm <- ReDaMoR::df_to_model(Collibra, Collibra_drug_dev_stage)
   dm$Collibra$display$x <- 0
   dm$Collibra_drug_dev_stage$display$x <- 0
   dm$Collibra$display$y <- -60
   dm$Collibra_drug_dev_stage$display$y <- 60
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
      x,
      memoMDB(
         dataTables=list(
            "Collibra"=Collibra,
            "Collibra_drug_dev_stage"=Collibra_drug_dev_stage
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
      name="Collibra", type="collibra",
      features=lapply(
         list_table_features(kmr, "collibra")$feature,
         function(x){
            list(feature=x, fields=sub(" [(]Collibra[)]$", "", x))
         }
      )
   )
   
   toRet <- add_km_table(
      toRet, kmr,
      name="Collibra_drug_dev_stage", type="collibra drug development stage",
      features=lapply(
         list_table_features(kmr, "collibra drug development stage")$feature,
         function(x){
            list(feature=x, fields=sub(" [(]Collibra[)]$", "", x))
         }
      )
   )
   
   return(toRet)
}
