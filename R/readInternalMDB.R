#' Read files and create an internal database with a documented data model
#'
#' @param dataModel a \code{\link{RelDataModel}} object
#' @param descriptionFile a json file with DB information
#' @param directory a path to the directory where to read the data
#' @param ext the file extension to consider (default: "ext"),
#' @param delim delimiter (default: '\\t')
#' @param quoted_na Should missing values inside quotes be treated
#' as missing values or as strings (the default).
#' @param ... additional  parameters for the \code{\link{read_delim}} function
#'
#'
#' @return An \code{\link{internalMDB}} object
#'
#' @importFrom readr read_tsv
#' @importFrom jsonlite read_json
#' @export
#'
readInternalMDB <- function(
   dataModel,
   descriptionFile,
   directory,
   ext='txt',
   delim='\t',
   quoted_na=FALSE,
   ...
){

   ## Checks ----
   stopifnot(
      is.RelDataModel(dataModel),
      is.character(directory),
      length(directory)==1,
      !is.na(directory),
      length(descriptionFile)==1,
      file.exists(descriptionFile)
   )
   
   ## Read description
   dbInfo <- read_json(descriptionFile)
   stopifnot(
      is.character(dbInfo$name), length(dbInfo$name)==1,
      is.character(dbInfo$title), length(dbInfo$title)==1,
      is.character(dbInfo$description), length(dbInfo$description)==1,
      is.character(dbInfo$`reference URL`), length(dbInfo$`reference URL`)==1,
      is.character(dbInfo$version), length(dbInfo$version)==1
   )
   names(dbInfo) <- sub("^reference URL$", "url", names(dbInfo))
   dbInfo <- dbInfo[c("name", "title", "description", "url", "version")]
   
   ## Read Tables ----
   availableTables <- sub(
      paste0(".", ext, "$"),
      "",
      list.files(path=directory, pattern=paste0(".", ext, "$"))
   )
   misTables <- setdiff(names(dataModel), availableTables)
   if(length(misTables)>0){
      stop(
         "The following tables were not found in the directory: ",
         paste(misTables, collapse=", ")
      )
   }
   supTables <- setdiff(availableTables, names(dataModel))
   if(length(supTables)>0){
      warning(
         "The following files in the directory are not part of ",
         "the data model and won't be read: ",
         paste(supTables, collapse=", ")
      )
   }

   toRet <- list()
   for(tn in names(dataModel)){
      message(sprintf ("Loading the %s table", tn))
      d <- read_delim(
         file=file.path(directory, paste(tn, ext, sep=".")),
         delim=delim,
         quoted_na=quoted_na,
         col_types=col_types(dataModel[[tn]]),
         ...
      )
      tft <- dataModel[[tn]]$fields
      tf <- tft$name
      misFields <- setdiff(tf, colnames(d))
      if(length(misFields)>0){
         stop(
            sprintf(
               "The following fields were not found in the %s table: ",
               tn
            ),
            paste(misFields, collapse=", ")
         )
      }
      supFields <- setdiff(colnames(d), tf)
      if(length(supFields)>0){
         warning(
            sprintf(
               "The following fields are not defined for the %s table",
               tn
            ),
            " and won't be taken into account: ",
            paste(misFields, collapse=", ")
         )
      }
      d <- d[, tf]
      checkTable(d, dataModel[[tn]])
      toRet[[tn]] <- d
   }
   toRet <- internalMDB(
      dataModel=dataModel, dbTables=toRet,
      dbInfo=dbInfo,
      checkTables=FALSE
   )
   return(toRet)

}
