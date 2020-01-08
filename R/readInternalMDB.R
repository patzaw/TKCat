#' Read files and create an internal database with a documented data model
#'
#' @param dataModel a [RelDataModel] object
#' @param descriptionFile a json file with DB information
#' @param directory a path to the directory where to read the data
#' @param verbose if TRUE display the data confrontation report
#' @param cr_variable_name a character with a single value indicating the name
#' of the variable in the .GlobalEnv environment where to put the data
#' confrontation report. If it's not a character with a single
#' value (default: NULL), then the confrontation report is not exported.
#' @param ext the file extension to consider (default: "ext"),
#' @param delim delimiter (default: '\\\\t')
#' @param quoted_na Should missing values inside quotes be treated
#' as missing values or as strings or strings (the default).
#' Be aware that the default value here is different than the one for the
#' original [readr::read_delim()] function.
#' @param ... additional  parameters for the [readr::read_delim()] function
#'
#'
#' @return An [internalMDB] object
#'
#' @importFrom readr read_tsv
#' @importFrom jsonlite read_json
#' @export
#'
readInternalMDB <- function(
   dataModel,
   descriptionFile,
   directory,
   verbose=FALSE,
   cr_variable_name=NULL,
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
   cr <- ReDaMoR::confront_data(
      dataModel,
      paths=list.files(
         path=directory,
         pattern=paste0(".", ext, "$"),
         full.names=TRUE
      ),
      returnData=TRUE,
      verbose=verbose
   )
   if(
      is.character(cr_variable_name) &&
      length(cr_variable_name)==1 &&
      !is.na(cr_variable_name)
   ){
      assign(cr_variable_name, cr[-which(names(cr)=="data")], envir=.GlobalEnv)
   }
   if(!cr$success){
      stop(ReDaMoR::format_confrontation_report(cr, title=dbInfo[["name"]]))
   }
   if(verbose){
      cat(format_confrontation_report(cr, title=dbInfo[["name"]]))
   }
   toRet <- internalMDB(
      dataModel=dataModel, dbTables=cr$data,
      dbInfo=dbInfo,
      checks=c(),
      verbose=FALSE
   )
   return(toRet)

}
