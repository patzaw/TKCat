###############################################################################@
#' Get reference URLs of biological tissues
#' 
#' @param ids tissue identifiers
#' @param reference reference database (default: "Uberon")
#' 
#' @return URLs corresponding to provided ids
#'   
#' @export
#' 
get_tissue_ref_url <- function(ids, reference="Uberon"){
   reference <- match.arg(reference)
   url_templates <- c("Uberon"="https://purl.obolibrary.org/obo/%s")
   return(sprintf(url_templates[reference], ids))
}
