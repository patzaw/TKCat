###############################################################################@
#' Check if the object is  an MDB object
#' 
#' @param x any object
#' 
#' @return A single logical: TRUE if x is an MDB object: [fileMDB]
#' 
#' @export
#'
is.MDB <- function(x){
   inherits(x, "MDB")
}
