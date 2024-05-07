###############################################################################@
#' Summarize the number of seizures observed in samples
#' 
#' @param x MDB object (by default, the MDB to which the function is attached).
#' 
#' @return A table with Min., 1st Qu., Median, Mean, 3rd Qu. and Max. values
#'   
#' @export
#' 
summarize_seizures <- function(x=THISMDB){
   return(summary(x$samples$seizures))
}
