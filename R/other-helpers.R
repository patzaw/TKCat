#' Format bytes numbers in human readable values 
#' 
#' @param bytes a vector of integers
#' @return A vector of character with human readable size
#' 
.format_bytes <- function(bytes){
   sunits <- c("B", "KB", "MB", "GB", "TB", "PB", "EB")
   uniti <- log2(bytes)%/%10
   uniti <- ifelse(
      uniti > (length(sunits)-1),
      length(sunits)-1,
      uniti
   )
   toRetHR <- lapply(
      1:length(bytes),
      function(i){
         format(
            bytes[i]/(2^(10*uniti[i])),
            digit=1,
            nsmall=ifelse(uniti[i]==0, 0, 1)
         )
      }
   )
   paste(toRetHR, sunits[uniti+1])
}
