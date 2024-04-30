###############################################################################@
#' Encode a binary file in a base64 string
#' 
#' @param what a file path or a raw vector
#' 
#' @return A character vector of length 1 with the base64 encoded file
#' 
#' @export
#' 
#' 
encode_bin <- function(what){
   base64enc::base64encode(what)
}

###############################################################################@
#' Decode base64 string
#' 
#' @param text the base64 character vector to decode
#' 
#' @return One decoded value (e.g. a raw vector corresponding to a binary file)
#' 
#' @export
#' 
decode_bin <- function(text){
   base64enc::base64decode(paste(unlist(text), collapse=""))
}

###############################################################################@
#' Parse source code to get R helpers
#' 
#' @param code the source code as a character vector
#' @param ... other objects to add in the environment of the returned functions
#' 
#' @return A list of functions from `code` plus an "help" function used to get
#' function documentation.
#' 
#' @details Functions in code must be documented
#' with [roxygen2::roxygen2-package]
#' tags and only functions with the '@export' tag are returned.
#' 
#' @examples {
#' 
#' code <- "
#'    a <- 2
#'    #' Set the 'a' value to use in the add_a function
#'    #' 
#'    #' @param v a numeric value
#'    #' 
#'    #' @return Does not return anything
#'    #' 
#'    #' @export
#'    set_a <- function(v) a <<- v
#'    
#'    #' Add a 'a' value defined separately
#'    #' 
#'    #' @param x a numeric value
#'    #' 
#'    #' @return x + a
#'    #' 
#'    #' @export
#'    add_a <- function(x) x + a
#'    
#'    #' Add a 'a' value defined separately to a b value made available
#'    #' in environment
#'    #' 
#'    #' 
#'    #' @return b + a
#'    #' 
#'    #' @export
#'    add_a_to_b <- function() b + a
#' "
#' helpers <- parse_R_helpers(code, b=3)
#' helpers$help()
#' helpers$help("add_a")
#' helpers$add_a(3.5)
#' helpers$set_a(4)
#' helpers$add_a(3.5)
#' helpers$add_a_to_b()
#' helpers <- parse_R_helpers(code, b=6)
#' helpers$add_a_to_b()
#' 
#' }
#' 
#' @export
#' 
parse_R_helpers <- function(code, ...){
   add_obj <- list(...)
   for(on in names(add_obj)){
      assign(on, add_obj[[on]])
   }
   eval(parse(text=code))
   rdoc <- roxygen2::parse_text(code, env=environment())
   rdoc <- do.call(c, lapply(rdoc, function(d){
      toRet <- list()
      for(i in 1:length(d$tags)){
         tag <- d$tags[[i]]
         toRet[[tag$tag]] <- c(
            toRet[[tag$tag]],
            list(tag$val)
         )
      }
      toChange <- lapply(
         toRet[setdiff(names(toRet), "param")],
         function(x) if(length(x)==1) unlist(x) else x
      )
      toRet[names(toChange)] <- toChange
      toRet <- magrittr::set_names(list(toRet), d$object$alias)
   }))
   
   ## help function ----
   if(!"help" %in% names(rdoc)){
      help <- function(f=NULL){
         if(length(f) > 1){
            stop("Only one function name can be provided")
         }
         if(length(f)==0){
            toShow <- paste(paste0(
               "- ",
               crayon::bold(names(rdoc)),
               unlist(lapply(rdoc, function(x){
                  if("title" %in% names(x)){
                     paste(":", x$title)
                  }else{
                     ""
                  }
               }))
            ), collapse="\n")
         }else{
            if(!f %in% names(rdoc)){
               stop("There isn't any helper with this name")
            }
            fdoc <- rdoc[[f]]
            toShow <- paste0(
               "\n",
               crayon::bgBlue(crayon::white(crayon::bold(f)))
            )
            if("title" %in% names(fdoc)){
               toShow <- paste0(
                  toShow, ": ", fdoc$title, "\n"
               )
            }else{
               toShow <- paste0(toShow, "\n")
            }
            if("usage" %in% names(fdoc)){
               toShow <- paste0(
                  toShow, "\n",
                  crayon::bgCyan("Usage"), ": ",
                  fdoc$usage,
                  "\n"
               )
            }
            if("param" %in% names(fdoc)){
               params <- fdoc$param
               toShow <- paste0(
                  toShow,
                  "\n",
                  crayon::bgCyan("Parameters"), ":\n"
               )
               for(i in 1:length(params)){
                  toShow <- paste0(
                     toShow,
                     "  - ", crayon::underline(params[[i]]$name), ": ",
                     params[[i]]$description,
                     "\n"
                  )
               }
               
            }
            if("return" %in% names(fdoc)){
               toShow <- paste0(
                  toShow,
                  "\n",
                  crayon::bgCyan("Result"), ": ", fdoc$return
               )
            }
            toShow <- paste0(toShow, "\n")
         }
         cat(toShow)
      }
      rdoc$help <- list(
         "title"="Display documentation regarding an helper function",
         "param"=list(
            list(
               name="f",
               description=paste(
                  'The name of the helper function to document.',
                  'If NULL (default), list all the available functions.'
               )
            )
         ),
         "return"=paste(
            "This function displays the documentation of an helper function",
            "and does not return anything"
         ),
         "import"=c("crayon"),
         "export"=""
      )
   }
   
   ## Check dependencies ----
   depends <- unique(unlist(lapply(rdoc, function(fd) fd$import)))
   missingDependencies <- which(!nzchar(mapply(system.file, package=depends)))
   if(length(missingDependencies!= 0)){
      warning(
         "The following packages are not available",
         " and some helpers might not work:\n",
         paste(paste("   -", depends[missingDependencies]), collapse="\n")
      )
   }
   
   ## To export ----
   toExport <- names(rdoc)[which(
      unlist(lapply(rdoc, function(x) "export" %in% names(x)))
   )]
   helpers <- list()
   for(fn in toExport){
      helpers[[fn]] <- get(fn)
   }
   # names(helpers) <- toExport
   rdoc <- rdoc[toExport]
   return(helpers)
}
