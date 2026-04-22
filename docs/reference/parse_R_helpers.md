# Parse source code to get R helpers

Parse source code to get R helpers

## Usage

``` r
parse_R_helpers(code, ...)
```

## Arguments

- code:

  the source code as a character vector

- ...:

  other objects to add in the environment of the returned functions

## Value

A list of functions from `code` plus an "help" function used to get
function documentation.

## Details

Functions in code must be documented with
[roxygen2::roxygen2-package](https://roxygen2.r-lib.org/reference/roxygen2-package.html)
tags and only functions with the '@export' tag are returned.

## Examples

``` r
{

code <- "
   a <- 2
   #' Set the 'a' value to use in the add_a function
   #'
   #' @param v a numeric value
   #'
   #' @return Does not return anything
   #'
   #' @export
   set_a <- function(v) a <<- v

   #' Add a 'a' value defined separately
   #'
   #' @param x a numeric value
   #'
   #' @return x + a
   #'
   #' @export
   add_a <- function(x) x + a

   #' Add a 'a' value defined separately to a b value made available
   #' in environment
   #'
   #'
   #' @return b + a
   #'
   #' @export
   add_a_to_b <- function() b + a
"
helpers <- parse_R_helpers(code, b=3)
helpers$help()
helpers$help("add_a")
helpers$add_a(3.5)
helpers$set_a(4)
helpers$add_a(3.5)
helpers$add_a_to_b()
helpers <- parse_R_helpers(code, b=6)
helpers$add_a_to_b()

}
#> - set_a: Set the 'a' value to use in the add_a function
#> - add_a: Add a 'a' value defined separately
#> - add_a_to_b: Add a 'a' value defined separately to a b value made available
#> in environment
#> - help: Display documentation regarding an helper function
#> add_a: Add a 'a' value defined separately
#> 
#> Usage: add_a(x)
#> 
#> Parameters:
#>   - x: a numeric value
#> 
#> Result: x + a
#> [1] 8
```
