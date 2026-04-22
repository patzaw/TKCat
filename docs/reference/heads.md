# Get the first records of each object data tables

Get the first records of each object data tables

## Usage

``` r
# S3 method for class 'chMDB'
heads(x, ..., n = 6L)

# S3 method for class 'fileMDB'
heads(x, ..., n = 6L)

heads(x, ..., n = 6L)

# S3 method for class 'memoMDB'
heads(x, ..., n = 6L)

# S3 method for class 'metaMDB'
heads(x, ..., n = 6L)
```

## Arguments

- x:

  an object with embedded data tables

- ...:

  the name of the tables to get (default: all of them)

- n:

  maximum number of records to return (default: 6)

## Value

A list of
[dplyr::tibble](https://dplyr.tidyverse.org/reference/reexports.html)
and [matrix](https://rdrr.io/r/base/matrix.html)
