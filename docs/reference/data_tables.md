# Get object data tables

Get object data tables

## Usage

``` r
# S3 method for class 'chMDB'
data_tables(x, ..., skip = 0, n_max = Inf)

# S3 method for class 'fileMDB'
data_tables(x, ..., skip = 0, n_max = Inf)

data_tables(x, ..., skip = 0, n_max = Inf)

# S3 method for class 'memoMDB'
data_tables(x, ..., skip = 0, n_max = Inf)

# S3 method for class 'metaMDB'
data_tables(x, ..., skip = 0, n_max = Inf)
```

## Arguments

- x:

  an object with embedded data tables

- ...:

  the name of the tables to get (default: all of them)

- skip:

  the number of rows to skip (default: 0)

- n_max:

  maximum number of rows to return (default: Inf)

## Value

A list of
[dplyr::tibble](https://dplyr.tidyverse.org/reference/reexports.html)
and [matrix](https://rdrr.io/r/base/matrix.html)
