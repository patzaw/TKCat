# Get database hosts

Get database hosts

## Usage

``` r
# S3 method for class 'DBIConnection'
get_hosts(x, ...)

# S3 method for class 'chMDB'
get_hosts(x, ...)

# S3 method for class 'chTKCat'
get_hosts(x, ...)

get_hosts(x, ...)

# S3 method for class 'metaMDB'
get_hosts(x, ...)
```

## Arguments

- x:

  an object with database connection(s)

- ...:

  additional parameters for methods.

## Value

A character vector with hosts information (generaly 1) in the following
shape: "host:port"
