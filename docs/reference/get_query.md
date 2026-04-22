# Get SQL query

Get SQL query

## Usage

``` r
# S3 method for class 'chMDB'
get_query(x, query, autoalias = TRUE, ...)

# S3 method for class 'chTKCat'
get_query(x, query, ...)

get_query(x, query, ...)
```

## Arguments

- x:

  an object with a database connection

- query:

  the SQL query

- autoalias:

  Change this parameter only if you know what you're doing. if TRUE,
  make relevant alias to query the chMDB using the table names from the
  data model. If FALSE, the user must know the table instance name in
  the remote database. By default, autoalias is set to TRUE.

- ...:

  method specific parameters

## Value

A tibble with query results
