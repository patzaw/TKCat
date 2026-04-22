# List tables in a clickhouse database

List tables in a clickhouse database

## Usage

``` r
# S3 method for class 'DBIConnection'
list_tables(x, dbNames = NULL, ...)

# S3 method for class 'chTKCat'
list_tables(x, dbNames = NULL, ...)

list_tables(x, ...)
```

## Arguments

- x:

  an object with a clickhouse connection

- dbNames:

  the name of databases to focus on (default NULL ==\> all)

- ...:

  method specific parameters

## Value

A tibble with at least the following columns:

- **database**: the name of the database

- **name**: the name of the table

- **total_rows**: the number of rows in the table

- **total_bytes**: the size of the table

- **total_columns**: the number of columns in the table
