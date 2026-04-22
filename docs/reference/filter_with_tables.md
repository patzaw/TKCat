# Filter an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object according to provided tables

Filter an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object
according to provided tables

## Usage

``` r
# S3 method for class 'chMDB'
filter_with_tables(x, tables, checkTables = TRUE, by = 10^5, ...)

# S3 method for class 'fileMDB'
filter_with_tables(x, tables, checkTables = TRUE, by = 10^5, ...)

filter_with_tables(x, tables, checkTables = TRUE, ...)

# S3 method for class 'memoMDB'
filter_with_tables(x, tables, checkTables = TRUE, ...)

# S3 method for class 'metaMDB'
filter_with_tables(x, tables, checkTables = TRUE, ...)
```

## Arguments

- x:

  an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object

- tables:

  a named list of tibbles to filter with. The names should correspond to
  the table names in x and the tibbles should fit the data model.

- checkTables:

  if TRUE, the tables are confronted to their model in the data model of
  x.

- by:

  the size of the batch: number of lines to process together (default:
  10000)

- ...:

  method specific parameters

## Value

a [memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md) object
