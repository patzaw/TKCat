# Filter a [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md) object and return a [memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md)

The filtering expressions are evaluated by the arrow package when
reading the parquet files: only the relevant records are loaded in R.
When an expression is not supported by arrow, it is applied in R on
successive parts of the file. The order of the records in the filtered
tables is not guaranteed.

## Usage

``` r
# S3 method for class 'parquetMDB'
filter(.data, ..., .preserve = FALSE)
```

## Arguments

- .data:

  a [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md)
  object

- ...:

  each argument should have the name of one of the tables of the
  [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md)
  object and contain a simple logical expression involving the names of
  the corresponding table.

- .preserve:

  not used

## Value

a [memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md) object
