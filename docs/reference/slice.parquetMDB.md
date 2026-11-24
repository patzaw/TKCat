# Subset a [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md) object according to row position in one table and return a [memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md)

Subset a
[parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md)
object according to row position in one table and return a
[memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md)

## Usage

``` r
# S3 method for class 'parquetMDB'
slice(.data, ..., .preserve = FALSE)
```

## Arguments

- .data:

  a [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md)
  object

- ...:

  a single argument. The name of this argument should be a table name of
  x and the value of this argument should be vector of integers
  corresponding to row indexes.

- .preserve:

  not used

## Value

a [memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md) object
