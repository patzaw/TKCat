# Subset a [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) object according to row position in one table and return a [memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md)

Subset a [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md)
object according to row position in one table and return a
[memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md)

## Usage

``` r
# S3 method for class 'fileMDB'
slice(.data, ..., .preserve = FALSE)
```

## Arguments

- .data:

  a [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md)
  object

- ...:

  a single argument. The name of this argument should be a table name of
  x and the value of this argument should be vector of integers
  corresponding to row indexes.

- .preserve:

  not used

## Value

a [memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md) object
