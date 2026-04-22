# Subset a [metaMDB](https://patzaw.github.io/TKCat/reference/metaMDB.md) object according to row position in one table

Subset a [metaMDB](https://patzaw.github.io/TKCat/reference/metaMDB.md)
object according to row position in one table

## Usage

``` r
# S3 method for class 'metaMDB'
slice(.data, ..., .preserve = FALSE)
```

## Arguments

- .data:

  a [metaMDB](https://patzaw.github.io/TKCat/reference/metaMDB.md)
  object

- ...:

  a single argument. The name of this argument should be a table name of
  x and the value of this argument should be vector of integers
  corresponding to row indexes.

- .preserve:

  not used

## Value

a [memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md) object
