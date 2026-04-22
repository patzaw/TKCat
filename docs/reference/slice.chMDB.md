# Subset a [chMDB](https://patzaw.github.io/TKCat/reference/chMDB.md) object according to row position in one table and return a [memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md)

Subset a [chMDB](https://patzaw.github.io/TKCat/reference/chMDB.md)
object according to row position in one table and return a
[memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md)

## Usage

``` r
# S3 method for class 'chMDB'
slice(.data, ..., by = 10^5, .preserve = FALSE)
```

## Arguments

- .data:

  a [chMDB](https://patzaw.github.io/TKCat/reference/chMDB.md) object

- ...:

  a single argument. The name of this argument should be a table name of
  x and the value of this argument should be vector of integers
  corresponding to row indexes.

- by:

  the size of the batch: number of records to slice together (default:
  10^5)

- .preserve:

  not used

## Value

a [memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md) object
