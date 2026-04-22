# Filter a [chMDB](https://patzaw.github.io/TKCat/reference/chMDB.md) object and return a [memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md)

Filter a [chMDB](https://patzaw.github.io/TKCat/reference/chMDB.md)
object and return a
[memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md)

## Usage

``` r
# S3 method for class 'chMDB'
filter(.data, ..., by = 10^5, .preserve = FALSE)
```

## Arguments

- .data:

  a [chMDB](https://patzaw.github.io/TKCat/reference/chMDB.md) object

- ...:

  each argument should have the name of one of the tables of the
  [chMDB](https://patzaw.github.io/TKCat/reference/chMDB.md) object and
  contain a simple logical expression involving the names of the
  corresponding table.

- by:

  the size of the batch: number of records to filter together (default:
  10^5)

- .preserve:

  not used

## Value

a [memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md) object
