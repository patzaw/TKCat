# Filter a [memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md) object

Filter a [memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md)
object

## Usage

``` r
# S3 method for class 'memoMDB'
filter(.data, ..., .preserve = FALSE)
```

## Arguments

- .data:

  a [memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md)
  object

- ...:

  each argument should have the name of one of the tables of the
  [memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md) object
  and contain a simple logical expression involving the names of the
  corresponding table.

- .preserve:

  not used

## Value

a filtered
[memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md) object
