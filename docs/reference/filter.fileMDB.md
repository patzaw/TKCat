# Filter a [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) object and return a [memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md)

Filter a [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md)
object and return a
[memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md)

## Usage

``` r
# S3 method for class 'fileMDB'
filter(.data, ..., .preserve = FALSE)
```

## Arguments

- .data:

  a [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md)
  object

- ...:

  each argument should have the name of one of the tables of the
  [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) object
  and contain a simple logical expression involving the names of the
  corresponding table.

- .preserve:

  not used

## Value

a [memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md) object
