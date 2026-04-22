# Filter a [metaMDB](https://patzaw.github.io/TKCat/reference/metaMDB.md) object

Filter a [metaMDB](https://patzaw.github.io/TKCat/reference/metaMDB.md)
object

## Usage

``` r
# S3 method for class 'metaMDB'
filter(.data, ..., .preserve = FALSE)
```

## Arguments

- .data:

  a [metaMDB](https://patzaw.github.io/TKCat/reference/metaMDB.md)
  object

- ...:

  each argument should have the name of one of the tables of the
  [metaMDB](https://patzaw.github.io/TKCat/reference/metaMDB.md) object
  and contain a simple logical expression involving the names of the
  corresponding table.

- .preserve:

  not used

## Value

a filtered
[memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md) object
