# List available [MDB](https://patzaw.github.io/TKCat/reference/MDB.md)

List available [MDB](https://patzaw.github.io/TKCat/reference/MDB.md)

## Usage

``` r
# S3 method for class 'TKCat'
list_MDBs(x, withInfo = TRUE)

# S3 method for class 'chTKCat'
list_MDBs(x, withInfo = TRUE)

list_MDBs(x, withInfo = TRUE)
```

## Arguments

- x:

  a [TKCat](https://patzaw.github.io/TKCat/reference/TKCat.md) related
  object (e.g.
  [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md))

- withInfo:

  if TRUE (default), the function returns a table with
  [db_info](https://patzaw.github.io/TKCat/reference/db_info.md). If
  FALSE, it returns only
  [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) names.

## Value

A tibble with information about the
[MDB](https://patzaw.github.io/TKCat/reference/MDB.md) available in a
[TKCat](https://patzaw.github.io/TKCat/reference/TKCat.md) related
object.
