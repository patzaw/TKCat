# Create a piece of knowledge (POK) from an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) and a [KMR](https://patzaw.github.io/TKCat/reference/create_KMR.md)object

Create a piece of knowledge (POK) from an
[MDB](https://patzaw.github.io/TKCat/reference/MDB.md) and a
[KMR](https://patzaw.github.io/TKCat/reference/create_KMR.md)object

## Usage

``` r
create_POK(mdb, kmr, tkcat = NULL)
```

## Arguments

- mdb:

  a [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object with
  KM specifications

- kmr:

  a [KMR](https://patzaw.github.io/TKCat/reference/create_KMR.md) object
  with KM requirements

- tkcat:

  A [TKCat](https://patzaw.github.io/TKCat/reference/TKCat.md) or
  [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md) object
  to make available in helper environment

## Value

A POK object: a list with 3 slots:

- \$mdb: the provided
  [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object

- \$kmr: the provided
  [KMR](https://patzaw.github.io/TKCat/reference/create_KMR.md) object

- \$helpers: a list functions to leverage data from mdb and kmr
