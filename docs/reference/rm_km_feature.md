# Remove KM feature specifications from an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object

Remove KM feature specifications from an
[MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object

## Usage

``` r
rm_km_feature(x, kmr, table, feature)
```

## Arguments

- x:

  an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object to
  update with specification tables

- kmr:

  an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object with
  KM requirements

- table:

  the name of an existing table in x

- feature:

  the name of a feature with specification in x table

## Value

An [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object with
relevant KM table feature specification removed
