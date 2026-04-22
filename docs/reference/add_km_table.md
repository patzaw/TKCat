# Add KM table specifications to an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object

Add KM table specifications to an
[MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object

## Usage

``` r
add_km_table(x, kmr, name, type, features = list())
```

## Arguments

- x:

  an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object to
  update with specification tables

- kmr:

  an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object with
  KM requirements

- name:

  the name of an existing table in x

- type:

  the name of an existing table type in kmr

- features:

  a list of feature definitions. Each element of a list is a list of
  parameters for the
  [`add_km_feature()`](https://patzaw.github.io/TKCat/reference/add_km_feature.md)
  function.

## Value

An [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object with
additional KM table specification
