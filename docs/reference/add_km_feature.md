# Add KM feature specifications to an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object

Add KM feature specifications to an
[MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object

## Usage

``` r
add_km_feature(x, kmr, table, feature, fields, unit = as.character(NA))
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

  the name of an existing feature in kmr

- fields:

  Either a single character providing the name of an existing field in
  table or a list named with feature property names from kmr. Each
  element of the list should provide a "field" slot with the name of the
  corresponding field and a "unit" slot with the name of the unit if
  relevant.

- unit:

  a single character providing the unit if relevant. Unit information
  provided in fields override this parameter value.

## Value

An [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object with
additional KM table feature specification
