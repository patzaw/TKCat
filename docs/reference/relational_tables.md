# Get a list of relational tables

Get a list of relational tables

## Usage

``` r
relational_tables(x, recursive = FALSE)
```

## Arguments

- x:

  a [metaMDB](https://patzaw.github.io/TKCat/reference/metaMDB.md)
  object

- recursive:

  if TRUE, function returns also the relational tables from embedded
  metaMDBs.

## Value

A list of relational tables (tibbles)
