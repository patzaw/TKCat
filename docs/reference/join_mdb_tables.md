# Join connected tables

Join connected tables

## Usage

``` r
join_mdb_tables(
  x,
  ...,
  type = c("left", "right", "inner", "full"),
  jtName = NA
)
```

## Arguments

- x:

  an MDB object

- ...:

  at least 2 names of tables to join

- type:

  the type of join among:

  - `"left"`: includes all rows of the first provided table

  - `"right"`: includes all rows of the last provided table

  - `"inner"`: includes all rows in all provided tables

  - `"full"`: includes all rows in at least one provide table

- jtName:

  the name of the joint. IF NA (default), the name is then the name is
  the first provided table name.

## Value

A [metaMDB](https://patzaw.github.io/TKCat/reference/metaMDB.md)
corresponding to x with the joined tables replaced by the joint. If less
than 2 table names are provided, the function returns the original x
MDB.
