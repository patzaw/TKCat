# Get the DB tables from a [chMDB](https://patzaw.github.io/TKCat/reference/chMDB.md) or [metaMDB](https://patzaw.github.io/TKCat/reference/metaMDB.md) object

Get the DB tables from a
[chMDB](https://patzaw.github.io/TKCat/reference/chMDB.md) or
[metaMDB](https://patzaw.github.io/TKCat/reference/metaMDB.md) object

## Usage

``` r
db_tables(x, host)
```

## Arguments

- x:

  a [chMDB](https://patzaw.github.io/TKCat/reference/chMDB.md) or a
  [metaMDB](https://patzaw.github.io/TKCat/reference/metaMDB.md) object

- host:

  the name of host (as returned by `[get_hosts]`) to focus on. Only used
  with [metaMDB](https://patzaw.github.io/TKCat/reference/metaMDB.md)
  objects.

## Value

a list with a chTKCat object (tkcon) and a named vector of DB table
names (dbTables).
