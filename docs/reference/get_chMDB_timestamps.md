# Get instance timestamps of an MDB in [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)

Get instance timestamps of an MDB in
[chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)

## Usage

``` r
get_chMDB_timestamps(x, name)
```

## Arguments

- x:

  a [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)
  object

- name:

  the name of the database

## Value

A tibble with the instance "timestamp" and a logical indicating if it's
the "current" one or not.
