# List instance timestamps of an MDB in [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)

List instance timestamps of an MDB in
[chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)

## Usage

``` r
list_chMDB_timestamps(x, name)
```

## Arguments

- x:

  a [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)
  object

- name:

  the name of the database

## Value

A tibble with the instance of each table at each timestamp. The
"current" attribute indicate the current timestamp instance. If there is
no recorded timestamp, the function returns NULL.
