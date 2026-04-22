# List users of an MDB of a [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md) object

List users of an MDB of a
[chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md) object

## Usage

``` r
list_chMDB_users(x, mdbs = NULL)
```

## Arguments

- x:

  a [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)
  object

- mdbs:

  names of the modeled databases. If NULL (default), all the databases
  are considered.

## Value

A tibble with 3 columns:

- user: the user login

- mdb: the name of the modeled database

- admin: if the user is an admin of the MDB
