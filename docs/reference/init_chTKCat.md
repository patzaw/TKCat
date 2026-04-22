# Initialize a chTKCat database

The initialization can only be done locally (host="localhost")

## Usage

``` r
init_chTKCat(x, instance, version, path, login, password, contact)
```

## Arguments

- x:

  a [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)
  object

- instance:

  instance name of the database

- version:

  version name of the database

- path:

  path to ClickHouse folder

- login:

  login of the primary administrator of the database

- password:

  password for the primary administrator of the database

- contact:

  contact information for the primary administrator of the database

## Value

a [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)
