# Manage user information in a shiny interface

Manage user information in a shiny interface

## Usage

``` r
manage_chTKCat_users(x, pwdFile = NULL)
```

## Arguments

- x:

  a [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)
  object

- pwdFile:

  a local file in which the password for x can be found. If NULL
  (default), the connection is shared by all sessions and can be
  disabled at some point.
