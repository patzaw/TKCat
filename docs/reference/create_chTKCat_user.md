# Create a chTKCat user

Create a chTKCat user

## Usage

``` r
create_chTKCat_user(
  x,
  login,
  password,
  contact,
  admin = FALSE,
  provider = admin
)
```

## Arguments

- x:

  a [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)
  object

- login:

  user login

- password:

  user password (NA ==\> no password)

- contact:

  contact information (can be NA)

- admin:

  a logical indicating if the user is an admin of the chTKCat instance
  (default: TRUE)

- provider:

  a logical indicating if the user is data provider (TRUE) or a data
  consumer (FALSE: default). If admin is set to TRUE provider will be
  set to TRUE

## Value

No return value, called for side effects
