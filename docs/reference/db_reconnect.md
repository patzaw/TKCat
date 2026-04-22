# Reconnect an object to a database

Reconnect an object to a database

## Usage

``` r
# S3 method for class 'POK'
db_reconnect(x, user, password, ntries = 3, ...)

# S3 method for class 'chMDB'
db_reconnect(x, user, password, ntries = 3, ...)

# S3 method for class 'chTKCat'
db_reconnect(x, user, password, ntries = 3, ...)

db_reconnect(x, user, password, ntries = 3, ...)

# S3 method for class 'metaMDB'
db_reconnect(x, user, password, ntries = 3, ...)
```

## Arguments

- x:

  an object with a database connection

- user:

  user name. If not provided, it's taken from x

- password:

  user password. If not provided, first the function tries to connect
  without any password.If it fails, the function asks the user to
  provide a password.

- ntries:

  the number of times the user can enter a wrong password (default: 3)

- ...:

  additional parameters for methods

## Value

A new database connection object.
