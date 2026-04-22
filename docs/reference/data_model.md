# Get object data model

Get object data model

## Usage

``` r
# S3 method for class 'chMDB'
data_model(x, ...)

# S3 method for class 'fileMDB'
data_model(x, ...)

data_model(x, ...)

# S3 method for class 'memoMDB'
data_model(x, ...)

# S3 method for class 'metaMDB'
data_model(x, rtOnly = FALSE, recursive = FALSE, ...)
```

## Arguments

- x:

  an object with an embedded data model

- ...:

  method specific parameters

- rtOnly:

  if TRUE, the function only returns the relational tables and the
  corresponding foreign tables (default: FALSE)

- recursive:

  if TRUE and rtOnly, the function returns also the relational tables
  from embedded metaMDBs.

## Value

A
[ReDaMoR::RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.html)
object
