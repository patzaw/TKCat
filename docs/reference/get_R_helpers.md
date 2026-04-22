# Get a set of helper functions from an object

Get a set of helper functions from an object

## Usage

``` r
# S3 method for class 'MDB'
get_R_helpers(x, hnames = NA, kmr, tkcat = NULL, ...)

# S3 method for class 'KMR'
get_R_helpers(x, hnames = NA, tkcat = NULL, mdb = NULL, ...)

get_R_helpers(x, hnames, ...)
```

## Arguments

- x:

  an object with helpers

- hnames:

  the names of the helper sets. If NA (default), all available are
  sourced.

- kmr:

  a [KMR](https://patzaw.github.io/TKCat/reference/create_KMR.md) object

- tkcat:

  A [TKCat](https://patzaw.github.io/TKCat/reference/TKCat.md) or
  [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md) object
  to make available in helper environment

- ...:

  method specific parameters

- mdb:

  An [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object to
  make available in helper environment

## Value

Return a list of functions

## Details

x, kmr and tkcat objects are made available in helpers environment as
'THISMDB', 'THISKMR' and 'THISTKCAT' objects respectively and can be
used as such within helpers code.

x, tkcat and mdb objects are made available in helpers environment as
'THISKMR', 'THISTKCAT' and 'THISMDB' objects respectively and can be
used as such within helpers code.
