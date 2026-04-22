# Get an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object from a [TKCat](https://patzaw.github.io/TKCat/reference/TKCat.md) related object

Get an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object
from a [TKCat](https://patzaw.github.io/TKCat/reference/TKCat.md)
related object

## Usage

``` r
# S3 method for class 'TKCat'
get_MDB(x, dbName, ...)

# S3 method for class 'chTKCat'
get_MDB(x, dbName, timestamp = NA, check = TRUE, n_max = 10, ...)

get_MDB(x, dbName, ...)
```

## Arguments

- x:

  a [TKCat](https://patzaw.github.io/TKCat/reference/TKCat.md) related
  object (e.g.
  [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md))

- dbName:

  the name of the database

- ...:

  method specific parameters

- timestamp:

  the timestamp of the instance to get. Default=NA: get the current
  version.

- check:

  logical: if TRUE (default) the data are confronted to the data model

- n_max:

  maximum number of records to read for checks purpose (default: 10).
  See also
  [`ReDaMoR::confront_data()`](https://patzaw.github.io/ReDaMoR/reference/confront_data.html).

## Value

An [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object

## See also

[get_confrontation_report](https://patzaw.github.io/TKCat/reference/get_confrontation_report.md),
[ReDaMoR::format_confrontation_report](https://patzaw.github.io/ReDaMoR/reference/format_confrontation_report.html)
and
[ReDaMoR::format_confrontation_report_md](https://patzaw.github.io/ReDaMoR/reference/format_confrontation_report_md.html)
for getting and formatting the report confronting the data to the model.
