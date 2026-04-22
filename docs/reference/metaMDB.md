# A metaMDB object

A metaMDB object is an
[MDB](https://patzaw.github.io/TKCat/reference/MDB.md) gathering several
other MDBs glued by relational tables.

## Usage

``` r
metaMDB(MDBs, relationalTables, dataModel, dbInfo, check = TRUE)

# S3 method for class 'metaMDB'
names(x) <- value

# S3 method for class 'metaMDB'
rename(.data, ...)

# S3 method for class 'metaMDB'
x[i]

# S3 method for class 'metaMDB'
x[[i]]

# S3 method for class 'metaMDB'
x$i

# S3 method for class 'metaMDB'
as.list(x, ...)
```

## Arguments

- MDBs:

  a list of [MDB](https://patzaw.github.io/TKCat/reference/MDB.md)
  objects

- relationalTables:

  a list of tibbles corresponding to the relational tables between the
  different MDBs

- dataModel:

  a
  [ReDaMoR::RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.html)
  object gathering all the data model of all the MDBs plus the
  relational tables

- dbInfo:

  a list with DB information: **"name"** (only mandatory field),
  "title", "description", "url", "version", "maintainer".

- check:

  logical: if TRUE (default) the data are confronted to the data model

- x:

  a metaMDB object

- value:

  new table names

- .data:

  a metaMDB object

- ...:

  additional parameters

- i:

  the index or the name of the tables to take

## Value

A metaMDB object

`as.list.metaMDB()` returns a simple list of tibbles with all the data
from the tables in x.

## See also

- MDB methods:
  [db_info](https://patzaw.github.io/TKCat/reference/db_info.md),
  [data_model](https://patzaw.github.io/TKCat/reference/data_model.md),
  [data_tables](https://patzaw.github.io/TKCat/reference/data_tables.md),
  [collection_members](https://patzaw.github.io/TKCat/reference/collection_members.md),
  [count_records](https://patzaw.github.io/TKCat/reference/count_records.md),
  [dims](https://patzaw.github.io/TKCat/reference/dims.md),
  [filter_with_tables](https://patzaw.github.io/TKCat/reference/filter_with_tables.md),
  [as_fileMDB](https://patzaw.github.io/TKCat/reference/as_fileMDB.md)

- Additional general documentation is related to
  [MDB](https://patzaw.github.io/TKCat/reference/MDB.md).

- [filter.metaMDB](https://patzaw.github.io/TKCat/reference/filter.metaMDB.md),
  [slice.metaMDB](https://patzaw.github.io/TKCat/reference/slice.metaMDB.md)

- [get_confrontation_report](https://patzaw.github.io/TKCat/reference/get_confrontation_report.md),
  [ReDaMoR::format_confrontation_report](https://patzaw.github.io/ReDaMoR/reference/format_confrontation_report.html)
  and
  [ReDaMoR::format_confrontation_report_md](https://patzaw.github.io/ReDaMoR/reference/format_confrontation_report_md.html)
  for getting and formatting the report confronting the data to the
  model.
