# An [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) (Modeled DataBase) relying on ClickHouse: chMDB

An [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) (Modeled
DataBase) relying on ClickHouse: chMDB

Rename tables of a chMDB object

## Usage

``` r
chMDB(
  tkcon,
  dbTables,
  dbInfo,
  dataModel,
  collectionMembers = NULL,
  check = TRUE,
  n_max = 10,
  verbose = FALSE
)

# S3 method for class 'chMDB'
names(x) <- value

# S3 method for class 'chMDB'
rename(.data, ...)

# S3 method for class 'chMDB'
x[i]

# S3 method for class 'chMDB'
x[[i]]

# S3 method for class 'chMDB'
as.list(x, ...)
```

## Arguments

- tkcon:

  a [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)
  object

- dbTables:

  a named vector of tables in tkcon\$chcon with
  `all(names(dbTables) %in% names(dataModel))`

- dbInfo:

  a list with DB information: **"name"** (only mandatory field),
  "title", "description", "url", "version", "maintainer".

- dataModel:

  a
  [ReDaMoR::RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.html)
  object

- collectionMembers:

  the members of collections as provided to the collection_members\<-
  function (default: NULL ==\> no member).

- check:

  logical: if TRUE (default) the data are confronted to the data model

- n_max:

  maximum number of records to read for checks purpose (default: 10). If
  0, the data are not checked. See also
  [`ReDaMoR::confront_data()`](https://patzaw.github.io/ReDaMoR/reference/confront_data.html).

- verbose:

  if TRUE display the data confrontation report

- x:

  a chMDB object

- value:

  new table names

- .data:

  a chMDB object

- ...:

  additional parameters

- i:

  the index or the name of the tables to take

## Value

A chMDB object

`as.list.chMDB()` returns a simple list of tibbles with all the data
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

- [filter.chMDB](https://patzaw.github.io/TKCat/reference/filter.chMDB.md),
  [slice.chMDB](https://patzaw.github.io/TKCat/reference/slice.chMDB.md)

- [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md),
  [`db_disconnect()`](https://patzaw.github.io/TKCat/reference/db_disconnect.md),
  [`db_reconnect()`](https://patzaw.github.io/TKCat/reference/db_reconnect.md)
