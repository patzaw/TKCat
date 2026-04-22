# MDB

The class "MDB" provides general functions for handling modeled
databases. The MDB classes implemented in the TKCat package are:
[fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md),
[memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md), chMDB
and [metaMDB](https://patzaw.github.io/TKCat/reference/metaMDB.md).
These classes provide additional functions.

## Usage

``` r
# S3 method for class 'MDB'
names(x)

# S3 method for class 'MDB'
length(x)

# S3 method for class 'MDB'
lengths(x, use.names = TRUE)

# S3 method for class 'MDB'
as.list(x, ...)

# S3 method for class 'MDB'
select(.data, ...)

# S3 method for class 'MDB'
pull(.data, var = -1, name = NULL, ...)

# S3 method for class 'MDB'
c(...)

# S3 method for class 'MDB'
merge(
  x,
  y,
  by = get_shared_collections(x, y),
  dbInfo = list(name = paste(db_info(x)$name, db_info(y)$name, sep = "_")),
  dmAutoLayout = TRUE,
  rtColor = "yellow",
  funs = list(),
  ...
)
```

## Arguments

- x:

  an MDB object

- use.names:

  return the names of the tables

- ...:

  additional parameters

- .data:

  an MDB object

- var:

  a variable specified as in
  [dplyr::pull](https://dplyr.tidyverse.org/reference/pull.html)

- name:

  not used but kept for compatibility with the generic function

- y:

  an MDB object

- by:

  a tibble as returned by the
  [`get_shared_collections()`](https://patzaw.github.io/TKCat/reference/get_shared_collections.md)
  function which indicates which collection members should be merged
  through a relational table. If the collection is `NA`, the relational
  table is built by merging identical columns in table.x and table.y. If
  the collection is provided, the relational table is build using the
  [`map_collection_members()`](https://patzaw.github.io/TKCat/reference/map_collection_members.md)
  function.

- dbInfo:

  a list with DB information: **"name"** (only mandatory field),
  "title", "description", "url", "version", "maintainer".

- dmAutoLayout:

  if TRUE (default) the layout of the merged data model is automatically
  adjusted.

- rtColor:

  the color of the relational tables in the merged data model (default:
  "yellow")

- funs:

  a named list of functions (default: list()). If there is no function
  for mapping a collection in this list, it is taken automatically using
  the
  [`get_collection_mapper()`](https://patzaw.github.io/TKCat/reference/get_collection_mapper.md)
  function.

## Value

[`names()`](https://rdrr.io/r/base/names.html) returns the table names.

[`length()`](https://rdrr.io/r/base/length.html) returns the number of
tables in x.

[`lengths()`](https://rdrr.io/r/base/lengths.html) returns the number of
fields for each table in x.

`as.list.MDB()` returns a simple list of tibbles with all the data from
the tables in x.

A [metaMDB](https://patzaw.github.io/TKCat/reference/metaMDB.md) object
gathering x and y along with relational tables between them created
using collection members and mapping functions automatically chosen or
provided by the `funs` parameter. `...` can be used to send parameters
to the mapper functions.

## See also

MDB methods:
[db_info](https://patzaw.github.io/TKCat/reference/db_info.md),
[data_model](https://patzaw.github.io/TKCat/reference/data_model.md),
[data_tables](https://patzaw.github.io/TKCat/reference/data_tables.md),
[collection_members](https://patzaw.github.io/TKCat/reference/collection_members.md),
[count_records](https://patzaw.github.io/TKCat/reference/count_records.md),
[filter_with_tables](https://patzaw.github.io/TKCat/reference/filter_with_tables.md),
[as_fileMDB](https://patzaw.github.io/TKCat/reference/as_fileMDB.md)
Additional documentation is provided for each specific class:
[fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md),
[memoMDB](https://patzaw.github.io/TKCat/reference/memoMDB.md), chMDB
and [metaMDB](https://patzaw.github.io/TKCat/reference/metaMDB.md).
