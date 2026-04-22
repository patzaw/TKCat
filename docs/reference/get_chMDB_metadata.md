# Get the metadata of an MDB from a [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md) connection

Get the metadata of an MDB from a
[chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)
connection

## Usage

``` r
get_chMDB_metadata(x, dbName, timestamp = NA)
```

## Arguments

- x:

  a [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)
  object

- dbName:

  the name of the MDB

- timestamp:

  the timestamp of the instance to get. Default=NA: get the current
  version.

## Value

A list with the following elements:

- dbInfo: General information regarding the MDB

- dataModel: The data model

- collectionMembers: Members of different collections

- access: type of access to the MDB

## See also

[get_MDB](https://patzaw.github.io/TKCat/reference/get_MDB.md)
