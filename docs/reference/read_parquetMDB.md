# Read a [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md) from a path

Read a
[parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md)
from a path

## Usage

``` r
read_parquetMDB(
  path,
  dbInfo = NULL,
  dataModel = NULL,
  collectionMembers = NULL,
  check = TRUE,
  n_max = 10,
  verbose = TRUE
)
```

## Arguments

- path:

  the path to a folder with data or with the following structure:

  - **data**: a folder with the data: one parquet file per table
    (*table_name.parquet*) or one directory per matrix stored in a long
    format (see
    [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md))

  - **DESCRIPTION.json**: a file with db information

  - **model**: a folder with the data model json file with the same name
    as the one given in the DESCRIPTION.json file

- dbInfo:

  a list or a json file with DB information: **"name"** (only mandatory
  field), "title", "description", "url" (or "reference URL"), "version",
  "maintainer". If NULL (default), the DESCRIPTION.json file found in
  path.

- dataModel:

  a
  [ReDaMoR::RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.html)
  object or json file. If NULL (default), the model json file found in
  path/model.

- collectionMembers:

  the members of collections as provided to the collection_members\<-
  function. If NULL (default), the members are taken from json files
  found in path/model/Collections

- check:

  logical: if TRUE (default) the data are confronted to the data model

- n_max:

  maximum number of records to read for checks purpose (default: 10).
  See also
  [`ReDaMoR::confront_data()`](https://patzaw.github.io/ReDaMoR/reference/confront_data.html).

- verbose:

  if TRUE (default) display the data confrontation report

## Value

A [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md)
object

## See also

[get_confrontation_report](https://patzaw.github.io/TKCat/reference/get_confrontation_report.md),
[ReDaMoR::format_confrontation_report](https://patzaw.github.io/ReDaMoR/reference/format_confrontation_report.html)
and
[ReDaMoR::format_confrontation_report_md](https://patzaw.github.io/ReDaMoR/reference/format_confrontation_report_md.html)
for getting and formatting the report confronting the data to the model.
