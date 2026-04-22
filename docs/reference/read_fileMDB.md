# Read a [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) from a path

Read a [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md)
from a path

## Usage

``` r
read_fileMDB(
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

  - **data**: a folder with the data

  - **DESCRIPTION.json**: a file with db information

  - **model**: a folder with the data model json file with the same name
    as the one given in the DESCRIPTION.json file

- dbInfo:

  a list or a json file with DB information: **"name"** (only mandatory
  field), "title", "description", "url" (or "reference URL"), "version",
  "maintainer". If NULL (default), the DESCRIPTION.json file found in
  path. This file should also contains relevant parameters for the
  [`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html)
  function. For example:

  - **delim delimiter** (default: '\\t')

  - **quoted_na**: Should missing values inside quotes be treated as
    missing values or as strings or strings. WARNING: THIS PARAMETER IS
    NOT TAKEN INTO ACCOUNT WITH readr\>=2.0.0.

  - **na**: String used for missing values. The default value for
    reading a fileMDB is "NA". But the default value for writing a
    fileMDB is "\<NA\>". This value is written in the DESCRIPTION.json
    file to avoid ambiguity when reading the fileMDB.

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

A [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) object

## See also

[get_confrontation_report](https://patzaw.github.io/TKCat/reference/get_confrontation_report.md),
[ReDaMoR::format_confrontation_report](https://patzaw.github.io/ReDaMoR/reference/format_confrontation_report.html)
and
[ReDaMoR::format_confrontation_report_md](https://patzaw.github.io/ReDaMoR/reference/format_confrontation_report_md.html)
for getting and formatting the report confronting the data to the model.
