# Changelog

## Version 1.2.0

- Move from {getPass} to {askpass} package to get input masked in
  positron.

### Changes in table creation

- LowCardinality: information taken from field comment containing
  ‘{ch_LowCardinality}’. It only applies to character data.

- ORDER BY:

  - The first index in the table data model
  - if no index, order by primary key
  - if no primary key, order by foreign keys
  - if none of the above take the first column
  - Matrices and Sparse Matrices are ordered as before on rows and/or
    columns

- INDEX:

  - Use all the indexes but the first one
  - character: TYPE bloom_filter(0.01) GRANULARITY 1
  - numeric or integer: TYPE minmax GRANULARITY 1
  - other data type: skip

## Version 1.1.15

- Correction of GRANTS

## Version 1.1.14

CRAN release: 2025-06-05

- Update queries for compatibility with ClickHouseHTTP version \>= 0.3.4
- Better handling of Rstudio hidden calls to `as.list` and `$`

## Version 1.1.13

- Avoid some nested joins in ClickHouse
- Correct query for listing MDBs
- Correct shiny explorer error with not populated MDB
- Update shiny explorer to show host_path in system tab
- Simplification of ClickHouse instantiation

## Version 1.1.12

CRAN release: 2025-03-17

- In RStudio, avoid calls to `as.list` during autocompletion on `chMDB`
  and `fileMDB` objects

## Version 1.1.11

CRAN release: 2024-07-03

- Fix of missing package anchors in doc

## Version 1.1.10

- Correct a bug in nullable sparse matrix in ClickHouse

## Version 1.1.9

- Correct a bug in
  [`manage_chTKCat_users()`](https://patzaw.github.io/TKCat/reference/manage_chTKCat_users.md)

## Version 1.1.8

CRAN release: 2024-05-23

- Correct a bug in time comparison in
  [`as_chMDB()`](https://patzaw.github.io/TKCat/reference/as_chMDB.md)
  function

## Version 1.1.7

CRAN release: 2024-05-08

- Documentation of knowledge management tools

## Version 1.1.6

- Consider timestamps equal when there are less than 60 seconds between
  them

## Version 1.1.5

- Format size in GB to avoid issue with JS render in DT \>= 0.32

## Version 1.1.4

- allow “’” character in login

## Version 1.1.3

- Correction of
  [`add_collection_member()`](https://patzaw.github.io/TKCat/reference/add_collection_member.md)

## Version 1.1.2

- [`has_km_spec()`](https://patzaw.github.io/TKCat/reference/has_km_spec.md)
  function
- [`db_reconnect()`](https://patzaw.github.io/TKCat/reference/db_reconnect.md)
  for knowledge: checks and improvements

## Version 1.1.1

- Debug column and row counts for matrices

## Version 1.1.0

- Implementation of knowledge management tools

## Version 1.0.9

- chMDBs size in bytes
- Improved [`dims()`](https://patzaw.github.io/TKCat/reference/dims.md)
  and size information

## Version 1.0.8

- Getting chMDB data model only
- Allow additional parameters in
  [`db_reconnect()`](https://patzaw.github.io/TKCat/reference/db_reconnect.md)
- Improved
  [`format.chTKCat()`](https://patzaw.github.io/TKCat/reference/format.chTKCat.md)

## Version 1.0.7

CRAN release: 2023-02-16

- Allow valid email as login in chTKCat

## Version 1.0.6

CRAN release: 2022-10-21

- Fix notes and warning from CRAN
- Fix overflow in sidebar of the explorer when the MDB name is too long
- Better management of indirect igraph suggest dependency in vignettes

## Version 1.0.5

- Alignment on ReDaMoR 0.7.0 changes
  ([\#2](https://github.com/patzaw/ReDaMoR/issues/2) fix)

## Version 1.0.4

### Corrections

- Fix warnings when calling icon()

## Version 1.0.3

CRAN release: 2022-06-07

### Corrections

- Bug fix in
  [`add_collection_member()`](https://patzaw.github.io/TKCat/reference/add_collection_member.md)
- Fix documentation in
  [`explore_MDBs()`](https://patzaw.github.io/TKCat/reference/explore_MDBs.md)

## Version 1.0.2

### New features

- `show_collection_members()` provides useful information about how to
  document collection members with the
  [`add_collection_member()`](https://patzaw.github.io/TKCat/reference/add_collection_member.md)
  function.

## Version 1.0.1

### Implementation changes

- Post-process data from ClickHouseHTTP to speed-up a little their
  access

## Version 1.0.0

### New features

- Change default DBI backend to ClickhouseHTTP.
- Allow selecting alternative driver. The 2 following are supported:
  `ClickhouseHTTP::ClickhouseHTTP()` and
  [`RClickhouse::clickhouse()`](https://rdrr.io/pkg/RClickhouse/man/ClickhouseDriver-class.html)

## Version 0.8.1

### Implementation hanges

- Avoid unnecessary data type conversions

## Version 0.8.0

### New features

- Expanding matrix support to the Matrix class from the Matrix package
  to support sparse matrices.

## Version 0.7.1

CRAN release: 2022-01-17

### New features

- Checks can be bypassed when creating memoMDB, fileMDB, chMDB and
  metaMDB (check=FALSE): it make the creation of the object faster but
  it should be used only when the user knows what she/he is doing with
  the data and the model.

### Implementation changes

- Display in
  [`explore_MDBs()`](https://patzaw.github.io/TKCat/reference/explore_MDBs.md):

  - maintainer is displayed in MDB table
  - markdown tags maintainer and title are rendered in MDB table

- `list_MDB.chTKCat()` filters DB based on ****MDB**** and
  ****Public**** tables

## Version 0.7.0

### New features

- Support versioning of tables in chMDB using timestamps.
- Support base64 data type.

## Version 0.6.2

### New features

- [`filter_mdb_matrix()`](https://patzaw.github.io/TKCat/reference/filter_mdb_matrix.md):
  Filter a matrix stored in an MDB

## Version 0.6.1

### New features

- Supporting matrix (ReDaMoR \>= 0.5.0)

## Version 0.6.0

### New features

- A user can be a data consumer (default) or a data provider (set the
  “provider” setting to TRUE). A data provider can create and manage a
  database.

## Version 0.5.6

- Correcting bug in setting chMDB collections
- More generic implementation of
  [`list_MDBs.chTKCat()`](https://patzaw.github.io/TKCat/reference/list_MDBs.md)
  and
  [`list_chMDB_users()`](https://patzaw.github.io/TKCat/reference/list_chMDB_users.md)
- Update user information
- Better management of GRANTs
- settings management when connecting and re-connecting
- Installation and initialization of ClickHouse is easier
- Shiny interface for user settings (contact and password)

## Version 0.5.5

- Correcting bug during initialization of collections in chTKCat
- Use
  [`DBI::dbAppendTable()`](https://dbi.r-dbi.org/reference/dbAppendTable.html)
  instead of
  [`DBI::dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html)
- Better display of access rights in
  [`explore_MDBs()`](https://patzaw.github.io/TKCat/reference/explore_MDBs.md)
- Tested with Clickhouse 21.4.6

## Version 0.5.4

CRAN release: 2021-03-04

Corrections for CRAN

## Version 0.5.1

- `verbose` option in
  [`read_fileMDB()`](https://patzaw.github.io/TKCat/reference/read_fileMDB.md)
- Mentioning
  [`get_confrontation_report()`](https://patzaw.github.io/TKCat/reference/get_confrontation_report.md)
  in man pages.

## Version 0.5.0

### R

- Complete refactoring of code to make it more generic and flexible

### DESCRIPTION.json file

- [`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html)
  parameters are now provided within this file and named accordingly. A
  few default values are handled:
  - **delim delimiter**: `"\\t"`
  - **quoted_na**: `FALSE`

### Built-in collections

- BE-Collection.json has been updated:
  - **“\$schema”** is now required: `"TKCat_BE_collection_1.0"`
  - **\$id”** is now required
- Condition_Collection.json has been updated
  - **“\$schema”** is now required: `"TKCat_Condition_collection_1.0"`
  - **“\$id”** is now required
  - **“condition”** is now a required property of **“fields”** (in
    addition to “source” and “identifier”)
