<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->

## Version 1.4.1

- Parquet files are not memory mapped anymore when read: on Windows, mapped
files could not be overwritten, moved or removed, making
`as_parquetMDB()` fail on metaMDB.
- The *promises* and *markdown* packages are not imported anymore:
*promises* was not used directly and markdown texts are now rendered with
`shiny::markdown()`.

<!----------------------------------------------------------------------------->
## Version 1.4.0

- New MDB implementation relying on [parquet](https://parquet.apache.org/)
files: `parquetMDB()` and `read_parquetMDB()`. A parquetMDB is similar to a
fileMDB but columns are typed according to the data model, the exact dimensions
of the tables are read from the metadata of the files (`dims()`) and filters
are applied when reading the files (`filter()`, `filter_with_tables()`,
`filter_mdb_matrix()`).
- New `as_parquetMDB()` function to write any MDB in parquet files.
Text files of a fileMDB are converted without loading the data in R.
A parquetMDB can be converted with `as_fileMDB()`, `as_memoMDB()` and
`as_chMDB()`.
- New `scan_MDBs()` function scanning a directory for fileMDB and
parquetMDB. `scan_fileMDBs()` is deprecated.
- `read_KMR()` reads KMR written in text or parquet files.
- `data_files()` and `data_file_size()` also support parquetMDB.
- The [arrow](https://arrow.apache.org/docs/r/) package is now imported.
- Tests relying on the testthat package have been added.
- Vignettes: parquetMDB are described in `vignette("TKCat")`.
- Bug fixes:
   - Matrices without any value, row or column are now correctly read from
   text files (fileMDB): `data_tables()`, `heads()`, `filter_mdb_matrix()`,
   `filter_with_tables()` and `as_fileMDB()`.
   - Sparse matrices read from text files keep the last rows and columns
   when they do not have any value.
   - `as_fileMDB()` supports MDB without any table and therefore
   metaMDB without relational table.
   - `filter_mdb_matrix()` keeps the dimensions of sparse matrices
   without any row or column.
   - `dims()` returns the "bytes" column for chMDB without any table.
   - `as_fileMDB()` on a chMDB does not reduce the size of the batch
   after each table anymore.
   - `$.chMDB` is documented with chMDB instead of fileMDB.
- When creating a fileMDB, the first records confronted to the data model are
now read by TKCat the same way as when getting the data.
- **Authorship**: the parquetMDB implementation, the related conversion
functions, `scan_MDBs()`, their documentation and the tests added in this
version were written by Claude (Anthropic's AI model, Claude Code),
under the supervision of the package maintainer.

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->

## Version 1.3.1

- `list_chMDB_timestamps()` now warns when several current timestamps
are recorded for an MDB and keeps the most recent one as the "current"
attribute.
- Vignettes: add a "References" section to `vignette("TKCat-EndToEnd")`.

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->

## Version 1.3.0

- New `chTKCat_mcp_server()` function: a read-only
[MCP](https://modelcontextprotocol.io) server giving AI assistants
(e.g. Claude Code, Claude Desktop) access to the MDBs of a chTKCat instance,
their descriptions and their data models
(see `vignette("TKCat-MCP")`).
- New `chTKCat_mcp_http_server()` function: the same MCP server available
to remote clients through HTTP (Streamable HTTP transport), with users
authenticated by their chTKCat credentials. The vignette documents its
deployment behind nginx.
- Bug correction: single quotes and backslashes in search terms are now
escaped in `search_MDB_tables()` and `search_MDB_fields()` for chTKCat.

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->

## Version 1.2.3

- Bug correction: order of index from datamodel in clickhouse now preserved

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->

## Version 1.2.2

- Export `ch_join_query()` and `ch_union_query()` helpers for building
ClickHouse `JOIN` and `UNION` queries from sub-queries (with roxygen2
documentation).

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->

## Version 1.2.1

- Remove dependency to {base64enc} library.
- Remove dependency to {magrittr}: replace `%>%` with the native R pipe `|>`
(requires R >= 4.1).

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->

## Version 1.2.0

- Move from {getPass} to {askpass} package to get input masked in positron.

### Changes in ClickHouse table creation

- LowCardinality: information taken from field comment
containing '{ch_LowCardinality}'. It only applies to character data.

- ORDER BY:
   - The first index in the table data model
   - if no index, order by primary key
   - if no primary key, order by foreign keys
   - if none of the above take the first column
   - Matrices and Sparse Matrices are ordered as before on rows and/or columns

- INDEX:
   - Use all the indexes but the first one
   - character: TYPE bloom_filter(0.01) GRANULARITY 1
   - numeric or integer: TYPE minmax GRANULARITY 1
   - other data type: skip

- PROJECTION:
   - Projections are created with `materialize_chMDB_projections()`
   - Projections are identifed in the comment of the table data model:
   '{ch_Projection:field1,field2...}'
   
- Large matrices (with more than 1000 columns) are now stored as a long
pivoted table in ClickHouse: filtering is much more efficient this way.


<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->

## Version 1.1.15

- Correction of GRANTS

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->

## Version 1.1.14

- Update queries for compatibility with ClickHouseHTTP version >= 0.3.4
- Better handling of Rstudio hidden calls to `as.list` and `$`

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->

## Version 1.1.13

- Avoid some nested joins in ClickHouse
- Correct query for listing MDBs
- Correct shiny explorer error with not populated MDB
- Update shiny explorer to show host_path in system tab
- Simplification of ClickHouse instantiation

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->

## Version 1.1.12

- In RStudio, avoid calls to `as.list` during autocompletion on `chMDB` and
`fileMDB` objects

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->

## Version 1.1.11

- Fix of missing package anchors in doc

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.1.10

- Correct a bug in nullable sparse matrix in ClickHouse


<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.1.9

- Correct a bug in `manage_chTKCat_users()`

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.1.8

- Correct a bug in time comparison in `as_chMDB()` function

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.1.7

- Documentation of knowledge management tools

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.1.6

- Consider timestamps equal when there are less than 60 seconds between them

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.1.5

- Format size in GB to avoid issue with JS render in DT >= 0.32

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.1.4

- allow "'" character in login

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.1.3

- Correction of `add_collection_member()`

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.1.2

- `has_km_spec()` function
- `db_reconnect()` for knowledge: checks and improvements

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.1.1

- Debug column and row counts for matrices

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.1.0

- Implementation of knowledge management tools

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.0.9

- chMDBs size in bytes
- Improved `dims()` and size information

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.0.8

- Getting chMDB data model only
- Allow additional parameters in `db_reconnect()`
- Improved `format.chTKCat()`

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.0.7

- Allow valid email as login in chTKCat

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.0.6

- Fix notes and warning from CRAN
- Fix overflow in sidebar of the explorer when the MDB name is too long
- Better management of indirect igraph suggest dependency in vignettes

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.0.5

- Alignment on ReDaMoR 0.7.0
changes ([#2](https://github.com/patzaw/ReDaMoR/issues/2) fix)

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.0.4

### Corrections

- Fix warnings when calling icon()

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.0.3

### Corrections

- Bug fix in `add_collection_member()`
- Fix documentation in `explore_MDBs()`

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.0.2

### New features

- `show_collection_members()` provides useful information about how to 
document collection members with the `add_collection_member()` function.

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.0.1

### Implementation changes

- Post-process data from ClickHouseHTTP to speed-up a little their access

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 1.0.0

### New features

- Change default DBI backend to ClickhouseHTTP.
- Allow selecting alternative driver.
The 2 following are supported: `ClickhouseHTTP::ClickhouseHTTP()`
and `RClickhouse::clickhouse()`

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.8.1

### Implementation hanges

- Avoid unnecessary data type conversions

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.8.0

### New features

- Expanding matrix support to the Matrix class from the Matrix package to
support sparse matrices.

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.7.1

### New features

- Checks can be bypassed when creating memoMDB, fileMDB, chMDB and metaMDB
(check=FALSE): it make the creation of the object faster but it should be used
only when the user knows what she/he is doing with the data and the model.

### Implementation changes

- Display in `explore_MDBs()`:

   - maintainer is displayed in MDB table
   - markdown tags maintainer and title are rendered in MDB table

- `list_MDB.chTKCat()` filters DB based on *___MDB___* and *___Public___* tables

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.7.0

### New features

- Support versioning of tables in chMDB using timestamps.
- Support base64 data type.

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.6.2

### New features

- `filter_mdb_matrix()`:  Filter a matrix stored in an MDB 

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.6.1

### New features

- Supporting matrix (ReDaMoR >= 0.5.0)

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.6.0

### New features

- A user can be a data consumer (default) or a data provider
(set the "provider" setting to TRUE). A data provider can create and manage
a database.

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.5.6

- Correcting bug in setting chMDB collections
- More generic implementation of `list_MDBs.chTKCat()` and `list_chMDB_users()`
- Update user information
- Better management of GRANTs
- settings management when connecting and re-connecting
- Installation and initialization of ClickHouse is easier
- Shiny interface for user settings (contact and password)

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.5.5

- Correcting bug during initialization of collections in chTKCat
- Use `DBI::dbAppendTable()` instead of `DBI::dbWriteTable()`
- Better display of access rights in `explore_MDBs()`
- Tested with Clickhouse 21.4.6

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.5.4

Corrections for CRAN

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.5.1

- `verbose` option in `read_fileMDB()`
- Mentioning `get_confrontation_report()` in man pages.

<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.5.0

### R

- Complete refactoring of code to make it more generic and flexible

### DESCRIPTION.json file

- `readr::read_delim()` parameters are now provided within this file and
named accordingly. A few default values are handled:
   - **delim delimiter**: `"\\t"`
   - **quoted_na**: `FALSE`

### Built-in collections

- BE-Collection.json has been updated:
    - **"\$schema"** is now required: `"TKCat_BE_collection_1.0"`
    - **\$id"** is now required
- Condition_Collection.json has been updated
    - **"\$schema"** is now required: `"TKCat_Condition_collection_1.0"`
    - **"\$id"** is now required
    - **"condition"** is now a required property of **"fields"**
    (in addition to "source" and "identifier")
