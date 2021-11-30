<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.7.1

### New features

- Checks can be bypassed when creating memoMDB, fileMDB, chMDB and metaMDB
(check=FALSE): it make the creation of the object faster but it should be used
only when the user knows what she/he is doing with the data and the model.

### Implementation hanges

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
