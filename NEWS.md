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
