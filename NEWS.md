<!----------------------------------------------------------------------------->
<!----------------------------------------------------------------------------->
## Version 0.5.0

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