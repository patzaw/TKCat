# Write a collection member JSON file

Write a collection member JSON file

## Usage

``` r
write_collection_members(colMembers, path = NA, collection = NULL)
```

## Arguments

- colMembers:

  A tibble as returned by
  [`read_collection_members()`](https://patzaw.github.io/TKCat/reference/read_collection_members.md)

- path:

  the JSON file to write. If `NA` (default), the JSON file is not
  written but returned by the function.

- collection:

  The collection definition (json string). If NULL (default), it is
  taken from TKCat environment (see
  [`list_local_collections()`](https://patzaw.github.io/TKCat/reference/list_local_collections.md).

## Value

The JSON representation of collection members. If a path is provided,
then the JSON is also written in it.
