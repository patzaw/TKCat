# Import a collection in a [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md) database

Import a collection in a
[chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md) database

## Usage

``` r
add_chTKCat_collection(x, json, overwrite = FALSE)
```

## Arguments

- x:

  a [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)
  object

- json:

  a single character indicating the collection to import. Can be:

  - a path to a file

  - the name of a local collection (see
    [`list_local_collections()`](https://patzaw.github.io/TKCat/reference/list_local_collections.md))

  - the json text defining the collection

- overwrite:

  a logical indicating if the existing collection should be replaced.

## Value

No return value, called for side effects
