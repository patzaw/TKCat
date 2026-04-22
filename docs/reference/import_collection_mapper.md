# Import a function to map collection members

Import a function to map collection members

## Usage

``` r
import_collection_mapper(collection, fun)
```

## Arguments

- collection:

  the name of the targeted collection (it should belong to local
  collections: see
  [`list_local_collections()`](https://patzaw.github.io/TKCat/reference/list_local_collections.md)).

- fun:

  a function which takes 2 data.frames (x an y) with fields described in
  the collection definition and map the different elements.

## Value

No return value, called for side effects. The function will be used to
map collection members.
