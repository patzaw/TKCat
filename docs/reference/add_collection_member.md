# Add a collection member to an MDB

Add a collection member to an MDB

## Usage

``` r
add_collection_member(x, collection, table, ...)
```

## Arguments

- x:

  an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object

- collection:

  a collection title in
  [`list_local_collections()`](https://patzaw.github.io/TKCat/reference/list_local_collections.md)

- table:

  the table providing the collection member

- ...:

  definition of the collection fields as lists (e.g.
  `be=list(static=TRUE, value="Gene")` or
  `organism=list(static=TRUE, value="Homo sapiens", type="Scientific name")`
  )
