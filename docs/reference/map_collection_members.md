# Map different collection members

Map different collection members

## Usage

``` r
map_collection_members(
  x,
  y,
  collection,
  xm,
  ym,
  suffix = c("_x", "_y"),
  fun = NA,
  ...
)
```

## Arguments

- x:

  a data.frame

- y:

  a data.frame

- collection:

  the name of the collection.

- xm:

  collection member x: a data.frame with the fields "field", "static",
  "value", "type" as returned by the
  [`read_collection_members()`](https://patzaw.github.io/TKCat/reference/read_collection_members.md)
  function.

- ym:

  collection member y: a data.frame with the fields "field", "static",
  "value", "type" as returned by the
  [`read_collection_members()`](https://patzaw.github.io/TKCat/reference/read_collection_members.md)
  function.

- suffix:

  the suffix to append to field names from x and y tables. Default:
  `c("_x", "_y")`

- fun:

  the function used to map x and y collection members. By default (NA)
  it is automatically identified if recorded in the system. The way to
  write this function is provided in the details section.

- ...:

  additional parameters for the fun function.

## Value

A tibble giving necessary information to map elements in x and y. The
columns corresponds to the field values in xm and ym followed by a
suffix (default: `c("_x", "_y")`). Only fields documented as non static
in xm and ym are kept.

## Details

fun must have at least an x and a y parameters. Each of them should be a
data.frame with all the field values given in xm and ym. Additional
parameters can be defined and will be forwarded using `...`. fun should
return a data frame with all the fields values given in xm and ym
followed by "\_x" and "\_y" suffix.
