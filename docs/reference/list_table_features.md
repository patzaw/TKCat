# List the features provided by a set of tables

List the features provided by a set of tables

## Usage

``` r
list_table_features(kmr, tables = NULL)
```

## Arguments

- kmr:

  a [KMR](https://patzaw.github.io/TKCat/reference/create_KMR.md) object

- tables:

  the name of the tables. If NULL (default), all the features are
  listed.

## Value

A [dplyr::tibble](https://dplyr.tidyverse.org/reference/reexports.html)
with feature description and properties
