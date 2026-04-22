# Show the definition of a collection

This function prints details regarding a collection: title, description
and arguments information. These arguments are those that can be used to
document collection members within an
[MDB](https://patzaw.github.io/TKCat/reference/MDB.md) using the
[`add_collection_member()`](https://patzaw.github.io/TKCat/reference/add_collection_member.md)
function.

## Usage

``` r
show_collection_def(collection, silent = FALSE)
```

## Arguments

- collection:

  a json string with the collection definition as returned by
  [`get_local_collection()`](https://patzaw.github.io/TKCat/reference/get_local_collection.md)

- silent:

  a logical indicating if the definition should be written (TRUE by
  default) or not.

## Value

A list with:

- collection **title**

- collection **description**

- a list of **arguments** for defining collection members as a list of
  elements with:

  - the **type** of the argument element

  - **allowed** values if any

## Examples

``` r
get_local_collection("BE") %>% show_collection_def()
#> BE collection: Collection of biological entity (BE) concepts
#> Arguments (non-mandatory arguments are between parentheses):
#>    - be:
#>       + static: logical
#>       + value: character
#>    - source:
#>       + static: logical
#>       + value: character
#>    - (organism):
#>       + static: logical
#>       + value: character
#>       + type: character in 'Scientific name', 'NCBI taxon identifier'
#>    - identifier:
#>       + static: logical
#>       + value: character
```
