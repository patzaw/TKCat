# List of supported values for an integer or a character property

List of supported values for an integer or a character property

## Usage

``` r
list_property_values(kmr, feature, property)
```

## Arguments

- kmr:

  a [KMR](https://patzaw.github.io/TKCat/reference/create_KMR.md) object

- feature:

  the name of the feature

- property:

  the name of the property

## Value

A [dplyr::tibble](https://dplyr.tidyverse.org/reference/reexports.html)
with value and their description (if available)
