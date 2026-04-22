# Add a feature definition to Knowledge Management Requirements (KMR)

Add a feature definition to Knowledge Management Requirements (KMR)

## Usage

``` r
add_feature_def(kmr, name, description, properties)
```

## Arguments

- kmr:

  a [KMR](https://patzaw.github.io/TKCat/reference/create_KMR.md) object

- name:

  the name of the feature type

- description:

  description of the feature type

- properties:

  properties of the feature. A list named with property names. For each
  property, a list with:

  - **type**: among "integer", "numeric", "logical", "character",
    "Date", "POSIXct", "base64", "table" and "field" ("table" and
    "field" type are used for referencing tables and fields; the other
    types come from ReDaMoR).

  - **description** (optional): a description of the feature property.
    Useful when the feature has more than one property

  - **mandatory**: a logical indicating the property is mandatory for
    the feature

  - **measurement** (optional and only for "integer" and "numeric"
    types): the name of the measurement for checking units

## Value

The modified KMR [MDB](https://patzaw.github.io/TKCat/reference/MDB.md)
object
