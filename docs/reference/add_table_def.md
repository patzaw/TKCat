# Add a table definition to Knowledge Management Requirements (KMR)

Add a table definition to Knowledge Management Requirements (KMR)

## Usage

``` r
add_table_def(
  kmr,
  name,
  description,
  collection = as.character(NA),
  mandatory_features
)
```

## Arguments

- kmr:

  a [KMR](https://patzaw.github.io/TKCat/reference/create_KMR.md) object

- name:

  the name of the table type

- description:

  description of the table type

- collection:

  the name of the collection of which this table type must be a member
  (default: NA)

- mandatory_features:

  a character vector with mandatory features for this table type

## Value

The modified KMR [MDB](https://patzaw.github.io/TKCat/reference/MDB.md)
object
