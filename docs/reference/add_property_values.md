# Add possible values to an integer or a character feature property in [KMR](https://patzaw.github.io/TKCat/reference/create_KMR.md)

Add possible values to an integer or a character feature property in
[KMR](https://patzaw.github.io/TKCat/reference/create_KMR.md)

## Usage

``` r
add_property_values(kmr, feature, property, values)
```

## Arguments

- kmr:

  a [KMR](https://patzaw.github.io/TKCat/reference/create_KMR.md) object

- feature:

  the name of the feature type

- property:

  the name of the property

- values:

  a vector of character or integer or a named vector with the
  description of the values

## Value

The modified KMR [MDB](https://patzaw.github.io/TKCat/reference/MDB.md)
object
