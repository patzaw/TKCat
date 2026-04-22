# Create ClickHouse MergeTree tables from a [ReDaMoR::RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.html)

Create ClickHouse MergeTree tables from a
[ReDaMoR::RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.html)

## Usage

``` r
mergeTrees_from_RelDataModel(con, dbName, dbm)
```

## Arguments

- con:

  the clickhouse connection

- dbName:

  the name of the database in which the tables should be written

- dbm:

  a
  [ReDaMoR::RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.html)
  object

## Value

No return value, called for side effects
