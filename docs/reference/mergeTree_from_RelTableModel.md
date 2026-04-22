# Create a ClickHouse MergeTree table from a [ReDaMoR::RelTableModel](https://patzaw.github.io/ReDaMoR/reference/RelTableModel.html)

Create a ClickHouse MergeTree table from a
[ReDaMoR::RelTableModel](https://patzaw.github.io/ReDaMoR/reference/RelTableModel.html)

## Usage

``` r
mergeTree_from_RelTableModel(con, dbName, tm)
```

## Arguments

- con:

  the clickhouse connection

- dbName:

  the name of the database in which the table should be written

- tm:

  a
  [ReDaMoR::RelTableModel](https://patzaw.github.io/ReDaMoR/reference/RelTableModel.html)
  object

## Value

No return value, called for side effects
