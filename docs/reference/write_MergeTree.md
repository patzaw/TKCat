# Write a Clickhouse [MergeTree](https://clickhouse.com/docs/en/engines/table-engines/mergetree-family/mergetree/) table

Write a Clickhouse
[MergeTree](https://clickhouse.com/docs/en/engines/table-engines/mergetree-family/mergetree/)
table

## Usage

``` r
write_MergeTree(
  con,
  dbName,
  tableName,
  value,
  rtypes = NULL,
  nullable = NULL,
  lowCardinality = NULL,
  sortKey = NULL,
  indexes = NULL
)
```

## Arguments

- con:

  the clickhouse connection

- dbName:

  the name of the database

- tableName:

  the name of the table

- value:

  the table to import

- rtypes:

  a named character vector giving the R type of each and every columns.
  If NULL (default), types are guessed from value.

- nullable:

  a character vector indicating the name of the columns which are
  nullable (default: NULL)

- lowCardinality:

  a character vector indicating the name of the columns with low
  cardinality (default: NULL)

- sortKey:

  a character vector indicating the name of the columns used in the sort
  key. If NULL (default), all the non-nullable columns are used in the
  key.

- indexes:

  a data.frame with 3 columns:

  - idx: index name,

  - field: field name,

  - type: 'bloom_filter(0.01)', 'minmax'... (see
    https://clickhouse.com/docs/optimize/skipping-indexes)

  - granularity: index granularity

## Value

No return value, called for side effects
