# Insert records by batches in a Clickhouse table

Insert records by batches in a Clickhouse table

## Usage

``` r
ch_insert(con, dbName, tableName, value, by = 10^6)
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

- by:

  the size of the batch: number of records to import together (default:
  10^6)

## Value

No return value, called for side effects
