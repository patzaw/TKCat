# Connect to a ClickHouse TKCat instance

Connect to a ClickHouse TKCat instance

## Usage

``` r
chTKCat(
  host = "localhost",
  port = 9111L,
  user = "default",
  password,
  settings = list(max_query_size = 1073741824, use_uncompressed_cache = 0, load_balancing
    = "random", max_memory_usage = 0, allow_introspection_functions = 1,
    joined_subquery_requires_alias = 0, max_table_size_to_drop = 0),
  ports = NULL,
  drv = ClickHouseHTTP::ClickHouseHTTP(),
  ...
)
```

## Arguments

- host:

  a character string specifying the host heberging the database
  (default: localhost)

- port:

  an integer specifying the port on which the database is listening
  (default: 9111)

- user:

  user name

- password:

  user password

- settings:

  list of [Clickhouse
  settings](https://clickhouse.com/docs/en/operations/settings/settings/)

- ports:

  a named list of available ports for accessing ClickHouse (default:
  NULL; example: `c(Native=9101, HTTP=9111)`)

- drv:

  a DBI driver for connecting to ClickHouse (default:
  [`ClickHouseHTTP::ClickHouseHTTP()`](https://rdrr.io/pkg/ClickHouseHTTP/man/ClickHouseHTTP.html);
  other supported driver:
  [`RClickhouse::clickhouse()`](https://rdrr.io/pkg/RClickhouse/man/ClickhouseDriver-class.html))

- ...:

  additional parameters for connection (see
  [ClickHouseHTTP::dbConnect,ClickHouseHTTPDriver-method](https://rdrr.io/pkg/ClickHouseHTTP/man/ClickHouseHTTPDriver-class.html)
  for the default driver)

## Value

a chTKCat object

## See also

[`check_chTKCat()`](https://patzaw.github.io/TKCat/reference/check_chTKCat.md),
[`db_disconnect()`](https://patzaw.github.io/TKCat/reference/db_disconnect.md),
[`db_reconnect()`](https://patzaw.github.io/TKCat/reference/db_reconnect.md)
