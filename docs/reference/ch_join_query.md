# Build a ClickHouse `JOIN` query from two sub-queries

Wraps two SQL sub-queries in a single `SELECT ... JOIN ... USING ...`
statement. Both sub-queries are used as derived tables, so they can be
any valid `SELECT` statements.

## Usage

``` r
ch_join_query(q1, q2, using, type = "LEFT", select = "*", a1 = "l", a2 = "r")
```

## Arguments

- q1:

  a character string with the left-hand side `SELECT` sub-query

- q2:

  a character string with the right-hand side `SELECT` sub-query

- using:

  a character string with the column(s) to join on, as expected after
  the `USING` keyword (e.g. `"id"` or `"(id, version)"`)

- type:

  the join type, passed verbatim before the `JOIN` keyword (e.g.
  `"LEFT"`, `"INNER"`, `"FULL"`, `"ANY LEFT"`). Default: `"LEFT"`.

- select:

  a character string with the columns to select. Default: `"*"`. Columns
  can be qualified with the sub-query aliases (see `a1`/`a2`) to
  disambiguate non-key columns present on both sides.

- a1:

  alias given to the left-hand side sub-query. Default: `"l"`.

- a2:

  alias given to the right-hand side sub-query. Default: `"r"`.

## Value

A character string with the assembled `JOIN` query.

## See also

[`ch_union_query()`](https://patzaw.github.io/TKCat/reference/ch_union_query.md)
