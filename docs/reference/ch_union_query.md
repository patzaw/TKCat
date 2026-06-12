# Build a ClickHouse `UNION` query from several sub-queries

Wraps each input sub-query as a derived table and combines them with
`UNION DISTINCT` or `UNION ALL`. A single sub-query is returned wrapped
but without any `UNION` clause.

## Usage

``` r
ch_union_query(queries, mode = c("DISTINCT", "ALL"))
```

## Arguments

- queries:

  a character vector of `SELECT` sub-queries to combine

- mode:

  the union mode: `"DISTINCT"` (default) to deduplicate rows across
  sub-queries, or `"ALL"` to keep every row

## Value

A character string with the assembled `UNION` query.

## See also

[`ch_join_query()`](https://patzaw.github.io/TKCat/reference/ch_join_query.md)
