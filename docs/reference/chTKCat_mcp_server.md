# Run a read-only MCP server giving access to a [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md) instance

Start a [Model Context Protocol](https://modelcontextprotocol.io) (MCP)
server communicating through the standard input and output (stdio
transport). The server lets an AI assistant (e.g., Claude Code, Claude
Desktop) explore and query the MDBs available in a chTKCat instance,
relying on their descriptions and data models.

## Usage

``` r
chTKCat_mcp_server(
  host = Sys.getenv("TKCAT_HOST", "localhost"),
  port = as.integer(Sys.getenv("TKCAT_PORT", "9111")),
  user = Sys.getenv("TKCAT_USER", "default"),
  password = Sys.getenv("TKCAT_PASSWORD", ""),
  https = as.logical(Sys.getenv("TKCAT_HTTPS", "FALSE")),
  maxRows = 1000L,
  maxChars = 60000L,
  maxExecutionTime = 300L,
  ...
)
```

## Arguments

- host:

  a character string specifying the host heberging the database
  (default: `TKCAT_HOST` environment variable or "localhost")

- port:

  an integer specifying the port on which the database is listening
  (default: `TKCAT_PORT` environment variable or 9111)

- user:

  user name (default: `TKCAT_USER` environment variable or "default")

- password:

  user password (default: `TKCAT_PASSWORD` environment variable or "")

- https:

  a logical indicating if the HTTPS protocol should be used (default:
  `TKCAT_HTTPS` environment variable or FALSE)

- maxRows:

  maximum number of rows returned by a query (default: 1000)

- maxChars:

  maximum number of characters returned by a tool call (default: 60000).
  Longer outputs are truncated.

- maxExecutionTime:

  maximum execution time of a query in seconds (default: 300)

- ...:

  additional parameters for
  [`chTKCat()`](https://patzaw.github.io/TKCat/reference/chTKCat.md)
  (only the default driver,
  [`ClickHouseHTTP::ClickHouseHTTP()`](https://patzaw.github.io/neo2R/reference/ClickHouseHTTP.html),
  is supported).

## Value

No return value, called for side effects: the function returns when the
input stream is closed by the MCP client.

## Details

The function is not supposed to be called in an interactive R session:
it is launched by the MCP client with a command such as
`Rscript -e "TKCat::chTKCat_mcp_server()"`. See
[`vignette("TKCat-MCP", package = "TKCat")`](https://patzaw.github.io/TKCat/articles/TKCat-MCP.md)
for installation and configuration instructions.

**Read-only access.** The server cannot write, create or delete anything
and cannot modify grants:

- the ClickHouse session is opened with the `readonly = 2` setting: the
  server rejects any query modifying data, tables, databases, users or
  grants, and the `readonly` setting cannot be changed afterwards;

- SQL statements are checked before being sent: only single `SELECT`
  queries (possibly starting with `WITH`) are accepted, without
  `SETTINGS` nor `INTO OUTFILE` clauses and without table functions
  accessing external resources (e.g.,
  [`url()`](https://rdrr.io/r/base/connections.html),
  [`file()`](https://rdrr.io/r/base/connections.html), `s3()`,
  `remote()`);

- the data accessible to the server are limited to those accessible to
  the ClickHouse `user`. Using a dedicated user with limited permissions
  is recommended.

**Available tools.**

- `list_mdbs`: list the MDBs with their description and access level.

- `search_tkcat`: search MDBs, tables and fields by keyword.

- `describe_mdb`: general information, tables and relationships of an
  MDB.

- `describe_tables`: detailed data model of MDB tables (fields, types,
  keys, indexes, collection members).

- `preview_table`: first records of a table or of a matrix.

- `query_mdb`: run a read-only ClickHouse SQL query on an MDB.

- `get_matrix_values`: get values from a matrix table.

- `list_collections`: collections and their members used to integrate
  MDBs.

**Environment variables.** The default values of the connection
parameters are taken from the following environment variables:
`TKCAT_HOST`, `TKCAT_PORT`, `TKCAT_USER`, `TKCAT_PASSWORD` and
`TKCAT_HTTPS`.

## See also

[`chTKCat_mcp_http_server()`](https://patzaw.github.io/TKCat/reference/chTKCat_mcp_http_server.md),
[`chTKCat()`](https://patzaw.github.io/TKCat/reference/chTKCat.md),
[`explore_MDBs()`](https://patzaw.github.io/TKCat/reference/explore_MDBs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## From a terminal (normally launched by the MCP client)
# TKCAT_HOST=localhost TKCAT_PORT=9111 TKCAT_USER=default \
#   Rscript -e "TKCat::chTKCat_mcp_server()"
} # }
```
