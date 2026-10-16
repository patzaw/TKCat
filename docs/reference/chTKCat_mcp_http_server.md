# Run a read-only MCP server giving remote access to a [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md) instance

Start a [Model Context Protocol](https://modelcontextprotocol.io) (MCP)
server available through HTTP (Streamable HTTP transport) to give remote
MCP clients (e.g., Claude Code) access to the MDBs of a chTKCat
instance. The server provides the same read-only tools as
[`chTKCat_mcp_server()`](https://patzaw.github.io/TKCat/reference/chTKCat_mcp_server.md).

## Usage

``` r
chTKCat_mcp_http_server(
  mcpHost = Sys.getenv("TKCAT_MCP_HOST", "127.0.0.1"),
  mcpPort = as.integer(Sys.getenv("TKCAT_MCP_PORT", "8800")),
  mcpPath = "/mcp",
  host = Sys.getenv("TKCAT_HOST", "localhost"),
  port = as.integer(Sys.getenv("TKCAT_PORT", "9111")),
  https = as.logical(Sys.getenv("TKCAT_HTTPS", "FALSE")),
  anonymousUser = Sys.getenv("TKCAT_MCP_ANONYMOUS_USER", NA),
  anonymousPassword = Sys.getenv("TKCAT_MCP_ANONYMOUS_PASSWORD", ""),
  allowedOrigins = NULL,
  maxRows = 1000L,
  maxChars = 60000L,
  maxExecutionTime = 300L,
  maxBodySize = 2^20,
  cacheTime = 3600,
  ...
)
```

## Arguments

- mcpHost:

  the address on which the MCP server listens (default: `TKCAT_MCP_HOST`
  environment variable or "127.0.0.1"). Use "0.0.0.0" to listen on all
  the interfaces.

- mcpPort:

  the port on which the MCP server listens (default: `TKCAT_MCP_PORT`
  environment variable or 8800)

- mcpPath:

  the path of the MCP endpoint (default: "/mcp")

- host:

  a character string specifying the host heberging the chTKCat database
  (default: `TKCAT_HOST` environment variable or "localhost")

- port:

  an integer specifying the port on which the database is listening
  (default: `TKCAT_PORT` environment variable or 9111)

- https:

  a logical indicating if the HTTPS protocol should be used to connect
  to the database (default: `TKCAT_HTTPS` environment variable or FALSE)

- anonymousUser:

  the ClickHouse user to use for requests without `Authorization` header
  (default: `TKCAT_MCP_ANONYMOUS_USER` environment variable or NA ==\>
  authentication is required). An empty string is equivalent to NA.

- anonymousPassword:

  the password of `anonymousUser` (default:
  `TKCAT_MCP_ANONYMOUS_PASSWORD` environment variable or "")

- allowedOrigins:

  requests with an `Origin` header are only accepted if this header is
  in this vector of origins (e.g., "https://tkcat.example.com").
  Default: NULL ==\> requests with an `Origin` header are rejected (MCP
  clients other than web browsers do not send this header).

- maxRows:

  maximum number of rows returned by a query (default: 1000)

- maxChars:

  maximum number of characters returned by a tool call (default: 60000).
  Longer outputs are truncated.

- maxExecutionTime:

  maximum execution time of a query in seconds (default: 300)

- maxBodySize:

  maximum size of a request body in bytes (default: 1048576)

- cacheTime:

  number of seconds during which user connections and MDB data models
  are kept in memory (default: 3600)

- ...:

  additional parameters for
  [`chTKCat()`](https://patzaw.github.io/TKCat/reference/chTKCat.md)
  (only the default driver,
  [`ClickHouseHTTP::ClickHouseHTTP()`](https://patzaw.github.io/neo2R/reference/ClickHouseHTTP.html),
  is supported).

## Value

No return value, called for side effects: the function runs the server
until it is interrupted.

## Details

The server is meant to be run as a service behind a reverse proxy (e.g.,
nginx) providing HTTPS: by default it only listens on the local
interface (`mcpHost = "127.0.0.1"`). See
[`vignette("TKCat-MCP", package = "TKCat")`](https://patzaw.github.io/TKCat/articles/TKCat-MCP.md)
for deployment instructions.

**Authentication.** Clients authenticate with their chTKCat (ClickHouse)
credentials provided in an HTTP Basic `Authorization` header
(`Authorization: Basic <base64 of "user:password">`). Credentials are
checked by ClickHouse and each client gets access to the MDBs readable
by its own user. No credential is stored by the server, except in memory
to keep the connections open (during `cacheTime` seconds). If
`anonymousUser` is set (e.g., "default"), requests without
`Authorization` header are processed with this user and
`anonymousPassword`. **Credentials are sent in clear text if HTTPS is
not used.**

**Read-only access.** As for
[`chTKCat_mcp_server()`](https://patzaw.github.io/TKCat/reference/chTKCat_mcp_server.md),
ClickHouse sessions are opened with the `readonly = 2` setting and SQL
queries are checked before being sent.

**Protocol.** The server is stateless: it does not create MCP sessions
and answers each POST request with a single JSON response. It does not
provide server-sent event streams (GET requests are rejected with a 405
status). Several instances can thus be run behind a load balancer.

**Logging.** Tool calls are logged on the standard error output (time,
client address, user and tool name).

## See also

[`chTKCat_mcp_server()`](https://patzaw.github.io/TKCat/reference/chTKCat_mcp_server.md),
[`chTKCat()`](https://patzaw.github.io/TKCat/reference/chTKCat.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## From a terminal (normally run as a service)
# TKCAT_HOST=localhost TKCAT_PORT=9111 \
#   Rscript -e "TKCat::chTKCat_mcp_http_server(mcpPort = 8800)"
} # }
```
