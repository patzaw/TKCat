# Write an MDB object in parquet files

Write an MDB object in parquet files

## Usage

``` r
as_parquetMDB(
  x,
  path,
  htmlModel = TRUE,
  compression = "zstd",
  maxCol = PQ_MAX_COL,
  by = 10^7,
  ...
)

# S3 method for class 'memoMDB'
as_parquetMDB(
  x,
  path,
  htmlModel = TRUE,
  compression = "zstd",
  maxCol = PQ_MAX_COL,
  by = 10^7,
  ...
)

# S3 method for class 'fileMDB'
as_parquetMDB(
  x,
  path,
  htmlModel = TRUE,
  compression = "zstd",
  maxCol = PQ_MAX_COL,
  by = 10^7,
  ...
)

# S3 method for class 'chMDB'
as_parquetMDB(
  x,
  path,
  htmlModel = TRUE,
  compression = "zstd",
  maxCol = PQ_MAX_COL,
  by = 10^7,
  ...
)

# S3 method for class 'metaMDB'
as_parquetMDB(
  x,
  path,
  htmlModel = TRUE,
  compression = "zstd",
  maxCol = PQ_MAX_COL,
  by = 10^7,
  ...
)

# S3 method for class 'parquetMDB'
as_parquetMDB(
  x,
  path,
  htmlModel = TRUE,
  compression = "zstd",
  maxCol = PQ_MAX_COL,
  by = 10^7,
  rewrite = FALSE,
  ...
)
```

## Arguments

- x:

  an MDB object

- path:

  the path where the MDB should be written

- htmlModel:

  a logical. If TRUE (default) the model is also plotted in an html
  file.

- compression:

  the compression algorithm used in parquet files (default: "zstd"). See
  [`arrow::write_parquet()`](https://arrow.apache.org/docs/r/reference/write_parquet.html)
  for available values.

- maxCol:

  the maximum number of columns for writing a dense matrix in a wide
  format: one column in the parquet file per column of the matrix
  (default: 1000). Dense matrices with more columns are written in a
  long format (see
  [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md)).
  Sparse matrices are always written in a long format.

- by:

  the size of the batch: number of records to write together (default:
  10^7)

- ...:

  method specific parameters

- rewrite:

  a single logical only used when x is a
  [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md).
  If FALSE (default) the parquet files are copied as they are:
  `compression` and `maxCol` are not taken into account. If TRUE, the
  files are written again according to `compression` and `maxCol`.

## Value

A [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md)
object.

## Details

For [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md), the
text files are converted in parquet files by the arrow package without
loading the data in R. When a file cannot be converted this way (e.g.
values not supported by the arrow reader), it is read by batches in R as
a fallback and a message is displayed. Dense matrices with more than
`maxCol` columns are also converted by batches in R.

## See also

[`read_parquetMDB()`](https://patzaw.github.io/TKCat/reference/read_parquetMDB.md),
[`as_fileMDB()`](https://patzaw.github.io/TKCat/reference/as_fileMDB.md)
