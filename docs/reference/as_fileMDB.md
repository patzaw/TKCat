# Write an MDB object

Write an MDB object

## Usage

``` r
# S3 method for class 'chMDB'
as_fileMDB(
  x,
  path,
  readParameters = list(delim = "\t", na = "<NA>"),
  htmlModel = TRUE,
  compress = TRUE,
  by = 10^5,
  ...
)

# S3 method for class 'fileMDB'
as_fileMDB(
  x,
  path,
  readParameters = list(delim = "\t", na = "<NA>"),
  htmlModel = TRUE,
  compress = TRUE,
  by = 10^5,
  ...
)

as_fileMDB(
  x,
  path,
  readParameters = list(delim = "\t", na = "<NA>"),
  htmlModel = TRUE,
  compress = TRUE,
  by = 10^5,
  ...
)

# S3 method for class 'memoMDB'
as_fileMDB(
  x,
  path,
  readParameters = list(delim = "\t", na = "<NA>"),
  htmlModel = TRUE,
  compress = TRUE,
  by = 10^5,
  ...
)

# S3 method for class 'metaMDB'
as_fileMDB(
  x,
  path,
  readParameters = list(delim = "\t", na = "<NA>"),
  htmlModel = TRUE,
  compress = TRUE,
  by = 10^5,
  ...
)
```

## Arguments

- x:

  an MDB object

- path:

  the path where the MDB should be written

- readParameters:

  The following parameters are currently supported:

  - **delim**: a single character used to separate fields within a
    record (default: '\t')

  - **quoted_na**: a single logical indicating if missing values inside
    quotes should be treated as missing values or strings. WARNING: THIS
    PARAMETER IS NOT TAKEN INTO ACCOUNT WITH readr\>=2.0.0.

  - **na**: String used for missing values. The default value for
    reading a fileMDB is "NA". But the default value for writing a
    fileMDB is ""\<NA\>"". This value is written in the DESCRIPTION.json
    file to avoid ambiguity when reading the fileMDB.

- htmlModel:

  a logical. If TRUE (default) the model is also plotted in an html
  file.

- compress:

  a logical specifying whether saving data is to use "gzip" compression
  (default: TRUE)

- by:

  the size of the batch: number of records to write together (default:
  10^5)

- ...:

  method specific parameters

## Value

A [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) object.
