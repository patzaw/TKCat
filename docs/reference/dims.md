# Detailed information about the format of the tables

Detailed information about the format of the tables

## Usage

``` r
# S3 method for class 'chMDB'
dims(x, ...)

# S3 method for class 'fileMDB'
dims(
  x,
  ...,
  by = 1000,
  estimateThr = 5e+07,
  estimateSample = 10^6,
  showWarnings = TRUE
)

dims(x, ...)

# S3 method for class 'memoMDB'
dims(x, ...)

# S3 method for class 'metaMDB'
dims(x, ...)
```

## Arguments

- x:

  an object with embedded data tables

- ...:

  the name of the tables to consider (default: all of them)

- by:

  the size of the batch: number of lines to count together (default:
  1000)

- estimateThr:

  file size threshold in bytes from which an estimation of row number
  should be computed instead of a precise count (default: 50000000 =
  50MB)

- estimateSample:

  number of values on which the estimation is based (default: 10^6)

- showWarnings:

  a warning is raised by default if estimation is done.

## Value

A tibble with one row for each considered table and the following
columns:

- name: the name of the table

- format: "table", "matrix" or "MatrixMarket"

- ncol: number of columns

- nrow: number of rows

- records: number of records (`nrow` for tables and `ncol*nrow` for
  matrices)

- bytes: size in bytes

- transposed: FALSE by default. TRUE only for matrices stored in a
  transposed format.
