# Get the size of data files from an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) stored in files

Get the size of data files from an
[MDB](https://patzaw.github.io/TKCat/reference/MDB.md) stored in files

## Usage

``` r
# S3 method for class 'fileMDB'
data_file_size(x, hr = FALSE, ...)

data_file_size(x, hr = FALSE, ...)

# S3 method for class 'parquetMDB'
data_file_size(x, hr = FALSE, ...)
```

## Arguments

- x:

  an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object
  stored in files: a
  [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) or a
  [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md)

- hr:

  a logical indicating if the values should be "human readable".
  (default: FALSE)

- ...:

  method specific parameters

## Value

a tibble with the name of the table, the size of the files in bytes
(hr=FALSE) or as a character vector with units (hr=TRUE) and a logical
indicating if the file is compressed. For
[parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md) the
compression is internal to the parquet files and not reported (NA).
