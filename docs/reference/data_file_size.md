# Get the size of data files from a [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) or a [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md) object

Get the size of data files from a
[fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) or a
[parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md)
object

## Usage

``` r
data_file_size(x, hr = FALSE)
```

## Arguments

- x:

  a [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) or a
  [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md)
  object

- hr:

  a logical indicating if the values should be "human readable".
  (default: FALSE)

## Value

a tibble with the name of the table, the size of the files in bytes
(hr=FALSE) or as a character vector with units (hr=TRUE) and a logical
indicating if the file is compressed. For
[parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md) the
compression is internal to the parquet files and not reported (NA).
