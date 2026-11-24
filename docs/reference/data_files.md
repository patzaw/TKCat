# Get the data files from a [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) or a [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md) object

Get the data files from a
[fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) or a
[parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md)
object

## Usage

``` r
data_files(x)
```

## Arguments

- x:

  a [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) or a
  [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md)
  object

## Value

a list with "dataFiles" and "readParameters" for reading the files.
There is no read parameter for
[parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md)
(empty list). For
[parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md) the
data file of a matrix stored in a long format is a directory.
