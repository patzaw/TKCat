# Get the data files from an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) stored in files

Get the data files from an
[MDB](https://patzaw.github.io/TKCat/reference/MDB.md) stored in files

## Usage

``` r
# S3 method for class 'fileMDB'
data_files(x, ...)

data_files(x, ...)

# S3 method for class 'parquetMDB'
data_files(x, ...)
```

## Arguments

- x:

  an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object
  stored in files: a
  [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) or a
  [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md)

- ...:

  method specific parameters

## Value

a list with "dataFiles" and "readParameters" for reading the files.
There is no read parameter for
[parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md)
(empty list). For
[parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md) the
data file of a matrix stored in a long format is a directory.
