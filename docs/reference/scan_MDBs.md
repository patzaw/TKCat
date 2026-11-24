# Scan a catalog of [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) and [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md)

Each sub directory is read as a
[parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md) if
it contains parquet data files and as a
[fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md)
otherwise.

## Usage

``` r
scan_MDBs(path, subdirs = NULL, check = TRUE, n_max = 10)
```

## Arguments

- path:

  directory from which all the
  [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) and
  [parquetMDB](https://patzaw.github.io/TKCat/reference/parquetMDB.md)
  should be read

- subdirs:

  the sub directories (relative to path) to take into account. If NULL
  (default) all the sub directories are considered.

- check:

  logical: if TRUE (default) the data are confronted to the data model

- n_max:

  maximum number of records to read for checks purpose (default: 10).
  See also
  [`ReDaMoR::confront_data()`](https://patzaw.github.io/ReDaMoR/reference/confront_data.html).

## Value

a TKCat object

## See also

[read_fileMDB](https://patzaw.github.io/TKCat/reference/read_fileMDB.md),
[read_parquetMDB](https://patzaw.github.io/TKCat/reference/read_parquetMDB.md)
