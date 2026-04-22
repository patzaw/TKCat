# Get the size of data files from a [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) object

Get the size of data files from a
[fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) object

## Usage

``` r
data_file_size(x, hr = FALSE)
```

## Arguments

- x:

  a [fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md)
  object

- hr:

  a logical indicating if the values should be "human readable".
  (default: FALSE)

## Value

a numeric vector with size in bytes (hr=FALSE) or a character vector
with size and units (hr=TRUE)
