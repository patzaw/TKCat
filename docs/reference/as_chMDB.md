# Push an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object in a ClickHouse database

Push an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object in
a ClickHouse database

## Usage

``` r
as_chMDB(
  x,
  tkcon,
  timestamp = Sys.time(),
  overwrite = FALSE,
  by = 10^5,
  materializeProjections = TRUE
)
```

## Arguments

- x:

  an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object

- tkcon:

  a [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)
  object

- timestamp:

  a single POSIXct value as a timestamp for the chMDB instance. The
  default value is the current system time. If this value is smaller or
  equal to the chMDB current value, an error is thrown. If NA, the
  current instance is overwritten (if the overwrite parameter is set to
  TRUE) without changing the existing timestamp.

- overwrite:

  a logical indicating if existing data should be overwritten (default:
  FALSE)

- by:

  the size of the batch: number of records to write together (default:
  10^5)

- materializeProjections:

  a logical indicating if projections should be materialize (default:
  TRUE)

## Value

A [chMDB](https://patzaw.github.io/TKCat/reference/chMDB.md) object.
