# Archive a chMDB in a [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)

Archive a chMDB in a
[chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)

## Usage

``` r
archive_chMDB(
  x,
  name,
  defaultTS = as.POSIXct("1970-01-01 00:00.0", tz = "UTC")
)
```

## Arguments

- x:

  a [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md)
  object

- name:

  the name of the database to archive

- defaultTS:

  a default timestamp value to use when not existing in the DB (default:
  `as.POSIXct("1970-01-01 00:00.0", tz="UTC")`)

## Value

No return value, called for side effects
