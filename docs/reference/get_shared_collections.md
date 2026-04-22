# Get collections shared by 2 objects and return member combinations

Get collections shared by 2 objects and return member combinations

## Usage

``` r
get_shared_collections(x, y)
```

## Arguments

- x:

  an MDB object

- y:

  an MDB object

## Value

A tibble with the following fields:

- **collection** the name of the collection

- **mid.x** the collection member identifier in x

- **table.x** the table of the collection member in x

- **mid.y** the collection member identifier in y

- **table.y** the table of the collection member in y
