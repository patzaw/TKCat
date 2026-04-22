# Explore available [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) in a shiny web interface

Explore available [MDB](https://patzaw.github.io/TKCat/reference/MDB.md)
in a shiny web interface

## Usage

``` r
# S3 method for class 'TKCat'
explore_MDBs(
  x,
  subSetSize = 100,
  download = FALSE,
  workers = 4,
  title = NULL,
  skinColors = "green",
  logoDiv = TKCAT_LOGO_DIV,
  rDirs = NULL,
  tabTitle = "TKCat",
  tabIcon = "www/TKCat-small.png",
  ...
)

# S3 method for class 'chTKCat'
explore_MDBs(
  x,
  subSetSize = 100,
  host = x$chcon@host,
  download = FALSE,
  workers = 4,
  userManager = NULL,
  title = NULL,
  skinColors = c("blue", "yellow"),
  logoDiv = TKCAT_LOGO_DIV,
  tabTitle = "chTKCat",
  tabIcon = "www/TKCat-small.png",
  rDirs = NULL,
  ...
)

explore_MDBs(x, ...)
```

## Arguments

- x:

  a [TKCat](https://patzaw.github.io/TKCat/reference/TKCat.md) related
  object (e.g.
  [chTKCat](https://patzaw.github.io/TKCat/reference/chTKCat.md))

- subSetSize:

  the maximum number of records to show

- download:

  a logical indicating if data can be downloaded (default: FALSE). If
  TRUE a temporary directory is created and made available for shiny.

- workers:

  number of available workers when download is available (default: 4)

- title:

  A title for the application. If NULL (default): the chTKCat instance
  name

- skinColors:

  two colors for the application skin: one for default connection
  ("blue" by default) and one for user connection ("yellow" by default).
  Working values: "blue", "black", "purple", "green", "red", "yellow".

- logoDiv:

  a [shiny::div](https://rdrr.io/pkg/shiny/man/reexports.html) object
  with a logo to display in side bar. The default is the TKCat hex
  sticker with a link to TKCat github repository.

- rDirs:

  a named character vector with resource path for
  [shiny::addResourcePath](https://rdrr.io/pkg/shiny/man/resourcePaths.html)

- tabTitle:

  a title to display in tab (default: "chTKCat")

- tabIcon:

  a path to an image (in available resource paths: "www", "doc" or in
  rDirs) to use as a tab icon.

- ...:

  method specific parameters

- host:

  the name of the host to show in the application

- userManager:

  URL for user management interface (see
  [`manage_chTKCat_users()`](https://patzaw.github.io/TKCat/reference/manage_chTKCat_users.md)).
  If NULL (default), the functionality is not added.

## Value

No return value, called for side effects
