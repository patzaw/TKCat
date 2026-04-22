# Add a set of helper functions to a compatible object

Add a set of helper functions to a compatible object

## Usage

``` r
# S3 method for class 'MDB'
add_helpers(x, code, name, language, kmr, ...)

# S3 method for class 'KMR'
add_helpers(x, code, name, language, ...)

add_helpers(x, code, name, language, ...)
```

## Arguments

- x:

  an object accepting helpers

- code:

  file path to the source code of helper functions

- name:

  the name of the helper set

- language:

  the programming language of the code (default: "R")

- kmr:

  an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object with
  KM requirements

- ...:

  method specific parameters

## Value

Return x with additional helpers
