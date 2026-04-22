# DB information

DB information

DB information

## Usage

``` r
# S3 method for class 'chMDB'
db_info(x, ...)

# S3 method for class 'chMDB'
db_info(x) <- value

# S3 method for class 'fileMDB'
db_info(x, ...)

# S3 method for class 'fileMDB'
db_info(x) <- value

db_info(x, ...)

db_info(x) <- value

# S3 method for class 'memoMDB'
db_info(x, ...)

# S3 method for class 'memoMDB'
db_info(x) <- value

# S3 method for class 'metaMDB'
db_info(x, ...)

# S3 method for class 'metaMDB'
db_info(x) <- value
```

## Arguments

- x:

  an object with embedded database information

- ...:

  method specific parameters

- value:

  list with the following elements:

  - **name**: a single character

  - **title**: a single character

  - **description**: a single character

  - **url**: a single character

  - **version**: a single character

  - **maintainer**: a single character vector

  - **size**: a numeric vector providing the size of the DB in bytes

## Value

A list with the following elements:

- **name**: a single character

- **title**: a single character

- **description**: a single character

- **url**: a single character

- **version**: a single character

- **maintainer**: a single character vector

- **size**: a numeric vector providing the size of the DB in bytes
