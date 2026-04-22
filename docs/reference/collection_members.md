# Collection members

Collection members

Collection members

## Usage

``` r
# S3 method for class 'TKCat'
collection_members(x, ...)

# S3 method for class 'chMDB'
collection_members(x, ...)

# S3 method for class 'chMDB'
collection_members(x) <- value

# S3 method for class 'chTKCat'
collection_members(x, ...)

# S3 method for class 'fileMDB'
collection_members(x, ...)

# S3 method for class 'fileMDB'
collection_members(x) <- value

collection_members(x, ...)

collection_members(x) <- value

# S3 method for class 'memoMDB'
collection_members(x, ...)

# S3 method for class 'memoMDB'
collection_members(x) <- value

# S3 method for class 'metaMDB'
collection_members(x, ...)
```

## Arguments

- x:

  an object with embedded collection members

- ...:

  names of the collections to focus on. By default, all of them are
  taken.

- value:

  the new collection members. A data.frame with the following columns:

  - **collection** (character): The name of the collection

  - **cid** (character): Collection identifier

  - **resource** (character): The name of the resource

  - **mid** (integer): The identifier of the member

  - **table** (character): The table recording collection information

  - **field** (character): The collection field.

  - **static** (logical): TRUE if the field value is common to all
    elements.

  - **value** (character): The name of the table column if static is
    FALSE or the field value if static is TRUE.

  - **type** (character): the type of the field. (not necessarily used
    ==\> NA if not)

## Value

A [dplyr::tibble](https://dplyr.tidyverse.org/reference/reexports.html)
with the following columns:

- **collection** (character): The name of the collection

- **cid** (character): Collection identifier

- **resource** (character): The name of the resource

- **mid** (integer): The identifier of the member

- **table** (character): The table recording collection information

- **field** (character): The collection field.

- **static** (logical): TRUE if the field value is common to all
  elements.

- **value** (character): The name of the table column if static is FALSE
  or the field value if static is TRUE.

- **type** (character): the type of the field. (not necessarily used
  ==\> NA if not)
