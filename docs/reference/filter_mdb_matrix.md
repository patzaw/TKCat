# Filter a matrix stored in an MDB

Filter a matrix stored in an MDB

## Usage

``` r
# S3 method for class 'chMDB'
filter_mdb_matrix(x, tableName, ...)

# S3 method for class 'fileMDB'
filter_mdb_matrix(x, tableName, .by = 10^5, ...)

filter_mdb_matrix(x, tableName, ...)

# S3 method for class 'memoMDB'
filter_mdb_matrix(x, tableName, ...)

# S3 method for class 'metaMDB'
filter_mdb_matrix(x, tableName, ...)
```

## Arguments

- x:

  an [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) object

- tableName:

  a character vector of length 1 corresponding to the name of the table
  to filter (must be a matrix)

- ...:

  character vectors with the row names and/or columns names to select.
  The names of the parameters must correspond to the name of the column
  and of the row fields (the matrix cannot be filtered from values).

- .by:

  the size of the batch: number of lines to process together (default:
  10000)

## Value

A sub-matrix of tableName in x. Only existing elements are returned. No
error is raised if any element is missing. The result must be checked
and adapted to user needs.

## Examples

``` r
if (FALSE) { # \dontrun{
## Return the matrix of expression values focused on the selected genes
filter_mdb_matrix(x=db, "Expression_value", gene=c("SNCA", "MAPT"))
} # }
```
