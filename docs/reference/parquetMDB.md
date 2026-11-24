# An [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) (Modeled DataBase) based on parquet files: parquetMDB

A parquetMDB is similar to a
[fileMDB](https://patzaw.github.io/TKCat/reference/fileMDB.md) but data
are stored in [parquet](https://parquet.apache.org/) files instead of
delimited text files. Columns are typed according to the data model, the
exact dimensions of the tables are available without reading the data
(see [`dims()`](https://patzaw.github.io/TKCat/reference/dims.md)) and
filters are applied when reading the files.

## Usage

``` r
parquetMDB(
  dataFiles,
  dbInfo,
  dataModel,
  collectionMembers = NULL,
  check = TRUE,
  n_max = 10,
  verbose = FALSE
)

# S3 method for class 'parquetMDB'
names(x) <- value

# S3 method for class 'parquetMDB'
rename(.data, ...)

# S3 method for class 'parquetMDB'
x[i]

# S3 method for class 'parquetMDB'
x[[i]]

# S3 method for class 'parquetMDB'
x$i

# S3 method for class 'parquetMDB'
as.list(x, ...)
```

## Arguments

- dataFiles:

  a named vector of path to data files (or directories for matrices
  stored in a long format) with
  `all(names(dataFiles) %in% names(dataModel))`

- dbInfo:

  a list with DB information: **"name"** (only mandatory field),
  "title", "description", "url", "version", "maintainer".

- dataModel:

  a
  [ReDaMoR::RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.html)
  object

- collectionMembers:

  the members of collections as provided to the collection_members\<-
  function (default: NULL ==\> no member).

- check:

  logical: if TRUE (default) the data are confronted to the data model

- n_max:

  maximum number of records to read for checks purpose (default: 10).
  See also
  [`ReDaMoR::confront_data()`](https://patzaw.github.io/ReDaMoR/reference/confront_data.html).

- verbose:

  if TRUE display the data confrontation report (default: FALSE)

- x:

  a parquetMDB object

- value:

  new table names

- .data:

  a parquetMDB object

- ...:

  additional parameters

- i:

  the index or the name of the tables to take

## Value

A parquetMDB object

`as.list.parquetMDB()` returns a simple list of tibbles with all the
data from the tables in x.

## Details

Tables are stored in one parquet file each. Matrices are stored either:

- in one "wide" parquet file with a `___ROWNAMES___` column followed by
  one column per matrix column;

- or in a directory with a "long" format made of 3 parquet files:
  *rows.parquet* (i, name), *columns.parquet* (j, name) and
  *values.parquet* (i, j, x) for sparse matrices or *all_values.parquet*
  (i, j, x) for dense matrices with many columns.

## See also

- MDB methods:
  [db_info](https://patzaw.github.io/TKCat/reference/db_info.md),
  [data_model](https://patzaw.github.io/TKCat/reference/data_model.md),
  [data_tables](https://patzaw.github.io/TKCat/reference/data_tables.md),
  [collection_members](https://patzaw.github.io/TKCat/reference/collection_members.md),
  [count_records](https://patzaw.github.io/TKCat/reference/count_records.md),
  [dims](https://patzaw.github.io/TKCat/reference/dims.md),
  [filter_with_tables](https://patzaw.github.io/TKCat/reference/filter_with_tables.md),
  [as_fileMDB](https://patzaw.github.io/TKCat/reference/as_fileMDB.md),
  [as_parquetMDB](https://patzaw.github.io/TKCat/reference/as_parquetMDB.md)

- Additional general documentation is related to
  [MDB](https://patzaw.github.io/TKCat/reference/MDB.md).

- [filter.parquetMDB](https://patzaw.github.io/TKCat/reference/filter.parquetMDB.md),
  [slice.parquetMDB](https://patzaw.github.io/TKCat/reference/slice.parquetMDB.md)

## Examples

``` r
## Read a fileMDB (text files)
hpof <- read_fileMDB(
  path = system.file("examples/HPO-subset", package = "ReDaMoR"),
  dataModel = system.file("examples/HPO-model.json", package = "ReDaMoR"),
  dbInfo = list(
    "name" = "HPO",
    "title" = "Data extracted from the HPO database",
    "description" = paste(
      "This is a very small subset of the HPO!",
      "Visit the reference URL for more information"
    ),
    "url" = "http://human-phenotype-ontology.github.io/"
  )
)
#> HPO
#> SUCCESS
#> 
#> Check configuration
#>    - Optional checks: 
#>    - Maximum number of records: 10

## Convert the text files in parquet files: data are not loaded in R
path <- tempfile()
hpop <- as_parquetMDB(hpof, path = path, htmlModel = FALSE)
hpop
#> parquetMDB HPO: Data extracted from the HPO database
#>    - 9 tables with 25 fields
#> 
#> No collection member
#> 
#> This is a very small subset of the HPO! Visit the reference URL for more information
#> (http://human-phenotype-ontology.github.io/)
#> 
#>  

## The same parquetMDB can be read from the path
hpop <- read_parquetMDB(file.path(path, "HPO"))
#> HPO
#> SUCCESS
#> 
#> Check configuration
#>    - Optional checks: 
#>    - Maximum number of records: 10

## Exact dimensions are read from the metadata of the parquet files
dims(hpop)
#> # A tibble: 9 × 7
#>   name                format  ncol  nrow records bytes transposed
#>   <chr>               <chr>  <dbl> <dbl>   <dbl> <dbl> <lgl>     
#> 1 HPO_hp              table      4   500     500 26266 FALSE     
#> 2 HPO_altId           table      2    89      89  1378 FALSE     
#> 3 HPO_sourceFiles     table      2     2       2  1163 FALSE     
#> 4 HPO_diseases        table      3  1903    1903 44343 FALSE     
#> 5 HPO_diseaseHP       table      3  2594    2594 14367 FALSE     
#> 6 HPO_diseaseSynonyms table      4  2337    2337 52029 FALSE     
#> 7 HPO_parents         table      2    95      95  1346 FALSE     
#> 8 HPO_descendants     table      2   972     972  4715 FALSE     
#> 9 HPO_synonyms        table      3   730     730 11327 FALSE     
count_records(hpop)
#>              HPO_hp           HPO_altId     HPO_sourceFiles        HPO_diseases 
#>                 500                  89                   2                1903 
#>       HPO_diseaseHP HPO_diseaseSynonyms         HPO_parents     HPO_descendants 
#>                2594                2337                  95                 972 
#>        HPO_synonyms 
#>                 730 

## Tables
select(hpop, HPO_hp:HPO_diseases)
#> parquetMDB HPO: Data extracted from the HPO database
#>    - 4 tables with 11 fields
#> 
#> No collection member
#> 
#> This is a very small subset of the HPO! Visit the reference URL for more information
#> (http://human-phenotype-ontology.github.io/)
#> 
#>  
hpop$HPO_diseases
#> # A tibble: 1,903 × 3
#>    db       id     label                                                        
#>    <chr>    <chr>  <chr>                                                        
#>  1 DECIPHER 15     NF1-microdeletion syndrome                                   
#>  2 DECIPHER 45     Xq28 (MECP2) duplication                                     
#>  3 DECIPHER 65     ATR-16 syndrome                                              
#>  4 OMIM     100050 AARSKOG SYNDROME, AUTOSOMAL DOMINANT                         
#>  5 OMIM     100650 ALDEHYDE DEHYDROGENASE 2 FAMILY                              
#>  6 OMIM     101800 ACRODYSOSTOSIS 1, WITH OR WITHOUT HORMONE RESISTANCE; ACRDYS1
#>  7 OMIM     102500 HAJDU-CHENEY SYNDROME; HJCYS                                 
#>  8 OMIM     102510 ACROPECTOROVERTEBRAL DYSPLASIA, F-FORM OF                    
#>  9 OMIM     102700 SEVERE COMBINED IMMUNODEFICIENCY, AUTOSOMAL RECESSIVE, T CEL…
#> 10 OMIM     102800 ADENOSINE TRIPHOSPHATASE DEFICIENCY, ANEMIA DUE TO           
#> # ℹ 1,893 more rows

## Filters are applied when reading the files and return a memoMDB
hpoSlice <- slice(hpop, HPO_diseases = 1:10)
count_records(hpoSlice)
#>        HPO_diseases       HPO_diseaseHP HPO_diseaseSynonyms              HPO_hp 
#>                  10                  13                  15                  13 
#>           HPO_altId         HPO_parents     HPO_descendants        HPO_synonyms 
#>                   8                   0                  14                  15 
label <- "Rolandic epilepsy"
reHP <- filter(hpop, HPO_diseases = label == !!label)
count_records(reHP)
#>        HPO_diseases       HPO_diseaseHP HPO_diseaseSynonyms              HPO_hp 
#>                   1                   1                   1                   1 
#>           HPO_altId         HPO_parents     HPO_descendants        HPO_synonyms 
#>                   0                   0                   1                   0 

## Conversion to other MDB implementations
hpom <- as_memoMDB(hpop)
hpof2 <- as_fileMDB(hpop, path = tempfile(), htmlModel = FALSE)

unlink(path, recursive = TRUE)
```
