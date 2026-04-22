# An [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) (Modeled DataBase) in memory: memoMDB

An [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) (Modeled
DataBase) in memory: memoMDB

Rename tables of a memoMDB object

## Usage

``` r
memoMDB(
  dataTables,
  dataModel,
  dbInfo,
  collectionMembers = NULL,
  check = TRUE,
  checks = c("unique", "not nullable", "foreign keys"),
  verbose = FALSE
)

# S3 method for class 'memoMDB'
names(x) <- value

# S3 method for class 'memoMDB'
rename(.data, ...)

# S3 method for class 'memoMDB'
x[i]

# S3 method for class 'memoMDB'
x[[i]]

# S3 method for class 'memoMDB'
x$i
```

## Arguments

- dataTables:

  a list of tibbles

- dataModel:

  a
  [ReDaMoR::RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.html)
  object

- dbInfo:

  a list with DB information: **"name"** (only mandatory field),
  "title", "description", "url", "version", "maintainer".

- collectionMembers:

  the members of collections as provided to the collection_members\<-
  function (default: NULL ==\> no member).

- check:

  logical: if TRUE (default) the data are confronted to the data model

- checks:

  a character vector with the name of optional checks to be done (all of
  them c("unique", "not nullable", "foreign keys"))

- verbose:

  if TRUE display the data confrontation report (default: FALSE)

- x:

  a memoMDB object

- value:

  new table names

- .data:

  a memoMDB object

- ...:

  Use new_name = old_name to rename selected tables

- i:

  the index or the name of the tables to take

## Value

A memoMDB object

## See also

- MDB methods:
  [db_info](https://patzaw.github.io/TKCat/reference/db_info.md),
  [data_model](https://patzaw.github.io/TKCat/reference/data_model.md),
  [data_tables](https://patzaw.github.io/TKCat/reference/data_tables.md),
  [collection_members](https://patzaw.github.io/TKCat/reference/collection_members.md),
  [count_records](https://patzaw.github.io/TKCat/reference/count_records.md),
  [dims](https://patzaw.github.io/TKCat/reference/dims.md),
  [filter_with_tables](https://patzaw.github.io/TKCat/reference/filter_with_tables.md),
  [as_fileMDB](https://patzaw.github.io/TKCat/reference/as_fileMDB.md)

- Additional general documentation is related to
  [MDB](https://patzaw.github.io/TKCat/reference/MDB.md).

- [filter.memoMDB](https://patzaw.github.io/TKCat/reference/filter.memoMDB.md),
  [slice.memoMDB](https://patzaw.github.io/TKCat/reference/slice.memoMDB.md)

## Examples

``` r
hpo <- read_fileMDB(
   path=system.file("examples/HPO-subset", package="ReDaMoR"),
   dataModel=system.file("examples/HPO-model.json", package="ReDaMoR"),
   dbInfo=list(
      "name"="HPO",
      "title"="Data extracted from the HPO database",
      "description"=paste(
         "This is a very small subset of the HPO!",
         "Visit the reference URL for more information"
      ),
      "url"="http://human-phenotype-ontology.github.io/"
   )
) %>% 
   as_memoMDB()
#> HPO
#> SUCCESS
#> 
#> Check configuration
#>    - Optional checks: 
#>    - Maximum number of records: 10
count_records(hpo)
#>              HPO_hp           HPO_altId     HPO_sourceFiles        HPO_diseases 
#>                 500                  89                   2                1903 
#>       HPO_diseaseHP HPO_diseaseSynonyms         HPO_parents     HPO_descendants 
#>                2594                2337                  95                 972 
#>        HPO_synonyms 
#>                 730 

## Too long on win-builder.r-project.org
if (FALSE) { # \dontrun{
   
hpoSlice <- slice(hpo, HPO_diseases=1:10)
count_records(hpoSlice)

if("stringr" %in% installed.packages()[,"Package"]){
   epilHP <- filter(
      hpo,
      HPO_diseases=stringr::str_detect(
         label, stringr::regex("epilepsy", ignore_case=TRUE)
      )
   )
   count_records(epilHP)
}

} # }
```
