# An [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) (Modeled DataBase) based on files: fileMDB

An [MDB](https://patzaw.github.io/TKCat/reference/MDB.md) (Modeled
DataBase) based on files: fileMDB

Rename tables of a fileMDB object

## Usage

``` r
# S3 method for class 'chMDB'
x$i

fileMDB(
  dataFiles,
  dbInfo,
  dataModel,
  readParameters = DEFAULT_READ_PARAMS,
  collectionMembers = NULL,
  check = TRUE,
  n_max = 10,
  verbose = FALSE
)

# S3 method for class 'fileMDB'
names(x) <- value

# S3 method for class 'fileMDB'
rename(.data, ...)

# S3 method for class 'fileMDB'
x[i]

# S3 method for class 'fileMDB'
x[[i]]

# S3 method for class 'fileMDB'
x$i

# S3 method for class 'fileMDB'
as.list(x, ...)
```

## Arguments

- x:

  a fileMDB object

- i:

  the index or the name of the tables to take

- dataFiles:

  a named vector of path to data files with
  `all(names(dataFiles) %in% names(dataModel))`

- dbInfo:

  a list with DB information: **"name"** (only mandatory field),
  "title", "description", "url", "version", "maintainer".

- dataModel:

  a
  [ReDaMoR::RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.html)
  object

- readParameters:

  a list of parameters for reading the data file. (e.g.
  `list(delim='\t', quoted_na=FALSE,)`)

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

- value:

  new table names

- .data:

  a fileMDB object

- ...:

  additional parameters

## Value

A fileMDB object

`as.list.fileMDB()` returns a simple list of tibbles with all the data
from the tables in x.

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

- [filter.fileMDB](https://patzaw.github.io/TKCat/reference/filter.fileMDB.md),
  [slice.fileMDB](https://patzaw.github.io/TKCat/reference/slice.fileMDB.md)

## Examples

``` r
hpof <- read_fileMDB(
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
)
#> HPO
#> SUCCESS
#> 
#> Check configuration
#>    - Optional checks: 
#>    - Maximum number of records: 10
count_records(hpof)
#>              HPO_hp           HPO_altId     HPO_sourceFiles        HPO_diseases 
#>                 500                  89                   2                1903 
#>       HPO_diseaseHP HPO_diseaseSynonyms         HPO_parents     HPO_descendants 
#>                2594                2337                  95                 972 
#>        HPO_synonyms 
#>                 730 

## The following commands take time on fileMDB object
if (FALSE) { # \dontrun{

select(hpof, HPO_hp:HPO_diseases)
toTake <- "HPO_altId"
select(hpof, all_of(toTake))

hpoSlice <- slice(hpof, HPO_diseases=1:10)
count_records(hpoSlice)

if("stringr" %in% installed.packages()[,"Package"]){
   epilHP <- filter(
      hpof,
      HPO_diseases=stringr::str_detect(
         label, stringr::regex("epilepsy", ignore_case=TRUE)
      )
   )
   count_records(epilHP)
   label <- "Rolandic epilepsy"
   cn <- sym("label")
   reHP <- filter(
      hpof,
      HPO_diseases=!!cn==!!label
   )
}

} # }
```
