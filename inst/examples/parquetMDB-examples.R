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

## Convert the text files in parquet files: data are not loaded in R
path <- tempfile()
hpop <- as_parquetMDB(hpof, path = path, htmlModel = FALSE)
hpop

## The same parquetMDB can be read from the path
hpop <- read_parquetMDB(file.path(path, "HPO"))

## Exact dimensions are read from the metadata of the parquet files
dims(hpop)
count_records(hpop)

## Tables
select(hpop, HPO_hp:HPO_diseases)
hpop$HPO_diseases

## Filters are applied when reading the files and return a memoMDB
hpoSlice <- slice(hpop, HPO_diseases = 1:10)
count_records(hpoSlice)
label <- "Rolandic epilepsy"
reHP <- filter(hpop, HPO_diseases = label == !!label)
count_records(reHP)

## Conversion to other MDB implementations
hpom <- as_memoMDB(hpop)
hpof2 <- as_fileMDB(hpop, path = tempfile(), htmlModel = FALSE)

unlink(path, recursive = TRUE)
