library(devTKCat)

hpof <- read_fileMDB(
   path=system.file("examples/HPO-subset", package="ReDaMoR"),
   dataModel=system.file("examples/HPO-model.json", package="ReDaMoR"),
   dbInfo=list(
      "name"="HPO",
      "title"="Data extracted from the HPO database", 
      "description"=paste(
         "The Human Phenotype Ontology (HPO) aims to provide a standardized", 
         "vocabulary of phenotypic abnormalities encountered in human disease.",
         "Each term in the HPO describes a phenotypic abnormality,",
         "such as atrial septal defect. The HPO is currently being developed",
         "using the medical literature, Orphanet, DECIPHER, and OMIM.",
         "HPO currently contains approximately 11,000 terms (still growing)",
         "and over 115,000 annotations to hereditary diseases.",
         "The HPO also provides a large set of HPO annotations to approximately",
         "4000 common diseases."
      ),
      "url"="http://human-phenotype-ontology.github.io/",
      "version"="0.9",
      "maintainer"="Patrice Godard <patrice.godard@ucb.com>"
   )
)
message(
   "This is a very small subset of the HPO! Visit ",
   db_info(hpof)$url
)
count_records(hpof)

hpoSlice <- slice(hpof, HPO_diseases=1:10)
count_records(hpoSlice)

if(requireNamespace("stringr", quietly = TRUE)){
   epilHP <- filter(
      hpof,
      HPO_diseases=stringr::str_detect(
         label, stringr::regex("epilepsy", ignore_case=TRUE)
      )
   )
   count_records(epilHP)
}
