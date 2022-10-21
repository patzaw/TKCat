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
count_records(hpo)

## Too long on win-builder.r-project.org
\dontrun{
   
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

}
