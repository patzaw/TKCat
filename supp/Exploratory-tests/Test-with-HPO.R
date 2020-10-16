library(devTKCat)

## Different type of MDBs ----
k <- chTKCat(port=9201, user="pgodard", password="1234")
hpof <- read_fileMDB("~/Tmp/HPO")
if(!"HPO" %in% list_MDBs(k, withInfo=FALSE)){
   create_chMDB(k, "HPO")
}
if(!"HPO" %in% list_MDBs(k, withInfo=TRUE)$name){
   hpoch <- as_chMDB(hpof, k)
}else{
   hpoch <- get_MDB(k, "HPO")
}
hpo <- as_memoMDB(hpoch)

## Filter test ----
fhpo <- filter(
   hpo,
   HPO_diseases=stringr::str_detect(
      label, stringr::regex("epilepsy", ignore_case=TRUE)
   )
)
fhpof <- filter(
   hpof,
   HPO_diseases=stringr::str_detect(
      label, stringr::regex("epilepsy", ignore_case=TRUE)
   )
)
fhpoch <- filter(
   hpoch,
   HPO_diseases=stringr::str_detect(
      label, stringr::regex("epilepsy", ignore_case=TRUE)
   )
)

## Slice test ----
toTake <- sample(1:count_records(hpoch, "HPO_hp"), 8, replace=FALSE)
shpo <- slice(
   hpo,
   HPO_hp=toTake
)
shpof <- slice(
   hpof,
   HPO_hp=toTake
)
shpoch <- slice(
   hpof,
   HPO_hp=toTake
)
