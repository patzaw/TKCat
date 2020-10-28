library(TKCat)

## Different type of MDBs ----
k <- chTKCat(port=9101, user="pgodard")
contf <- read_fileMDB("~/Shared/Data-Science/Data-Source-Model-Repository/CortellisONT/")
if(!"CortellisONT" %in% list_MDBs(k, withInfo=FALSE)){
   create_chMDB(k, "CortellisONT")
}
if(!"CortellisONT" %in% list_MDBs(k, withInfo=TRUE)$name){
   contch <- as_chMDB(contf, k)
}else{
   contch <- get_MDB(k, "CortellisONT")
}
