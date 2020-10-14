library(devTKCat)

## Different type of MDBs ----
k <- chTKCat(port=9201, user="pgodard", password="1234")
contf <- read_fileMDB("~/Shared/Data-Science/Data-Source-Model-Repository/CortellisONT/")
if(!"CortellisONT" %in% list_chMDBs(k, withInfo=FALSE)){
   create_chMDB(k, "CortellisONT")
}
if(!"CortellisONT" %in% list_chMDBs(k, withInfo=TRUE)$name){
   contch <- as_chMDB(contf, k)
}else{
   contch <- get_chMDB(k, "CortellisONT")
}
