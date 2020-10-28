library(TKCat)

k <- chTKCat(
   host="bel040344",
   port=9101, http=9111,
   user="default", password=""
)
explore_MDBs(k)

dscf <- "~/Tmp/dsc.rds"
if(file.exists(dscf)){
   dsc <- readRDS(dscf)
}else{
   dsc <- scan_fileMDBs(
      path="~/Shared/Data-Science/Data-Source-Model-Repository/"
   )
   saveRDS(dsc, file=dscf)
}
explore_MDBs(dsc)
