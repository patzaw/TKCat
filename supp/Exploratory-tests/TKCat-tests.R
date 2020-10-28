library(TKCat)

dscf <- "~/Tmp/dsc.rds"
if(file.exists(dscf)){
   dsc <- readRDS(dscf)
}else{
   dsc <- scan_fileMDBs(
      path="~/Shared/Data-Science/Data-Source-Model-Repository/"
   )
   saveRDS(dsc, file=dscf)
}
k <- chTKCat(port=9101L, password="")

mdsc <- c(dsc[9:11], TKCat(get_MDB(k, "HPO")))
