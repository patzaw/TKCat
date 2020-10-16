library(devTKCat)

dsc <- scan_fileMDBs(
   path="~/Shared/Data-Science/Data-Source-Model-Repository/",
   subdirs=c("HPO", "Reactome")
)
dsc <- scan_fileMDBs(
   path="~/Shared/Data-Science/Data-Source-Model-Repository/"
)
k <- chTKCat(port=9201L, password="")

mdsc <- c(dsc[9:11], TKCat(get_MDB(k, "HPO")))
