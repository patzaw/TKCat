library(here)

## Build and copy vignettes ----
devtools::build_vignettes()
dir.create(here("inst/doc"), showWarnings=FALSE)
for(f in list.files(here("doc"))){
   file.copy(
      file.path(here("doc"), f), file.path(here("vignettes"), f),
      overwrite=TRUE
   )
   file.copy(
      file.path(here("doc"), f), file.path(here("inst/doc"), f),
      overwrite=TRUE
   )
   if(sub("^.*[.]", "", f)=="html"){
      file.copy(
         file.path(here("doc"), f), file.path(here("docs"), f),
         overwrite=TRUE
      )
   }
   file.remove(file.path(here("doc"), f))
}
file.remove("doc")

## Build and check package ----
pv <- desc::desc_get_version(here())
system(paste(
   sprintf("cd %s", here("..")),
   "R CMD build TKCat",
   sprintf("R CMD check --as-cran devTKCat_%s.tar.gz", pv),
   sep=" ; "
))