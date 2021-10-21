library(here)

##############################@
## Build and copy vignettes ----
rmarkdown::render(here("README.Rmd"))
devtools::build_vignettes(clean=FALSE, quiet=TRUE, install=TRUE)
cssToClean <- here("vignettes/libs/bootstrap-3.3.5/css")
unlink(
   file.path(cssToClean, setdiff(list.files(cssToClean), "cerulean.min.css")),
   recursive=TRUE
)
toClean <- c(
   here("vignettes/libs/vis-9.1.0/img"),
   here("vignettes/libs/vis-9.1.0/vis-network.min.css")
)
unlink(toClean, recursive=TRUE)
dir.create(here("inst/doc"), showWarnings=FALSE)
file.copy(
   here("vignettes/libs"),
   here("inst/doc"),
   overwrite=TRUE, recursive=TRUE
)
file.copy(
   here("vignettes/libs"),
   here("docs/"),
   overwrite=TRUE, recursive=TRUE
)
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

##############################@
## Build and check package ----
pv <- desc::desc_get_version(here())
system(paste(
   sprintf("cd %s", here("..")),
   "R CMD build TKCat",
   sprintf("R CMD check --as-cran TKCat_%s.tar.gz", pv),
   sep=" ; "
))
install.packages(here(sprintf("../TKCat_%s.tar.gz", pv)), repos=NULL)
