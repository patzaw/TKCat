## Managing local collections ----
tkcatEnv <- new.env(hash=TRUE, parent=emptyenv())
assign(
   x="COLLECTIONS",
   value=tibble(
      title=character(), description=character(), json=character()
   ),
   envir=tkcatEnv
)

## Importing built-in collections when loading the library ----
.onLoad <- function(libname, pkgname){
   assign(
      x="COL_SCHEMA",
      value=paste(readLines(system.file(
         "Collections", "Collection-Schema.json",
         package=pkgname
      )), collapse="\n"),
      envir=tkcatEnv
   )
   files <- list.files(
      path=system.file(
         "Collections", "Built-in",
         package=pkgname
      ),
      pattern="[.]json$", ignore.case=TRUE,
      full.names=TRUE
   )
   for(f in files){
      import_local_collection(f)
   }
}
