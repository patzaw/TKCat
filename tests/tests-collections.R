library(magrittr)

###############################################################################@
## Built-in collections ----
jsonvalidate::json_validate(
   json="inst/Collections/Built-in/BE-Collection.json",
   schema="inst/Collections/Collection-Schema.json"
)
jsonvalidate::json_validate(
   json="inst/Collections/Built-in/Condition-Collection.json",
   schema="inst/Collections/Collection-Schema.json"
)

###############################################################################@
## BE collection members ----
resources <- readLines("~/Shared/Data-Science/Data-Source-Model-Repository/01-pgodard-Resources/All-Resources.txt")
for(r in resources){
   message("")
   message(r)
   f <- file.path(
      "~/Shared/Data-Science/Data-Source-Model-Repository/", r,
      "model/Collections/BE-members.json"
   )
   if(file.exists(f)){
      if(jsonvalidate::json_validate(
         schema="inst/Collections/Built-in/BE-Collection.json",
         json=f,
         verbose=TRUE
      )){
         raw <- readLines(f) %>% paste(collapse="\n")
         def <- jsonlite::fromJSON(raw)
         print(def$'$id')
      }else{
         message("Not a valid BE collection")
      }
   }else{
      message("No BE collection")
   }
}

###############################################################################@
## Condition collection members ----
for(r in resources){
   message("")
   message(r)
   f <- file.path(
      "~/Shared/Data-Science/Data-Source-Model-Repository/", r,
      "model/Collections/Condition-members.json"
   )
   if(file.exists(f)){
      if(jsonvalidate::json_validate(
         schema="inst/Collections/Built-in/Condition-Collection.json",
         json=f,
         verbose=TRUE
      )){
         raw <- readLines(f) %>% paste(collapse="\n")
         def <- jsonlite::fromJSON(raw)
         print(def$'$id')
      }else{
         message("Not a valid Condition collection")
      }
   }else{
      message("No Condition collection")
   }
}
