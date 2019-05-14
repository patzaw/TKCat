library(here)
library(glue)
library(TKCat)

dsr <- "~/Shared/Data-Science/Data-Source-Model-Repository"
tkcon <- chTKCat()

test <- NA
while(is.na(test)){
   test <- readline("Test only? TRUE (default) or FALSE: ")
   if(test==""){
      test <- "TRUE"
   }
   if(toupper(test) %in% c("TRUE", "FALSE")){
      test <- as.logical(test)
   }else{
      test <- NA
   }
}

overwrite <- NA
while(is.na(overwrite)){
   overwrite <- readline(
      "Overwrite existing ClickHouse data? TRUE or FALSE (default): "
   )
   if(overwrite==""){
      overwrite <- "FALSE"
   }
   if(toupper(overwrite) %in% c("TRUE", "FALSE")){
      overwrite <- as.logical(overwrite)
   }else{
      overwrite <- NA
   }
}

resources <- c(
   "HPO",
   "Reactome",
   "ClinVar",
   "CortellisDD",
   "CortellisID",
   "CortellisONT",
   "CortellisTAR",
   "CTen",
   "GO",
   "MetaBase",
   "PubMed",
   NULL
)
if(test){
   for(r in resources){
      message(glue("Testing {r}"))
      colMbFiles <- list.files(
         file.path(dsr, r, "model/Collections"),
         pattern="[.]json$", ignore.case=TRUE,
         full.names=TRUE
      )
      colMb <- NULL
      for(f in colMbFiles){
         colMb <- colMb %>%
            bind_rows(readCollectionMembers(f))
      }
      toload <- readInternalMDB(
         dataModel=readSQLDataModel(file.path(dsr, r, glue("model/{r}.sql"))),
         descriptionFile=file.path(dsr, r, "DESCRIPTION.json"),
         directory=file.path(dsr, r, "data")
      )
      collectionMembers(toload) <- colMb
      rm(toload)
      gc()
   }
}else{
   for(r in resources){
      et <- listMDBs(tkcon)
      if(r %in% et$name & !overwrite){
         message(glue("{r} is already in the chTKCat"))
      }else{
         message(glue("Importing {r}"))
         colMbFiles <- list.files(
            file.path(dsr, r, "model/Collections"),
            pattern="[.]json$", ignore.case=TRUE,
            full.names=TRUE
         )
         colMb <- NULL
         for(f in colMbFiles){
            colMb <- colMb %>%
               bind_rows(readCollectionMembers(f))
         }
         toload <- readInternalMDB(
            dataModel=readSQLDataModel(file.path(dsr, r, glue("model/{r}.sql"))),
            descriptionFile=file.path(dsr, r, "DESCRIPTION.json"),
            directory=file.path(dsr, r, "data")
         )
         collectionMembers(toload) <- colMb
         TKCat:::rmMDBFromChTKCat(tkcon, r)
         loaded <- TKCat:::addMDBToChTKCat(tkcon, toload)
         rm(toload)
         gc()
      }
   }
}

# ttt <- c(-100, 50, 00, -50, 100)
# visNetwork(nodes=tibble(id=ttt, label=ttt, x=ttt, y=ttt), edges=tibble(from=numeric(), to=numeric()), width="100%") %>%
#    visNodes(physics=FALSE)
#    # visPhysics(
#    #    solver="repulsion",
#    #    repulsion=list(
#    #       nodeDistance=100,
#    #       springLength=100,
#    #       springConstant=0.001,
#    #       damping=1
#    #    )
#    # ) %>%
#    # visLayout(randomSeed=2) %>%
#    # visOptions(selectedBy="label", highlightNearest=TRUE) %>%
#    # visIgraphLayout(smooth=TRUE, type="full", randomSeed=2)
