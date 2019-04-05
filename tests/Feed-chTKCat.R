library(here)
library(glue)
library(TKCat)

dsr <- "~/Shared/Data-Science/Data-Source-Model-Repository"
tkcon <- chTKCat()

resources <- c(
   "ClinVar",
   "CortellisDD",
   "CortellisID",
   "CortellisONT",
   "CortellisTAR",
   "CTen",
   "GO",
   "HPO",
   "MetaBase",
   "PubMed",
   "Reactome",
   NULL
)
for(r in resources){
   et <- listMDBs(tkcon)
   if(r %in% et$name){
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
      loaded <- TKCat:::addMDBToChTKCat(tkcon, toload)
      rm(toload)
      gc()
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
