library(TKCat)
tkcon <- chTKCat("bel040344")
listMDBs(tkcon)
mb <- chMDB(tkcon, "MetaBase")
plot(mb)

toRet <- 
   mb[["MetaBase_Imagemaps"]] %>%
   select(-pngB64) %>%
   rename("Pathway"="name", "imagemap_id"="id") %>%
   inner_join(
      mb[["MetaBase_ImagemapsShapes"]],
      by="imagemap_id"
   ) %>%
   rename("shape_id"="id", "shape_name"="name") %>%
   inner_join(
      mb[["MetaBase_ImagemapsClasses"]],
      by="shape_id"
   ) %>%
   inner_join(
      mb[["MetaBase_Objects"]] %>% rename("object_name"="name"),
      by=c("object_id"="id")
   )

library(BED)
toRet <- convDfBeIds(
   toRet %>%
      mutate(object_id=as.character(object_id)) %>%
      as.data.frame(stringsAsFactors=FALSE),
   idCol="object_id",
   from="Object", from.source="MetaBase_object", from.org="human",
   to="Gene", to.source="EntrezGene"
) %>%
   as_tibble() %>%
   rename("object_id"="conv.from", "EntrezGene"="conv.to") %>%
   filter(!is.na(EntrezGene))
toRet <- toRet %>%
   bind_cols(
      getBeIdDescription(
         ids=toRet$EntrezGene,
         be="Gene", source="EntrezGene", organism="human"
      ) %>% as_tibble()
   )
