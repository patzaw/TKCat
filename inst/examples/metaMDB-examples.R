hpof <- read_fileMDB("~/Tmp/HPO")
cvf <- read_fileMDB("~/Tmp/ClinVar")
pmf <- read_fileMDB("~/Tmp/PubMed/")
# mbf <- read_fileMDB("~/Tmp/MetaBase")

rt <- get_shared_collections(cvf, pmf)
rt[which(rt$collection=="BE"), "table.y"] <- "PubMed_geneByMedgen"
rt[which(rt$collection=="BE"), "collection"] <- NA

cvpm <- merge(cvf, pmf, by=rt)
rt2 <- get_shared_collections(cvpm, hpof)
cvpmhp <- merge(cvpm, hpof, by=rt2[4,])

searchTerm <- stringr::regex("Epilepsy", ignore_case=TRUE)
scv <- cvf %>% 
   select(
      ClinVar_traits, ClinVar_traitNames, ClinVar_traitCref,
      ClinVar_entrezNames
   ) %>%
   filter(
      ClinVar_traitNames=stringr::str_detect(name, searchTerm),
      ClinVar_entrezNames=symbol %in% c("SLC1A1", "SV2A")
   )
spm <- pmf %>% 
   select(PubMed_medgenNames, PubMed_geneByPubmed) %>% 
   filter(
      PubMed_medgenNames=stringr::str_detect(name, searchTerm),
      PubMed_geneByPubmed=entrez %in% c(6505, 9900)
   )
scvpm <- merge(scv, spm)
shp <- hpof %>% 
   filter(HPO_diseases=stringr::str_detect(label, searchTerm))
rt <- get_shared_collections(scvpm, shp)
scvpmhp <- merge(scvpm, shp, by=rt[4,])
plot(data_model(scvpmhp, rtOnly=TRUE, recursive=TRUE))
