library(devTKCat)

hpof <- read_fileMDB("~/Tmp/HPO")
cvf <- read_fileMDB("~/Tmp/ClinVar")
pmf <- read_fileMDB("~/Tmp/PubMed/")
# mbf <- read_fileMDB("~/Tmp/MetaBase")

rt <- get_shared_collections(cvf, pmf)
rt[which(rt$collection=="BE"), "table.y"] <- "PubMed_geneByMedgen"
rt[which(rt$collection=="BE"), "collection"] <- NA

cvpm <- full_join_MDBs(cvf, pmf, by=rt)

scv <- cvf %>% 
   select(
      ClinVar_traits, ClinVar_traitNames, ClinVar_traitCref,
      ClinVar_entrezNames
   ) %>%
   filter(
      ClinVar_traitNames=stringr::str_detect(name, "Epilepsy"),
      ClinVar_entrezNames=symbol %in% c("SLC1A1", "SV2A")
   )
spm <- pmf %>% 
   select(PubMed_medgenNames, PubMed_geneByPubmed) %>% 
   filter(
      PubMed_medgenNames=stringr::str_detect(name, "Epilepsy"),
      PubMed_geneByPubmed=entrez %in% c(6505, 9900)
   )
scvpm <- full_join_MDBs(scv, spm)


hpcv <- metaMDB(
   MDBs=list(HPO=hpof, ClinVar=cvf),
   relationalTables=list(),
   dataModel=c(data_model(hpof), data_model(cvf)),
   dbInfo=list(name="HPO+MB")
)
data_model(hpcv) %>% auto_layout(force=TRUE) %>% plot()

shpcv <- hpcv[c("HPO_hp", "HPO_synonyms")]
shpcv <- hpcv[c("HPO_hp", "ClinVar_entrezNames")]
