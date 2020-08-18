library(devTKCat)

hpof <- read_fileMDB("~/Tmp/HPO")
cvf <- read_fileMDB("~/Tmp/ClinVar")

hpcv <- metaMDB(
   MDBs=list(HPO=hpof, ClinVar=cvf),
   relationalTables=list(),
   dataModel=c(data_model(hpof), data_model(cvf)),
   dbInfo=list(name="HPO+MB")
)
data_model(hpcv) %>% auto_layout(force=TRUE) %>% plot()

shpcv <- hpcv[c("HPO_hp", "HPO_synonyms")]
shpcv <- hpcv[c("HPO_hp", "ClinVar_entrezNames")]
