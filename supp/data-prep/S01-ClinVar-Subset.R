library(TKCat)
library(here)

k <- chTKCat(port=9101L)

clinvar <- get_MDB(k, "ClinVar")

# clinvar$ClinVar_entrezNames %>%
#    filter(nchar(symbol) <= 6) %>% pull(symbol) %>% sample(100)
gToTake <- c(
   "APOC3", "AQR", "CBLN3", "CBX4", "CBY1", "CCNJL", "CDSN", "CHI3L1", 
   "CKB", "CLCC1", "CLOCK", "COPS8", "CTSD", "CTTN", "CYB5D2", "DGLUCY", 
   "DIMT1", "DMTF1", "DNAJB2", "DUOXA2", "DUSP8", "DYNLL1", "FBXO40", 
   "GAB3", "GATB", "GLIS3", "HAPLN3", "ICOS", "IDS", "IFIT5", "INSRR", 
   "IQCE", "KCNG2", "KDM2B", "KLK11", "KRT23", "LAMA4", "LANCL2", 
   "LPAR6", "LRRC40", "MAFF", "MIR617", "MIR98", "MPP6", "MRPL52", 
   "MTUS1", "MYL4", "MYRF", "NCLN", "NDFIP2", "NDRG2", "NEU2", "NFRKB", 
   "NUP37", "NYNRIN", "ODAD3", "OMG", "OR2C3", "OR5M1", "P2RY14", 
   "PAG1", "PGAM4", "PGS1", "PHF20", "PHKG1", "PIK3R2", "PMM1", 
   "PNKY", "PRR15L", "PRSS48", "PSME1", "RCC1", "RGL3", "RNF225", 
   "S1PR3", "SEMA3B", "SGF29", "SH3GL1", "SH3RF2", "SHTN1", "SLAMF9", 
   "SMR3A", "SOCAR", "SSR4", "STATH", "TEX2", "TIAM2", "TJP3", "TRAK1", 
   "TRIM65", "TRMT13", "UGT1A8", "VPS4A", "ZBTB26", "ZNF302", "ZNF467", 
   "ZNF488", "ZNF512", "ZNF641", "ZNF79"
)
fcv <- clinvar %>% 
   filter(ClinVar_entrezNames=symbol %in% gToTake)
fcv <- fcv %>% 
   filter(
      ClinVar_ReferenceClinVarAssertion=reviewStatus %in% c(
         "criteria provided, multiple submitters, no conflicts",
         "reviewed by expert panel"
   ))
dbi <- db_info(fcv)
dbi$description <- paste(
   dbi$description,
   "This is a very small subset of ClinVar!",
   "Visit the reference URL for more information."
)
db_info(fcv) <- dbi
as_fileMDB(fcv, path=here("inst/examples"), htmlModel=FALSE)
