library(TKCat)
library(here)
library(BED)

k <- chTKCat(port=9101L)

chembl <- get_MDB(k, "CHEMBL")
scv <- read_fileMDB(here("inst/examples/ClinVar"))
se <- sort(scv$ClinVar_entrezNames$entrez)[seq(1, 20, by=2)]
sep <- convBeIds(
   se, from="Gene", from.source="EntrezGene", from.org="Homo sapiens",
   to="Peptide", to.source="Uniprot"
)$to %>% setdiff(NA)
fromch <- sort(chembl$CHEMBL_component_sequence$accession)[
   round(seq(9, 9000, length.out=30))
]
toTake <- union(fromch, sep) %>% 
   intersect(chembl$CHEMBL_component_sequence$accession)
fchembl <- filter(chembl, CHEMBL_component_sequence=accession %in% toTake)


dbi <- db_info(fchembl)
dbi$description <- paste(
   dbi$description,
   "This is a very small subset of CHEMBL!",
   "Visit the reference URL for more information."
)
db_info(fchembl) <- dbi
as_fileMDB(fchembl, path=here("inst/examples"), htmlModel=FALSE)
