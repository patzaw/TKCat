library(here)
source("~/opt/KMT.R")

###############################################################################@
## Initialization ----
tbkm <- read_KMR(here("supp/TBKM-requirements/TBKM"))

###############################################################################@
## Collibra ----

tbkm <- add_property_values(
   tbkm, "Domain (Collibra)", "value",
   values=c(
      "Biochemical information", "Genetics", "Multi omics",
      "Real world data", "Scientific Information", "UCB Clinical Samples",
      "Experiments", "Experiments/Multi omics",
      "Assays and reference data", "Molecules"
   )
)

tbkm <- add_property_values(
   tbkm, "Community (Collibra)", "value",
   values=c(
      "Early Solutions Internal Data Catalog",
      "Early Solutions External Data Catalog"
   )
)

tbkm <- add_property_values(
   tbkm, "Restrictions (Collibra)", "value",
   values=c(
      "Restrictions on data use",
      "Restrictions on data sharing/disclosure",
      "Restrictions on the use or sharing of derived data and outputs",
      "No Restrictions"
   )
)

tbkm <- add_property_values(
   tbkm, "License type (Collibra)", "value",
   values=c(
      "Open access with standard license agreement e.g. Creative Commons",
      "Open access with specific Data Use Agreement",
      "Commercial access with specific Data Use Agreement",
      "Consortium access with  specific Data Use Agreement",
      "No license/ License not known"
   )
)

tbkm <- add_property_values(
   tbkm, "Data Protection Category (Collibra)", "value",
   values=c(
      "Non-personal data",
      "Anonymized (previously personal) data",
      "Pseudonymized personal data"
   )
)

tbkm <- add_property_values(
   tbkm, "Drug development stage (Collibra)", "value",
   values=c(
      "Target Identification", "Target Validation", "Hit finding",
      "Biomarker Development", "Post-marketing",
      "Clinical research and development", "Pre-clinical",
      "Lead generation (Hit to Lead)",
      "Lead optimization to candidate selection", "Regulatory approval"
   )
)


###############################################################################@
## Sample and condition features ----

### General  <> ----

#### organism ----
tbkm <- add_property_values(
   tbkm, "organism", "reference",
   values="NCBI"
)


#### biological model ----
tbkm <- add_property_values(
   tbkm, "biological model", "value",
   values=c(
      "in vivo", "in vitro"
   )
)

### In vivo models  <> ----

#### gender ----
tbkm <- add_property_values(
   tbkm, "gender", "value",
   values=c(
      "F"="female", "female"="female", "woman"="female",
      "M"="male", "male"="male", "man"="male"
   )
)

#### Hardy scale ----
tbkm <- add_property_values(
   tbkm, "Hardy scale", "value",
   values=c(
      "1"="Violent and fast death Deaths due to accident, blunt force trauma or suicide, terminal phase estimated at < 10 min",
      "2"="Fast death of natural causes Sudden unexpected deaths of people who had been reasonably healthy, after a terminal phase estimated at < 1 hr (with sudden death from a myocardial infarction as a model cause of death for this category)",
      "3"="Intermediate death Death after a terminal phase of 1 to 24 hrs (not classifiable as 2 or 4); patients who were ill but death was unexpected.",
      "4"="low death Death after a long illness, with a terminal phase longer than 1 day (commonly cancer or chronic pulmonary disease); deaths that are not unexpected 
0) Ventilator Case All cases on a ventilator immediately before death."
   )
)

#### strain ----
tbkm <- add_property_values(
   tbkm, "strain", "reference",
   values=c(
      "JAX"="Mouse strains in the Jackson Laboratory: https://www.jax.org/strain",
      "Charles River"="Animal strains in the Charles River Laboratories: https://www.criver.com/products-services/find-model/",
      "hPSCreg"="Human Pluripotent Stem Cell Registry: https://hpscreg.eu/"
   )
)

#### tissue ----
tbkm <- add_property_values(
   tbkm, "tissue", "reference",
   values=c(
      "Uberon"="Identifier should be provided without the 'Uberon_' or 'Uberon:' prefix. http://obofoundry.org/ontology/uberon.html"
   )
)
tbkm <- add_property_values(
   tbkm, "tissue", "side",
   values=c(
      "left", "right"
   )
)
tbkm <- add_property_values(
   tbkm, "tissue", "relative side",
   values=c(
      "ipsilateral", "contralateral"
   )
)

#### disease ----
tbkm <- add_property_values(
   tbkm, "disease", "reference",
   values=DODO::list_database()$database
)

### In vitro models <> ----

#### cell type ----
tbkm <- add_property_values(
   tbkm, "cell type", "reference",
   values=c(
      "Cell Ontology"="Identifier should be provided without the 'CL_' or 'CL:' prefix. http://obofoundry.org/ontology/cl.html"
   )
)

### Genetics <> ----

#### Biological entity ----
tbkm <- add_property_values(
   tbkm, "biological entity", "be",
   values=BED::listBe()
)
tbkm <- add_property_values(
   tbkm, "biological entity", "source",
   values=sort(setdiff(BED::bedCall(
      neo2R::cypher, query="MATCH (n:BEID) RETURN DISTINCT n.database AS db"
   )$db, c("BEDTech_gene", "BEDTech_transcript")))
)
tbkm <- add_property_values(
   tbkm, "biological entity", "organism",
   values=sort(BED::listOrganisms())
)

### Treatments  <> ----
tbkm <- add_property_values(
   tbkm, "compound", "reference",
   values=c(
      "CHEMBL"="https://www.ebi.ac.uk/chembl/compound_report_card/"
   )
)

###############################################################################@
## Dataset features ----

### dataset type ----
tbkm <- add_property_values(
   tbkm, "dataset type", "value",
   values=c("BE quantification", "BE feature quantification")
)

###############################################################################@
## Analyses ----

### correlation method ----
tbkm <- add_property_values(
   tbkm, "correlation method", "value",
   values=c("pearson", "spearman", "kendall")
)

### regulation impact ----
tbkm <- add_property_values(
   tbkm, "regulation impact", "value",
   values=c("activation", "inhibition", "unknown")
)

#### functional be list ----
tbkm <- add_property_values(
   tbkm, "functional be list", "reference",
   values=c(
      "MetaBase"="MetaBase pathways",
      "Reactome"="Reactome pathways",
      "GO BP"="Gene Ontology biological process",
      "GO MF"="Gene Ontology molecular function",
      "GO CC"="Gene Ontology cellular component",
      "MedGen PubMed"="Genes associated to MedGen terms in PubMed abstracts",
      "PanglaoDB"="Transcriptional markers of cell types from PangaloDB"
   )
)

###############################################################################@
## Save requirements ----
fp <- here("supp/TBKM-requirements/TBKM")
if(file.exists(fp)){
   file.rename(fp, here(sprintf("supp/TBKM-requirements/%s-TBKM", Sys.time())))
}
ftbkm <- as_fileMDB(tbkm, here("supp/TBKM-requirements/"), htmlModel=FALSE)
