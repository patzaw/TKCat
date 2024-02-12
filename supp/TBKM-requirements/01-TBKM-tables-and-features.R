library(TKCat)
library(here)

###############################################################################@
## Initialization ----
tbkm <- create_KMR(
   name="TBKM",
   title="Translational Bioinformatics Knowledge Management",
   description="Requirements for integrating knwoledge from translational bioinformatics activities",
   version="0.1",
   maintainer="Patrice Godard <patrice.godard@ucb.com>"
)


###############################################################################@
## Units ----

### Duration units ----
tbkm <- add_unit_def(tbkm, "duration", "s", "Duration in seconds") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "duration", "min", "Duration in minutes") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "duration", "days", "Duration in days") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "duration", "h", "Duration in hours") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "duration", "months", "Duration in month") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "duration", "years", "Duration in years") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "duration", "weeks", "Duration in weeks") %>% 
   as_memoMDB() %>% as_KMR()

### Concentration units ----
tbkm <- add_unit_def(tbkm, "concentration", "M", "Concentration in molars (moles per liter)") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "concentration", "mM", "Concentration in millimolars") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "concentration", "µM", "Concentration in micromolars") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "concentration", "nM", "Concentration in nanomolars") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "concentration", "pM", "Concentration in picomolars") %>% 
   as_memoMDB() %>% as_KMR()

### Dose units ----
tbkm <- add_unit_def(tbkm, "dose", "M", "Concentration in molars (moles per liter)") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "dose", "mM", "Concentration in millimolars") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "dose", "µM", "Concentration in micromolars") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "dose", "nM", "Concentration in nanomolars") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "dose", "pM", "Concentration in picomolars") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "dose", "mg/kg", "Dose in mg/kg") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "dose", "mg", "Dose in mg") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "dose", "vg", "Dose in vector genomes") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "dose", "vg/kg", "Dose in vector genomes per kg") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "dose", "IU", "Dose in infectious units") %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_unit_def(tbkm, "dose", "IU/kg", "Dose in infectious units per kg") %>% 
   as_memoMDB() %>% as_KMR()

###############################################################################@
## General features ----

### name ** ----
tbkm <- add_feature_def(
   tbkm,
   "name",
   "name/identifier of a record",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

### description ** ----
tbkm <- add_feature_def(
   tbkm,
   "description",
   "description of a record",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

### table ref ** ----
tbkm <- add_feature_def(
   tbkm,
   "table ref",
   "Reference to a table of records",
   properties=list(
      "value"=list(
         type="table",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

###############################################################################@
## Collibra ----

### Features ----
tbkm <- add_feature_def(
   tbkm,
   "Domain (Collibra)",
   "Scientific domain covered by the data",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_feature_def(
   tbkm,
   "Primary Use Case (Collibra)",
   "UCB primary reason the data was generated or accessed",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_feature_def(
   tbkm,
   "Restrictions (Collibra)",
   "Limitations on data access and usage",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_feature_def(
   tbkm,
   "Restrictions summary (Collibra)",
   "Additional details on restrictions (e.g., geography, function, contract, etc.)",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_feature_def(
   tbkm,
   "License type (Collibra)",
   "Type of License that dictates the data use terms and conditions",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_feature_def(
   tbkm,
   "License (Collibra)",
   "UCB relationship with the data provider e.g. public access, academic partnership, commercial license",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_feature_def(
   tbkm,
   "Data Protection Category (Collibra)",
   "Level of patient identification within the dataset",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_feature_def(
   tbkm,
   "Nature of data (Collibra)",
   "Type(s) of data within the asset. This can include clarification on the entities captured in the asset (e.g. genes, proteins).",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_feature_def(
   tbkm,
   "Community (Collibra)",
   "Collibra community of users",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_feature_def(
   tbkm,
   "Alias (Collibra)",
   "Short name of the asset. This can be an acronym or abbreviation.",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_feature_def(
   tbkm,
   "Source of data (Collibra)",
   "Main source(s) of the captured data or method of data collection/generation",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_feature_def(
   tbkm,
   "Refresh Frequency (Collibra)",
   "How often the asset is updated",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_feature_def(
   tbkm,
   "Drug development stage (Collibra)",
   "Stages in drug development process where the asset may be relevant or valuable.",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

### Tables ----

#### collibra ** ----
optFeatures <- c(
   "Alias", "Source of data", "Refresh Frequency", "Restrictions summary"
) %>% 
   paste("(Collibra)")
tbkm <- add_table_def(
   tbkm,
   "collibra",
   "A table with additional metadata for the Collibra catalog",
   mandatory_features=c(
      "Domain", "Primary Use Case", "Restrictions", "License type", "License",
      "Data Protection Category",
      "Nature of data", "Community"
   ) %>% 
      paste("(Collibra)")
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_table_features(
   tbkm,
   "collibra",
   features=optFeatures
) %>% 
   as_memoMDB() %>% as_KMR()

#### collibra drug development stage ** ----
tbkm <- add_table_def(
   tbkm,
   "collibra drug development stage",
   "Drug development stage of applications to be indicated in Collibra catalog",
   mandatory_features=c(
      "Drug development stage"
   ) %>% 
      paste("(Collibra)")
) %>% 
   as_memoMDB() %>% as_KMR()

###############################################################################@
## Samples and conditions ----

### Features ----

#### References ----

##### sample ref ** ----
tbkm <- add_feature_def(
   tbkm,
   "sample ref",
   "Reference to a sample table",
   properties=list(
      "value"=list(
         type="field",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### condition ref ** ----
tbkm <- add_feature_def(
   tbkm,
   "condition ref",
   "Reference to a condition table",
   properties=list(
      "value"=list(
         type="field",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_feature_def(
   tbkm,
   "ref. condition ref",
   "Reference to a condition table: reference condition",
   properties=list(
      "value"=list(
         type="field",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### General ----

##### organism ** ----
tbkm <- add_feature_def(
   tbkm,
   "organism",
   "The name of the biological species",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      ),
      "identifier"=list(
         type="character",
         description="Identifier in reference database",
         mandatory=TRUE
      ),
      "reference"=list(
         type="character",
         description="Reference database",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()


##### biological model ** ----
tbkm <- add_feature_def(
   tbkm,
   "biological model",
   "The type biological model",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### treatment ** ----
tbkm <- add_feature_def(
   tbkm,
   "treatment",
   "description of a treatment applied on the experimental model (control, compound, drug...)",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### In vivo models ----

##### gender ** ----
tbkm <- add_feature_def(
   tbkm,
   "gender",
   "Subject gender",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### age ** ----
tbkm <- add_feature_def(
   tbkm,
   "age",
   "Elapsed time since birth",
   properties=list(
      "value"=list(
         type="numeric",
         mandatory=TRUE,
         measurement="duration"
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### ethnicity ** ----
tbkm <- add_feature_def(
   tbkm,
   "ethnicity",
   "Subject origin",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### Hardy scale ** ----
tbkm <- add_feature_def(
   tbkm,
   "Hardy scale",
   "Death classification based on the 4-point Hardy Scale",
   properties=list(
      "value"=list(
         type="integer",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### strain ** ----
tbkm <- add_feature_def(
   tbkm,
   "strain",
   "A particular breed, stock, or variety of a lab model",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      ),
      "identifier"=list(
         type="character",
         description="Identifier in reference database",
         mandatory=TRUE
      ),
      "reference"=list(
         type="character",
         description="Reference database",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### tissue ** ----
tbkm <- add_feature_def(
   tbkm,
   "tissue",
   "Name of a biological tissue",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      ),
      "identifier"=list(
         type="character",
         description="Identifier in reference database",
         mandatory=TRUE
      ),
      "reference"=list(
         type="character",
         description="Reference database",
         mandatory=TRUE
      ),
      "side"=list(
         type="character",
         description="Sampling side",
         mandatory=FALSE
      ),
      "relative side"=list(
         type="character",
         description="Sampling relative side",
         mandatory=FALSE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### death cause ** ----
tbkm <- add_feature_def(
   tbkm,
   "death cause",
   "Description of the cause of death",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### disease ** ----
tbkm <- add_feature_def(
   tbkm,
   "disease",
   "A disorder of structure or function",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      ),
      "identifier"=list(
         type="character",
         description="Identifier in reference database",
         mandatory=TRUE
      ),
      "reference"=list(
         type="character",
         description="Reference database",
         mandatory=TRUE
      ),
      "duration"=list(
         type="numeric",
         description="Elapsed time since disease onset",
         mandatory=FALSE,
         measurement="duration"
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### compound ** ----
tbkm <- add_feature_def(
   tbkm,
   "compound",
   "A chemical compound or a drug",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      ),
      "identifier"=list(
         type="character",
         description="Identifier in reference database",
         mandatory=TRUE
      ),
      "reference"=list(
         type="character",
         description="Reference database",
         mandatory=TRUE
      ),
      "dose"=list(
         type="numeric",
         description="Applied compound dose",
         mandatory=FALSE,
         measurement="dose"
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### In vitro models ----

##### cell type ** ----
tbkm <- add_feature_def(
   tbkm,
   "cell type",
   "The name of the cell type",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      ),
      "identifier"=list(
         type="character",
         description="Identifier in reference database",
         mandatory=TRUE
      ),
      "reference"=list(
         type="character",
         description="Reference database",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### culture type ** ----
tbkm <- add_feature_def(
   tbkm,
   "culture type",
   "The name of the type of culture",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### time in vitro ** ----
tbkm <- add_feature_def(
   tbkm,
   "time in vitro",
   "Culture time in vitro",
   properties=list(
      "value"=list(
         type="numeric",
         mandatory=TRUE,
         measurement="duration"
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### Genetics ----

##### biological entity ** ----
tbkm <- add_feature_def(
   tbkm,
   "biological entity",
   "Biological entity as defined in BED",
   properties=list(
      "value"=list(
         type="character",
         description="BE name or symbol",
         mandatory=TRUE
      ),
      "impact"=list(
         type="character",
         description="How the BE is impacted (e.g. LoF, CRISPRa...)",
         mandatory=TRUE
      ),
      "id"=list(
         type="character",
         description="Identifier in reference database",
         mandatory=TRUE
      ),
      "be"= list(
         type="character",
         description="Type of BE (e.g. Gene, Peptide...)",
         mandatory=TRUE
      ),
      "source"=list(
         type="character",
         description="Reference database (e.g. Ens_gene, Uniprot)",
         mandatory=TRUE
      ),
      "organism"=list(
         type="character",
         description="Organism of origin",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### genetic variant ** ----
tbkm <- add_feature_def(
   tbkm,
   "genetic variant",
   "Genetic variant such as SNP or chromosome abnormalities",
   properties=list(
      "value"=list(
         type="character",
         description="Name of the variant",
         mandatory=TRUE
      ),
      "id"=list(
         type="character",
         description="Identifier in reference database",
         mandatory=TRUE
      ),
      "reference"=list(
         type="character",
         description="Reference database",
         mandatory=TRUE
      ),
      "number"=list(
         type="integer",
         description="number of copies",
         mandatory=FALSE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### Phenotypes ----

##### Number of seizures ** ----
tbkm <- add_feature_def(
   tbkm,
   "number of seizures",
   "Number of recorded seizures during a time period",
   properties=list(
      "value"=list(
         type="integer",
         description="The number of recorded seizures",
         mandatory=TRUE
      ),
      "period"=list(
         type="numeric",
         description="Duration of the recording period",
         mandatory=FALSE,
         measurement="duration"
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

### Tables ----

#### samples ** ----
optFeatures <- c(
   "organism", "biological model", "treatment",
   "gender", "age", "ethnicity", "Hardy scale", "strain",
   "death cause", "disease", "compound",
   "tissue",
   "cell type", "culture type", "time in vitro",
   "biological entity", "genetic variant", "number of seizures"
)
tbkm <- add_table_def(
   tbkm,
   "samples",
   "A table listing samples and associated features",
   mandatory_features=c("name", "description")
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_table_features(
   tbkm,
   "samples",
   features=c("sample ref", optFeatures)
) %>% 
   as_memoMDB() %>% as_KMR()

#### conditions ** ----
tbkm <- add_table_def(
   tbkm,
   "conditions",
   "A table listing conditions and associated features. A condition correspond to a set of samples considered as a coherent group.",
   mandatory_features=c("name", "description")
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_table_features(
   tbkm,
   "conditions",
   features=optFeatures
) %>% 
   as_memoMDB() %>% as_KMR()

#### shared sample features ** ----
tbkm <- add_table_def(
   tbkm,
   "shared sample features",
   "A table listing features associated to all records in samples or conditions tables.",
   mandatory_features=c("table ref")
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_table_features(
   tbkm,
   "shared sample features",
   features=optFeatures
) %>% 
   as_memoMDB() %>% as_KMR()

#### samples per condition ** ----
tbkm <- add_table_def(
   tbkm,
   "samples per condition",
   description="Association table between samples and conditions",
   mandatory_features=c("sample ref", "condition ref")
) %>% 
   as_memoMDB() %>% as_KMR()


###############################################################################@
## Reports ----

### Features ----

#### References ----

##### report ref ** ----
tbkm <- add_feature_def(
   tbkm,
   "report ref",
   "Reference to a report table",
   properties=list(
      "value"=list(
         type="field",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### Others ----

##### file ** ----
tbkm <- add_feature_def(
   tbkm,
   "file",
   "File in base64 format",
   properties=list(
      "name"=list(
        type="character",
        mandatory=TRUE,
        description="Name of the file, including its extension (e.g. 'S01-report.html')"
      ),
      "title"=list(
         type="character",
         mandatory=TRUE,
         description="Title of the file"
      ),
      "value"=list(
         type="base64",
         mandatory=TRUE,
         description="The file itself"
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

### Tables ----

#### reports ** ----
tbkm <- add_table_def(
   tbkm,
   "reports",
   description="List of reports",
   mandatory=c(
      "file", "description"
   )
) %>% 
   as_memoMDB() %>% as_KMR()


###############################################################################@
## Datasets ----

### Features ----

#### References ----

##### dataset reference ** ----
tbkm <- add_feature_def(
   tbkm,
   "dataset ref",
   "Reference to the description of a dataset",
   properties=list(
      "value"=list(
         type="field",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### Others ----

### dataset type ** ----
tbkm <- add_feature_def(
   tbkm,
   "dataset type",
   "The type of the dataset",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE,
         description="The type of dataset (e.g. 'BE quantification')"
      ),
      "technology"=list(
         type="character",
         mandatory=TRUE,
         description="Underlying technology (e.g. 'RNA-seq', 'Affymetrix')"
      ),
      "measurement"=list(
         type="character",
         mandatory=TRUE,
         description="Type of measurement reported in the dataset (e.g. 'log2 CPM')"
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

### Tables ----

#### datasets ** ----
tbkm <- add_table_def(
   tbkm,
   "datasets",
   description="List of available datasets (matrices)",
   mandatory=c(
      "table ref", "description", "dataset type"
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_table_features(
   tbkm,
   "datasets",
   features=c("report ref")
) %>% 
   as_memoMDB() %>% as_KMR()


###############################################################################@
## Biological entities ----

### Features ----

#### References ----

##### beid ref ** ----
tbkm <- add_feature_def(
   tbkm,
   "beid ref",
   "Reference to a BEID in a BE collection",
   properties=list(
      "value"=list(
         type="field",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### Others ----

##### beid ** ----
tbkm <- add_feature_def(
   tbkm,
   "beid",
   "BE identifier in a BE collection",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

### Tables ----

#### biological entities ** ----
tbkm <- add_table_def(
   tbkm,
   "biological entities",
   description="Table of biological entity identifiers",
   mandatory=c("beid"),
   collection="BE"
) %>% 
   as_memoMDB() %>% as_KMR()

###############################################################################@
## Analyses features ----

### References ----

#### analysis ref ** ----
tbkm <- add_feature_def(
   tbkm,
   "analysis ref",
   "Reference to a description of an analysis",
   properties=list(
      "value"=list(
         type="field",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### feature column ** ----
tbkm <- add_feature_def(
   tbkm,
   "feature column",
   "A column in a table",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      ),
      "table"=list(
         type="table",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

### Others ----

#### p-value ** ----
tbkm <- add_feature_def(
   tbkm,
   "p-value",
   "p-value from a statistical test",
   properties=list(
      "value"=list(
         type="numeric",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

####  FDR ** ----
tbkm <- add_feature_def(
   tbkm,
   "FDR",
   "False Discovery Rate from a statistical test",
   properties=list(
      "value"=list(
         type="numeric",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

###############################################################################@
## Differential expression ----

### Features ----

#### logFC ** ----
tbkm <- add_feature_def(
   tbkm,
   "logFC",
   "Log2 fold change between 2 conditions",
   properties=list(
      "value"=list(
         type="numeric",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

### Tables ----

#### differential expression ** ----
tbkm <- add_table_def(
   tbkm,
   "differential expression",
   description="Results of differential expression (DE) analyses",
   mandatory=c(
      "analysis ref", "beid ref", "logFC", "p-value", "FDR" 
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### DE analyses ** ----
tbkm <- add_table_def(
   tbkm,
   "DE analyses",
   description="Description of differential expression analyses",
   mandatory=c(
      "name", "condition ref", "ref. condition ref" 
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_table_features(
   tbkm,
   "DE analyses",
   features=c("dataset ref", "description", "report ref")
) %>% 
   as_memoMDB() %>% as_KMR()

###############################################################################@
## Correlation with expression ----

### Features ----

##### correlation method ** ----
tbkm <- add_feature_def(
   tbkm,
   "correlation method",
   "Method for measuring correlation",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### correlation ** ----
tbkm <- add_feature_def(
   tbkm,
   "correlation",
   "Correlation value between two variables",
   properties=list(
      "value"=list(
         type="numeric",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

### Tables ----

#### expression correlation ** ----
tbkm <- add_table_def(
   tbkm,
   "expression correlation",
   description="Results of differential expression (DE) analyses",
   mandatory=c(
      "analysis ref", "beid ref", "correlation", "p-value", "FDR" 
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### EC analyses ** ----
tbkm <- add_table_def(
   tbkm,
   "EC analyses",
   description="Description of correlation with expression analyses",
   mandatory=c(
      "name", "condition ref", "feature column", "correlation method"
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_table_features(
   tbkm,
   "DE analyses",
   features=c("dataset ref", "description", "report ref")
) %>% 
   as_memoMDB() %>% as_KMR()

###############################################################################@
## Distribution of expression -----

### Features ----

#### exp. average ** ----
tbkm <- add_feature_def(
   tbkm,
   "exp. average",
   "Average of BE expression in log2 scale",
   properties=list(
      "value"=list(
         type="numeric",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### exp. SD ** ----
tbkm <- add_feature_def(
   tbkm,
   "exp. SD",
   "Standard deviation of BE expression in log2 scale",
   properties=list(
      "value"=list(
         type="numeric",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### exp. min ** ----
tbkm <- add_feature_def(
   tbkm,
   "exp. min",
   "Minimum of BE expression in log2 scale",
   properties=list(
      "value"=list(
         type="numeric",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### exp. max ** ----
tbkm <- add_feature_def(
   tbkm,
   "exp. max",
   "Maximum of BE expression in log2 scale",
   properties=list(
      "value"=list(
         type="numeric",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### exp. Q25 ** ----
tbkm <- add_feature_def(
   tbkm,
   "exp. Q25",
   "First quartile of BE expression in log2 scale",
   properties=list(
      "value"=list(
         type="numeric",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### exp. Q50 ** ----
tbkm <- add_feature_def(
   tbkm,
   "exp. Q50",
   "Median of BE expression in log2 scale",
   properties=list(
      "value"=list(
         type="numeric",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### exp. Q75 ** ----
tbkm <- add_feature_def(
   tbkm,
   "exp. Q75",
   "Third quartile of BE expression in log2 scale",
   properties=list(
      "value"=list(
         type="numeric",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### exp. MAD ** ----
tbkm <- add_feature_def(
   tbkm,
   "exp. MAD",
   "Medain absolute deviation of BE expression in log2 scale",
   properties=list(
      "value"=list(
         type="numeric",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

### Tables ----

#### expression distribution ** ----
tbkm <- add_table_def(
   tbkm,
   "expression distribution",
   description="BE expression distribution",
   mandatory=c(
      "analysis ref", "beid ref",
      "exp. average", "exp. SD",
      "exp. min", "exp. max",
      "exp. Q25", "exp. Q50", "exp. Q75", "exp. MAD"
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### ED analyses ** ----
tbkm <- add_table_def(
   tbkm,
   "ED analyses",
   description="Description of analyses of expression distribution",
   mandatory=c(
      "name", "condition ref",
      "exp. average", "exp. SD",
      "exp. min", "exp. max",
      "exp. Q25", "exp. Q50", "exp. Q75", "exp. MAD"
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_table_features(
   tbkm,
   "ED analyses",
   features=c("dataset ref", "description", "report ref")
) %>% 
   as_memoMDB() %>% as_KMR()

###############################################################################@
## BE list ----

### Features ----

#### References ----

##### be list ref ** ----
tbkm <- add_feature_def(
   tbkm,
   "be list ref",
   "Reference to a list of BE taken into account in an analysis (e.g. expressed genes, background list of genes...)",
   properties=list(
      "value"=list(
         type="field",
         mandatory=TRUE
      )
   )
)

#### Others ----

### Tables ----

#### be list description ** ----
tbkm <- add_table_def(
   tbkm,
   "be list description",
   description="Description of a list of biological entities",
   mandatory=c(
      "name", "description"
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### be list members ** ----
tbkm <- add_table_def(
   tbkm,
   "be list members",
   description="Members of a list of biological entities",
   mandatory=c(
      "be list ref", "beid ref"
   )
) %>% 
   as_memoMDB() %>% as_KMR()


###############################################################################@
## CoReMo ----

### Features ----

#### References ----

##### module ref ** ----
tbkm <- add_feature_def(
   tbkm,
   "module ref",
   "Reference to a BE module description",
   properties=list(
      "value"=list(
         type="field",
         mandatory=TRUE
      )
   )
)

#### Others ----

##### module members ** ----
tbkm <- add_feature_def(
   tbkm,
   "module members",
   "Number of elements in a BE module",
   properties=list(
      "value"=list(
         type="integer",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### CoReMo sub-module ** ----
tbkm <- add_feature_def(
   tbkm,
   "CoReMo sub-module",
   "Structured name of CoReMo sub-module",
   properties=list(
      "module"=list(
         type="character",
         description="Unsigned module name",
         mandatory=TRUE
      ),
      "sub-module"=list(
         type="character",
         description="suffix for the sub-module",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### r2 ** ----
tbkm <- add_feature_def(
   tbkm,
   "r2",
   "Squared correlation value",
   properties=list(
      "value"=list(
         type="numeric",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

### Tables ----

#### CoReMo members ** ----
tbkm <- add_table_def(
   tbkm,
   "CoReMo members",
   description="Members of modules of coregulated genes",
   mandatory=c(
      "module ref", "beid ref"
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### CoReMo modules ** ----
tbkm <- add_table_def(
   tbkm,
   "CoReMo modules",
   description="Description of modules of coregulated genes",
   mandatory=c(
      "name", "r2", "module members", "analysis ref"
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_table_features(
   tbkm,
   "CoReMo modules",
   features=c("description", "CoReMo sub-module")
) %>% 
   as_memoMDB() %>% as_KMR()

#### CoReMo analyses ** ----
tbkm <- add_table_def(
   tbkm,
   "CoReMo analyses",
   description="Description of CoReMo analyses",
   mandatory=c(
      "name", "condition ref", "correlation method", "be list ref"
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_table_features(
   tbkm,
   "CoReMo analyses",
   features=c("dataset ref", "description", "report ref")
) %>% 
   as_memoMDB() %>% as_KMR()


###############################################################################@
## yACRA ----

### Features ----

#### References ----

##### regulator ref ** ----
tbkm <- add_feature_def(
   tbkm,
   "regulator ref",
   "Reference to a BEID in a BE collection",
   properties=list(
      "value"=list(
         type="field",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### Others ----

##### regulator targets ** ----
tbkm <- add_feature_def(
   tbkm,
   "regulator targets",
   "Number of elements targeted by a regulator",
   properties=list(
      "value"=list(
         type="integer",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### regulation impact ** ----
tbkm <- add_feature_def(
   tbkm,
   "regulation impact",
   "Impact of a regulation",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### intersection ** ----
tbkm <- add_feature_def(
   tbkm,
   "intersection",
   "Number of intersecting elements",
   properties=list(
      "value"=list(
         type="integer",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### direct impact ** ----
tbkm <- add_feature_def(
   tbkm,
   "direct impact",
   "Is the impact direct?",
   properties=list(
      "value"=list(
         type="logical",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

### Tables ----

#### yACRA results ** ----
tbkm <- add_table_def(
   tbkm,
   "yACRA results",
   description="Results from yACRA analyses",
   mandatory=c(
      "analysis ref", "module ref", "module members",
      "regulator ref", "regulation impact", "regulator targets",
      "intersection", "p-value", "FDR"
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### yACRA analyses ** ----
tbkm <- add_table_def(
   tbkm,
   "yACRA analyses",
   description="Description of yACRA analyses: causal reasoning analysis",
   mandatory=c(
      "name", "be list ref", "direct impact"
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_table_features(
   tbkm,
   "yACRA analyses",
   features=c("description", "report ref")
) %>% 
   as_memoMDB() %>% as_KMR()




###############################################################################@
## Functional enrichment ----

### Features ----

#### References ----

#### Others ----

##### functional be list ** ----
tbkm <- add_feature_def(
   tbkm,
   "functional be list",
   "Reference to a list of BE sharing a function or any other feature",
   properties=list(
      "value"=list(
         type="character",
         mandatory=TRUE,
         description="The name of the list"
      ),
      "id"=list(
         type="character",
         mandatory=TRUE,
         description="External identifier"
      ),
      "reference"=list(
         type="character",
         mandatory=TRUE,
         description="Reference to an external resource"
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

##### functional members ** ----
tbkm <- add_feature_def(
   tbkm,
   "functional members",
   "Number of elements belonging to a list of BE sharing a funciton or any other feature",
   properties=list(
      "value"=list(
         type="integer",
         mandatory=TRUE
      )
   )
) %>% 
   as_memoMDB() %>% as_KMR()

### Tables ----

#### functional enrichment ** ----
tbkm <- add_table_def(
   tbkm,
   "functional enrichment",
   description="Results of functional enrichment analysis",
   mandatory=c(
      "analysis ref", "module ref", "module members",
      "functional be list", "functional members",
      "intersection", "p-value", "FDR"
   )
) %>% 
   as_memoMDB() %>% as_KMR()

#### FE analyses ** ----
tbkm <- add_table_def(
   tbkm,
   "FE analyses",
   description="Description of functional enrichment analysis",
   mandatory=c(
      "name", "be list ref"
   )
) %>% 
   as_memoMDB() %>% as_KMR()
tbkm <- add_table_features(
   tbkm,
   "FE analyses",
   features=c("description", "report ref")
) %>% 
   as_memoMDB() %>% as_KMR()


###############################################################################@
## Save requirements ----
fp <- here("supp/TBKM-requirements/TBKM")
if(file.exists(fp)){
   file.rename(fp, here(sprintf("supp/TBKM-requirements/%s-TBKM", Sys.time())))
}
ftbkm <- as_fileMDB(tbkm, here("supp/TBKM-requirements/"), htmlModel=FALSE)
