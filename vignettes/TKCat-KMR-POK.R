## ----setup, message=FALSE, echo=FALSE, include=FALSE, cache=FALSE-------------
library(knitr)
opts_chunk$set(
   include=TRUE,
   echo=TRUE,
   message=TRUE,
   warning=TRUE,
   cache=FALSE,
   cache.lazy=FALSE
)
library(TKCat)

## -----------------------------------------------------------------------------
ebkm <- create_KMR(
   name="EBKM",
   title="Experimental and Biological Knowledge Management",
   description="Requirements for integrating knowledge from biological research activities",
   version="0.1",
   maintainer="[Patrice Godard](mailto:patrice.godard@gmail.com)"
)

## ----eval=FALSE---------------------------------------------------------------
#  data_model(ebkm) %>%
#     plot()

## -----------------------------------------------------------------------------
ebkm <- add_feature_def(
   kmr = ebkm,
   name = "name",
   description = "name/identifier of a record",
   properties = list(
      "value" = list(
         type = "character",
         mandatory = TRUE
      )
   )
)
ebkm <- add_feature_def(
   kmr = ebkm,
   name = "description",
   description = "description of a record",
   properties = list(
      "value" = list(
         type = "character",
         mandatory = TRUE
      )
   )
)

## -----------------------------------------------------------------------------
ebkm <- add_table_def(
   kmr = ebkm,
   name = "samples",
   description = "A table listing samples and associated features",
   mandatory_features=c("name", "description")
)

## -----------------------------------------------------------------------------
ebkm <- add_feature_def(
   kmr = ebkm,
   name = "sex",
   description = "Subject sex",
   properties = list(
      "value" = list(
         type = "character",
         mandatory = TRUE
      )
   )
)
ebkm <- add_feature_def(
   kmr = ebkm,
   name = "tissue",
   description = "Name of a biological tissue",
   properties = list(
      "value" = list(
         type = "character",
         mandatory = TRUE
      ),
      "identifier" = list(
         type = "character",
         description = "Identifier in reference database",
         mandatory = TRUE
      ),
      "reference" = list(
         type="character",
         description = "Reference database",
         mandatory=TRUE
      ),
      "side" = list(
         type = "character",
         description = "Sampling side",
         mandatory = FALSE
      ),
      "relative side" = list(
         type="character",
         description = "Sampling relative side",
         mandatory = FALSE
      ),
      "side reference" = list(
         type="character",
         description =
            "Reference for relative side (e.g., Handedness, treatment...)",
         mandatory = FALSE
      )
   )
)
ebkm <- add_table_features(
   kmr = ebkm,
   table = "samples",
   features = c("sex", "tissue")
)

## -----------------------------------------------------------------------------
ebkm <- add_property_values(
   kmr = ebkm, feature = "sex", property = "value",
   values = c("female", "male")
)

## -----------------------------------------------------------------------------
ebkm <- add_property_values(
   kmr = ebkm, feature = "tissue", property = "reference",
   values=c(
      "Uberon" = "https://obophenotype.github.io/uberon/"
   )
)
ebkm <- add_property_values(
   kmr = ebkm, feature = "tissue", property = "side",
   values = c("left", "right")
)
ebkm <- add_property_values(
   kmr = ebkm, feature = "tissue", property = "relative side",
   values = c("ipsilateral", "contralateral")
)

## -----------------------------------------------------------------------------
ebkm <- add_unit_def(
   kmr = ebkm,
   measurement = "duration", unit = "s", description = "seconds"
)
ebkm <- add_unit_def(
   kmr = ebkm,
   measurement = "duration", unit = "min", description = "minutes"
)
ebkm <- add_unit_def(
   kmr = ebkm,
   measurement = "duration", unit = "h", description = "hours"
)
ebkm <- add_unit_def(
   kmr = ebkm,
   measurement = "duration", unit = "d", description = "days"
)
ebkm <- add_unit_def(
   kmr = ebkm,
   measurement = "duration", unit = "m", description = "months"
)
ebkm <- add_unit_def(
   kmr = ebkm,
   measurement = "duration", unit = "y", description = "years"
)
ebkm <- add_unit_def(
   kmr = ebkm,
   measurement = "duration", unit = "w", description = "weeks"
)

##

ebkm <- add_feature_def(
   kmr = ebkm,
   name = "age",
   description = "Elapsed time since birth",
   properties = list(
      "value" = list(
         type = "numeric",
         mandatory = TRUE,
         measurement = "duration"
      )
   )
)
ebkm <- add_table_features(
   kmr = ebkm,
   table = "samples",
   features = c("age")
)

## -----------------------------------------------------------------------------
list_table_types(ebkm)

## -----------------------------------------------------------------------------
list_table_features(ebkm, "samples")

## -----------------------------------------------------------------------------
list_feature_properties(ebkm, "tissue")

## -----------------------------------------------------------------------------
list_property_values(ebkm, "sex", "value")

## -----------------------------------------------------------------------------
list_measurements(ebkm)
list_measurement_units(ebkm, "duration")

## -----------------------------------------------------------------------------
samples <- tibble(
   name = c("S1", "S2", "S3", "S4"),
   description = c(
      "Sample from left hippocampus from patient 1",
      "Sample from left hippocampus from patient 2",
      "Sample from left hippocampus from patient 3",
      "Sample from left hippocampus from patient 4"
   ),
   sex = c("male", "female", "female", "male"),
   age = c(25, 36, 28, 42),
   tissue_name = rep("hippocampus", 4),
   tissue_id = rep("UBERON_0002421", 4),
   tissue_ref = rep("Uberon", 4),
   tissue_side = rep("left", 4),
   seizures = c(31, 64, 12, 29)
)
model <- ReDaMoR::df_to_model(samples)
mdb <- memoMDB(
   list(samples=samples),
   model,
   dbInfo=list(
      name="Test"
   )
)

## -----------------------------------------------------------------------------
mdb <- add_km_spec(mdb, ebkm)
mdb <- add_km_table(
   mdb, ebkm,
   name="samples", type="samples",
   features=list(
      
      ### TBKM mandatory features ###
      list(feature="name", fields="name"),
      list(feature="description", fields="description"),
      
      ### TBKM optional features  ###
      list(feature="age", fields="age", unit="y"),
      list(feature="sex", fields="sex"),
      list(
         feature="tissue",
         fields=list(
            "value"=list(field="tissue_name"),
            "identifier"=list(field="tissue_id"),
            "reference"=list(field="tissue_ref"),
            "side"=list(field="tissue_side")
         )
      )
   )
)

## ----results='asis', echo=FALSE-----------------------------------------------
ec <- readLines("EBKM-helpers.R")
cat('```r', sep="\n")
cat(ec, sep="\n")
cat('```', sep="\n")

## -----------------------------------------------------------------------------
ebkm <- add_helpers(
   ebkm,
   code="EBKM-helpers.R",
   name="R-Helpers",
   language = "R"
)

## -----------------------------------------------------------------------------
ebkm_helpers <- get_R_helpers(ebkm)
ebkm_helpers$help()
ebkm_helpers$help("get_tissue_ref_url")
ebkm_helpers$get_tissue_ref_url(c("UBERON_0002421", "UBERON_0001876"))

## ----results='asis', echo=FALSE-----------------------------------------------
ec <- readLines("MDB-helpers.R")
cat('```r', sep="\n")
cat(ec, sep="\n")
cat('```', sep="\n")

## -----------------------------------------------------------------------------
mdb <- add_helpers(
   mdb,
   kmr = ebkm,
   code="MDB-helpers.R",
   name="R-Helpers",
   language = "R"
)

## -----------------------------------------------------------------------------
mdb_helpers <- get_R_helpers(mdb, kmr = ebkm)
mdb_helpers$help("summarize_seizures")
mdb_helpers$summarize_seizures()

## -----------------------------------------------------------------------------
pok <- create_POK(mdb, ebkm)
pok

## -----------------------------------------------------------------------------
tkcat <- TKCat(Test=mdb, EBKM=ebkm)
get_POK(tkcat, "Test", "EBKM")

