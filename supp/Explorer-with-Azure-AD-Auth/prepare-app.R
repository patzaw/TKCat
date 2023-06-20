library(here)
AuthHelpers::prepare_shiny_app_with_azure_authorization(
   resource = "api://kmt-prd01/.default",
   tenant = "237582ad-3eab-4d44-8688-06ca9f2e613b",
   app = "f55d2b52-9fed-4b05-8b0a-b24cf8149922",
   redirect = "https://bel038783/shiny/pgodard/UCB-TKCat/",
   appTitle = "UCB TKCat",
   file = here("supp/Explorer-with-Azure-AD-Auth/app-skeleton.R")
)
