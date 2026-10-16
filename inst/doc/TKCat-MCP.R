## ----setup, message=FALSE, echo=FALSE, include=FALSE, cache=FALSE--------------------------------------------------------------------------
library(knitr)
opts_chunk$set(
   include=TRUE,
   echo=TRUE,
   eval=FALSE,
   message=TRUE,
   warning=TRUE,
   cache=FALSE
)

## ----eval=FALSE----------------------------------------------------------------------------------------------------------------------------
# install.packages("TKCat")
# ## or the development version
# devtools::install_github("patzaw/TKCat")

## ----eval=FALSE----------------------------------------------------------------------------------------------------------------------------
# library(TKCat)
# k <- chTKCat(host = "localhost", port = 9111, user = "admin_user")
# create_chTKCat_user(k, login = "mcp_user", contact = NA, admin = FALSE)
# ## Give access to a private MDB
# add_chMDB_user(k, mdb = "Private_MDB", login = "mcp_user", admin = FALSE)

## ----eval=FALSE----------------------------------------------------------------------------------------------------------------------------
# jsonlite::base64_enc("jdoe:xxxxxx")

