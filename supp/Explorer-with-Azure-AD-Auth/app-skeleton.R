#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
## Setup libraries ----
library(AzureAuth)
library(shiny)
library(shinyjs)
################################################@
### TO EDIT: additional required libraries ----

################################################@

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
## Authorization configuration ----
`_azure_tenant_` <- "237582ad-3eab-4d44-8688-06ca9f2e613b"
`_azure_app_` <- "f55d2b52-9fed-4b05-8b0a-b24cf8149922"
`_azure_resource_` <- c("api://kmt-prd01/.default", "offline_access")
`_azure_redirect_` <- "https://bel038783/shiny/pgodard/UCB-TKCat/"
`_shiny_appTitle_` <- "UCB TKCat"


################################################@
## TO EDIT: global logic ----

################################################@


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
## UI ----

################################################@
### TO EDIT: UI logic ----

## Replace this with your app's regular UI
## keeping the call to shinyjs::useShinyjs()

ui <- fluidPage(
   shinyjs::useShinyjs(),
   verbatimTextOutput("token")
)

################################################@

### Final ui function ----
ui_func <- function(req){
   opts <- shiny::parseQueryString(req$QUERY_STRING)
   if(is.null(opts$code)){
      auth_uri <- build_authorization_uri(
         resource=`_azure_resource_`,
         tenant=`_azure_tenant_`,
         app=`_azure_app_`,
         redirect_uri=`_azure_redirect_`,
         version=2
      )
      redir_js <- sprintf("location.replace(\"%s\");", auth_uri)
      tags$script(HTML(redir_js))
   }
   else{
      ui
   }
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
## Server ----
server <- function(input, output, session)
{
   ### Clean URL after authentication ----
   shinyjs::runjs(sprintf(
      "
      $(document).ready(function(event) {
         const nextURL = '%s';
         const nextTitle = '%s';
         const nextState = { additionalInformation: 'Updated the URL with JS' };
         // This will create a new entry in the browser's history,
         //without reloading
         window.history.pushState(nextState, nextTitle, nextURL);
      });
      ",
      `_azure_redirect_`, `_shiny_appTitle_`
   ))
   opts <- shiny::parseQueryString(shiny::isolate(
      session$clientData$url_search
   ))
   if(is.null(opts$code)){
      return()
   }

   ### Authentication ----
   auth_cred <- AzureAuth::get_azure_token(
      resource=c(`_azure_resource_`, "offline_access"),
      tenant=`_azure_tenant_`,
      app=`_azure_app_`,
      version=2,
      auth_type="authorization_code",
      use_cache = FALSE,
      authorize_args=list(redirect_uri=`_azure_redirect_`),
      auth_code=opts$code
   )

   ################################################@
   ### TO EDIT: Server logic ----

   ## Replace the following lines with your own logic

   output$token <- renderPrint(auth_cred)

   ################################################@
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
## Run the application ----
shinyApp(ui_func, server)
