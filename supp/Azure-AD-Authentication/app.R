library(shiny)
library(bslib)
library(tidyverse)
library(AzureAuth)
library(TKCat)
library(BED)
library(DT)
library(future)
plan(multisession)

ui <- function(req){
   fluidPage(
      tags$head(
         tags$link(
            rel="stylesheet", type="text/css", href="defChanges.css"
         )
      ),
      title="Test TKCat with AzureAuth",
      fluidRow(
         h2("Test TKCat with AzureAuth"),
         style="margin:5px;"
      ),
      fluidRow(
         uiOutput("bed")
      ),
      fluidRow(
         DTOutput("mdbTable"),
         style="margin:5px;"
      )
   )
}

server <- function(input, output, session){
   
   app_state <- reactiveValues(
      credentials=NULL,
      k=NULL
   )
   
   observe({
      print(names(session$clientData))
      print(session$clientData[["url_hostname"]])
   })
   
   observe({
      credentials <- app_state$credentials
      if(
         is.null(credentials) ||
         !is_azure_token(credentials) ||
         !credentials$validate()
      ){
         creds <- get_device_creds(
            resource = c("api://kmt-prd01/user_impersonation"),
            tenant = "237582ad-3eab-4d44-8688-06ca9f2e613b",
            app = "f55d2b52-9fed-4b05-8b0a-b24cf8149922",
            version = 2
         )
         credentials %<-% get_azure_token(
            resource = c("api://kmt-prd01/user_impersonation"),
            tenant = "237582ad-3eab-4d44-8688-06ca9f2e613b",
            app = "f55d2b52-9fed-4b05-8b0a-b24cf8149922",
            version = 2,
            auth_type="device_code",
            use_cache=FALSE,
            device_creds=creds
         )
         shiny::showModal(shiny::modalDialog(
            tagList(
               fluidRow(
                  column(
                     12,
                     tags$p(
                        "Go to the following URL:",
                        tags$a(
                           href=creds$verification_uri,
                           target="_blank",
                           creds$verification_uri
                        )
                     ),
                     tags$p(
                        "and paste the following code:",
                        tags$strong(creds$user_code)
                     )
                  )
               )
            ),
            size="s",
            easyClose=TRUE,
            footer=NULL
         ))
         app_state$credentials <- credentials
      }else{
         token <- credentials$credentials$access_token
         app_state$k <- chTKCat(
            "tkcat.ucb.com",
            password="",
            port=443, https=TRUE,
            extended_headers=list(
               "Authorization"=paste("Bearer", token)
            )
         )
         connectToBed(
            "https://bed.ucb.com",
            remember=FALSE, useCache=TRUE,
            .opts = list(
               extendedHeaders=list("Authorization"=paste("Bearer", token))
            )
         )
      }
   })
   
   output$mdbTable <- renderDT({
      credentials <- app_state$credentials
      req(credentials$validate())
      k <- app_state$k
      req(is.chTKCat(k))
      list_MDBs(k) %>% 
         datatable(rownames=FALSE)
   })
   
   output$bed <- renderUI({
      k <- app_state$k
      burl <- BED:::bedEnv$graph$url
      req(burl=="https://bed.ucb.com")
      req(checkBedConn())
      tagList(
         beidsUI("be"),
         fluidRow(
            column(
               12,
               tags$br(),
               h3("Selected gene entities"),
               DTOutput("bedResults")
            )
         )
      )
   })
   
   found <- beidsServer("be", toGene=TRUE, multiple=TRUE, tableHeight=250)
   output$bedResults <- renderDT({
      req(found())
      toRet <- found()
      datatable(toRet, rownames=FALSE)
   })
   
}

### Running ----
shinyApp(
   ui=ui,
   server=server
)

