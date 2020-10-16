###############################################################################@
.etkc_add_resources <- function(){
   shiny::addResourcePath(
      "www",
      system.file("www", package=utils::packageName())
   )
   shiny::addResourcePath(
      "doc",
      system.file("doc", package=utils::packageName())
   )
   return(invisible(NULL))
}


###############################################################################@
.etkc_sd_header <- function(){
   return(shinydashboard::dashboardHeader(
      title=shiny::uiOutput("instance"),
      titleWidth="300px",
      shiny::tags$li(
         class = "dropdown",
         shiny::tags$div(
            shiny::uiOutput("status"),
            style=paste(
               "margin-top:0;",
               "margin-right:15px;",
               "-ms-transform:translateY(50%);",
               "transform:translateY(50%);"
            )
         )
      )
   ))
}


###############################################################################@
.etkc_sd_sidebar <- function(sysInterface){
   
   ## Resources ----
   sbelts <- list(
      id="sidebarmenu",
      shinydashboard::menuItem(
         "Resources",
         tabName="resources",
         icon=shiny::icon("list-alt")
      ),
      shinydashboard::menuItem(
         "Data model",
         tabName="model",
         icon=shiny::icon("project-diagram")
      ),
      shinydashboard::menuItem(
         "Search resources",
         tabName="search",
         icon=shiny::icon("search")
      )
   )
   
   ## System ----
   if(sysInterface){
      sbelts <- c(sbelts, list(
         shiny::tags$hr(),
         shinydashboard::menuItem(
            "System",
            tabName="system",
            icon=shiny::icon("info-circle")
         ),
         shinydashboard::menuItem(
            shiny::uiOutput("currentUser"),
            icon=NULL
         ),
         shinydashboard::menuItem(
            shiny::uiOutput("signin"),
            tabName="signinTab",
            icon=NULL
         )
      ))
   }
   
   ## Documentation ----
   sbelts <- c(sbelts, list(
      shiny::tags$hr(),
      shinydashboard::menuItem(
         "Documentation",
         icon=shiny::icon("question-circle"),
         shinydashboard::menuSubItem(
            "clichouse dm",
            href="doc/chTKCat-data-models.html"
         )
      )
   ))
   
   return(shinydashboard::dashboardSidebar(
      ## Logo ----
      div(
         shiny::img(
            src="www/TKCat-small.png",
            height="120px",
            id="mainLogo"
         ),
         style=paste(
            "width:150px;",
            "margin-left:auto; margin-right:auto;",
            "margin-top:15px;",
            "margin-bottom:15px;",
            "text-align:center;"
         )
      ),
      shiny::tags$hr(),
      ## Menu ----
      do.call(shinydashboard::sidebarMenu, sbelts)
   ))
}

###############################################################################@
.etkc_sd_body <- function(sysInterface){
   
   belts <- list(
      
      ## Resources ----
      shinydashboard::tabItem(
         tabName="resources",
         shiny::fluidRow(
            shiny::column(
               6,
               DT::DTOutput("mdbList")
            ),
            shiny::column(
               6,
               shiny::uiOutput("dbInfo")
            )
         )
      ),
      
      # Data model ----
      shinydashboard::tabItem(
         tabName="model",
         shiny::fluidRow(
            shiny::column(
               7,
               visNetwork::visNetworkOutput(
                  "dataModel", height="85vh"
               ),
               style="border:solid; min-height:90vh;"
            ),
            shiny::column(
               5,
               shiny::fluidRow(
                  shiny::uiOutput("collectionInfo"),
                  style="margin-left:3px;"
               ),
               shiny::fluidRow(
                  shiny::uiOutput("tableInfo"),
                  style="margin-left:3px;"
               )
            ),
            style="margin-left:3px;margin-right:3px"
         )
      ),
      
      # Search ----
      shinydashboard::tabItem(
         tabName="search",
         shiny::fluidRow(
            shiny::column(
               10,
               shiny::textInput(
                  inputId="searchInput",
                  label="Search resource, table and field information",
                  placeholder="search value"
               )
            ),
            shiny::column(
               2,
               shiny::uiOutput("searchMessages")
            )
         ),
         shiny::uiOutput("searchResults")
      )
   )
   
   # System information ----
   if(sysInterface){
      belts <- c(belts, list(
         shinydashboard::tabItem(
            tabName="system",
            shiny::fluidRow(
               shiny::uiOutput("systemInfo")
            )
         )
      ))
   }
   
   shinydashboard::dashboardBody(
      ## Page header ----
      shiny::tags$head(
         shiny::tags$link(
            rel="icon",
            href='www/TKCat-small.png'
         ),
         shiny::tags$script(src='www/interactions.js')
      ),
      ## Body ----
      do.call(shinydashboard::tabItems, belts)
      
   )
}



###############################################################################@
.highlightText <- function(text, value){
   value <- sub('^"', '', sub('"$', '', value))
   value <- gsub("[[:punct:]]", ".?", value)
   return(unlist(lapply(
      text,
      function(x){
         if(is.na(x)){
            return(x)
         }
         p <- gregexpr(value, x, ignore.case=TRUE)[[1]]
         if(p[1]>0){
            toRet <- c(substr(x, 0, p[1]-1))
            for(i in 1:length(p)){
               toRet <- c(
                  toRet,
                  '<mark style="background-color:yellow;font-weight:bold;">',
                  substr(x, p[i], p[i]+attr(p, "match.length")[i]-1),
                  '</mark>',
                  substr(
                     x,
                     p[i]+attr(p, "match.length")[i],
                     min(
                        p[i+1]-1,
                        nchar(x)+1,
                        na.rm=TRUE
                     )
                  )
               )
            }
            toRet <- paste(toRet, collapse="")
         }else{
            toRet <- x
         }
         return(toRet)
      }
   )))
}
