library(TKCat)
library(AuthHelpers)
library(shiny)
library(shinydashboard)
library(DT)
library(future)
## Set future plan to allow several users to log in simultaneously
future::plan(future::multisession, workers=4)

###############################################################################@
## Global ----
shiny::addResourcePath(
   "www",
   system.file("www", package = "TKCat")
)
subSetSize <- 100
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

###############################################################################@
## UI ----
ui <- function(req) {
   shinydashboard::dashboardPage(
      skin="yellow",
      title = "UCB TKCat",
      
      ########################@
      ## Dashboard header ----
      ## Uses output$instance and output$status
      header = shinydashboard::dashboardHeader(
         title = "UCB TKCat",
         titleWidth = "300px",
         shiny::tags$li(
            class = "dropdown",
            shiny::tags$div(
               shiny::uiOutput("status", inline=TRUE),
               shiny::HTML("&nbsp;&nbsp;|&nbsp;&nbsp;"),
               shiny::actionLink(
                  "refresh", "",
                  icon=shiny::icon("arrows-rotate", verify_fa = FALSE),
                  style="color:black;",
                  title="Refresh resource table"
               ),
               style = paste(
                  "margin-top:0;",
                  "margin-right:15px;",
                  "-ms-transform:translateY(50%);",
                  "transform:translateY(50%);"
               )
            )
         )
      ),
      
      ########################@
      ## Sidebar ----
      ## Uses uiOutput("currentUser") and uiOutput("signin")
      sidebar = shinydashboard::dashboardSidebar(
         ### Logo ----
         HTML('
           <div style="width:150px; margin-left:auto; margin-right:auto; margin-top:15px; margin-bottom:15px; text-align:center;">
                <img src="www/TKCat-small.png" height="120px" id="mainLogo"/>
              </a>
            </div>
         '),
         ### Menu ----
         shinydashboard::sidebarMenu(
            #### Resources ----
            id = "sidebarmenu",
            style = "white-space:normal;",
            shinydashboard::menuItem(
               shiny::uiOutput("currentUser"),
               icon=NULL
            ),
            shiny::tags$hr(),
            shinydashboard::menuItem(
               "Resources",
               tabName = "resources",
               icon = shiny::icon("list-alt", verify_fa = FALSE)
            ),
            shinydashboard::menuItem(
               "Data model",
               tabName = "model",
               icon = shiny::icon("project-diagram", verify_fa = FALSE)
            ),
            shinydashboard::menuItem(
               "Search resources",
               tabName = "search",
               icon = shiny::icon("search", verify_fa = FALSE)
            ),
            
            #### Authentication in R ----
            shiny::tags$hr(),
            shinydashboard::menuItem(
               "Authentication from R",
               tabName = "authInR",
               icon = shiny::icon("info-circle", verify_fa = FALSE)
            ),
            
            #### Documentation ----
            shinydashboard::menuItem(
               text = "R package",
               icon = shiny::icon("question-circle", verify_fa = FALSE),
               href = "https://patzaw.github.io/TKCat/"
            ),
            
            #### Contact ----
            shinydashboard::menuItem(
               text = "Contact admin",
               icon = shiny::icon("envelope", verify_fa = FALSE),
               href = shiny::HTML(
                  "mailto:Patrice Godard <patrice.godard@ucb.com>?Subject=UCB-TKCat"
               ),
               newtab = FALSE
            )
         )
      ),
      
      ########################@
      ## Body ----
      body = shinydashboard::dashboardBody(
         ### Page header ----
         shiny::tags$head(
            shiny::tags$style(shiny::HTML(
               'table.dataTable tr.selected td a {background-color: white !important;}'
            )),
            shiny::tags$link(
               rel = "icon",
               href = 'www/TKCat-small.png'
            ),
            shiny::tags$script(src = 'www/interactions.js')
         ),
         
         ### Body ----
         shinydashboard::tabItems(
            
            #### Resources ----
            shinydashboard::tabItem(
               tabName="resources",
               shiny::fluidRow(
                  shiny::column(
                     9,
                     DT::DTOutput("mdbList")
                  ),
                  shiny::column(
                     3,
                     shiny::uiOutput("dbInfo")
                  )
               )
            ),
            
            #### Data model ----
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
            
            #### Search ----
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
            ),
            
            #### System information ----
            shinydashboard::tabItem(
               tabName="authInR",
               shiny::fluidRow(shiny::column(
                  6,
                  shiny::markdown(
                     paste(
                        readLines("supp/authentication-in-R.Rmd"),
                        collapse = "\n"
                     )
                  )
               ))
            )
         )
      )
   )
}

###############################################################################@
## Server ----
server <- function(input, output, session) {
   
   ## Authentication ----
   auth_cred <- shiny_azure_auth(
      id="authentication",
      resource=c("api://kmt-prd01/user_impersonation"),
      tenant="237582ad-3eab-4d44-8688-06ca9f2e613b",
      app="f55d2b52-9fed-4b05-8b0a-b24cf8149922",
      timeout=60
   )
   
   ## TKCat ----
   tkcatState <- shiny::reactiveValues(
      user=NULL
   )
   get_tkcat <- function(){
      device_credentials <- isolate(auth_cred())
      if(!device_credentials$is_valid()){
         device_credentials$refresh_token()
         auth_cred(device_credentials)
      }
      token <- device_credentials$get_access_token()
      chTKCat(
         "tkcat.ucb.com",
         password="",
         port=443, https=TRUE,
         extended_headers=list(
            "Authorization"=paste("Bearer", token)
         )
      )
   }
   observe({
      device_credentials <- auth_cred()
      req(device_credentials)
      token <- device_credentials$get_access_token()
      tkcon <- get_tkcat()
      tkcatState$user <- tkcon$chcon@user
   })
   
   ## User ----
   output$currentUser <- shiny::renderUI({
      req(tkcatState$user)
      shiny::actionLink(
         inputId="currentUser",
         label=shiny::span(
            tkcatState$user
         ),
         icon=shiny::icon(
            "user",
            verify_fa = FALSE
         ),
         style="margin:0;",
         title="Current user"
      )
   })
   
   ########################@
   ## Selection status ----
   selStatus <- shiny::reactiveValues(
      resource=NULL,
      mdb=NULL,
      access=NULL,
      tables=NULL,
      size=NULL
   )
   output$status <- shiny::renderUI({
      mdb <- selStatus$mdb
      shiny::req(mdb)
      if(is.MDB(mdb)){
         dbi <- db_info(mdb)
      }else{
         dbi <- mdb$dbInfo
      }
      shiny::tags$span(
         "Selected resource:",
         shiny::tags$strong(dbi$name),
         sprintf("(%s)", dbi$title)
      )
   })
   
   ########################@
   ## MDB list ----
   mdbs <- shiny::reactiveValues(
      list=NULL,
      collections=NULL,
      validInput=FALSE
   )
   shiny::observe({
      req(auth_cred())
      input$refresh
      shiny::withProgress(
         message="Getting list of MDBs",
         expr={
            tkcon <- get_tkcat()
            mdbs$list <- list_MDBs(tkcon) %>% 
               dplyr::filter(.data$populated)
            mdbs$collections <- collection_members(tkcon)
         }
      )
   })
   output$mdbList <- DT::renderDT({
      shiny::req(mdbs$list)
      colToTake <- intersect(
         c("name", "title", "access", "maintainer", "timestamp"),
         colnames(mdbs$list)
      )
      toShow <- mdbs$list %>%
         dplyr::select(dplyr::all_of(colToTake)) %>%
         dplyr::rename("Resource"="name") %>% 
         dplyr::rename_all(function(x){
            paste0(
               toupper(substr(x, 1, 1)),
               substr(x, 2, nchar(x))
            )
         })
      if(nrow(mdbs$collections)>0){
         cm <- mdbs$collections %>%
            dplyr::select("collection", "resource") %>%
            dplyr::distinct() %>%
            dplyr::group_by(.data$resource) %>% 
            dplyr::summarize(
               collection=paste(.data$collection, collapse=", ")
            ) %>%
            dplyr::ungroup() %>%
            dplyr::rename("Collections"="collection")
         toShow <- dplyr::left_join(toShow, cm, by=c("Resource"="resource"))
      }else{
         toShow <- dplyr::mutate(toShow, Collection=NA)
      }
      mdbs$validInput <- TRUE
      toRet <- DT::datatable(
         dplyr::mutate(
            toShow,
            Title=unlist(lapply(
               .data$Title,
               shiny::markdown
            )),
            Maintainer=unlist(lapply(
               .data$Maintainer,
               shiny::markdown
            ))
         ),
         rownames=FALSE,
         filter="top",
         escape=FALSE,
         selection = list(
            mode="single",
            selected=which(
               toShow$Resource==shiny::isolate(selStatus$resource)
            )
         ),
         options = list(
            pageLength=10,
            dom=c("ltip"),
            order=list(list(0, 'asc'))
         )
      )
      if("Access" %in% colnames(toShow)){
         toRet <- toRet %>% 
            DT::formatStyle(
               1:ncol(toShow), valueColumns="Access",
               target="row",
               color=DT::styleEqual("none", "red", default="black"),
               'font-style'=DT::styleEqual(
                  "none", "italic", default="normal"
               )
            )
      }
      toRet
   })
   mdbListProxy <- DT::dataTableProxy("mdbList")
   
   shiny::observe({
      shiny::req(mdbs$validInput)
      mdbl <- mdbs$list
      s <- input$mdbList_rows_selected
      n <- mdbl$name[s]
      if(length(n)==0 || n==""){
         selStatus$resource <- NULL
         selStatus$mdb <- NULL
         selStatus$access <- NULL
         selStatus$size <- NULL
      }else{
         selStatus$resource <- n
      }
   })
   shiny::observe({
      n <- selStatus$resource
      shiny::req(n)
      mdbl <- shiny::isolate(mdbs$list)
      if("access" %in% colnames(mdbl)){
         access <- dplyr::filter(mdbl, .data$name==!!n) %>% 
            dplyr::pull("access")
      }else{
         access <- NULL
      }
      selStatus$access <- access
      selStatus$size <- NULL
      shiny::withProgress(
         message=sprintf("Getting %s metadata", n),
         expr={
            tkcon <- get_tkcat()
            if(!is.null(access) && access=="none"){
               mdb <- try(get_chMDB_metadata(tkcon, n))
            }else{
               mdb <- try(get_MDB(tkcon, n, check=FALSE), silent=TRUE)
            }
            selStatus$mdb <- mdb
            if(is.MDB(mdb)){
               m <- data_model(mdb)
            }
            if(is.list(mdb) && "dataModel" %in% names(mdb)){
               m <- mdb$dataModel
            }
         }
      )
      if(
         inherits(mdb, "try-error") ||
         !all(shiny::isolate(selStatus$tables) %in% names(m))
      ){
         selStatus$tables <- NULL
      }
   })
   shiny::observe({
      mdb <- selStatus$mdb
      shiny::req(mdb)
      if(is.MDB(mdb)){
         m <- data_model(mdb)
      }
      if(is.list(mdb) && "dataModel" %in% names(mdb)){
         m <- mdb$dataModel
      }
      tn <- shiny::isolate(selStatus$tables)
      if(inherits(mdb, "try-error") || !all(tn %in% names(m))){
         selStatus$tables <- NULL
      }
   })
   
   ########################@
   ## DB information ----
   
   output$dbInfo <- shiny::renderUI({
      mdb <- selStatus$mdb
      shiny::req(!is.null(mdb))
      access <- shiny::isolate(selStatus$access)
      tkcon <- get_tkcat()
      if(inherits(mdb, "try-error")){
         n <- shiny::isolate(selStatus$resource)
         shiny::tagList(
            shiny::tags$p(
               attr(mdb, "condition")$message,
               style="color:red;"
            )
         )
      }else{
         if(is.MDB(mdb)){
            dbi <- db_info(mdb)
            dm <- data_model(mdb)
            if(is.chMDB(mdb)){
               ts <- get_chMDB_timestamps(unclass(mdb)$tkcon, dbi$name)
               if(!is.null(ts)){
                  dbi$"other instances"=nrow(ts)-1
               }
            }
         }else{
            dbi <- mdb$dbInfo
            dm <- mdb$dataModel
            ts <- get_chMDB_timestamps(tkcon, dbi$name)
            if(!is.null(ts)){
               dbi$"other instances"=nrow(ts)-1
            }
         }
         dbi$"Number of tables" <- length(dm)
         dbi$"Number of fields" <- lapply(dm, function(x) nrow(x$fields)) %>% 
            unlist() %>% 
            sum()
         dbi$access <- access
         shiny::tagList(
            shiny::h3(dbi$name),
            do.call(shiny::tags$ul, lapply(
               setdiff(names(dbi), c("name")),
               function(n){
                  if(!is.na(dbi[[n]]) && as.character(dbi[[n]])!=""){
                     if(n=="url"){
                        vt <- shiny::tags$a(
                           shiny::HTML(dbi[[n]]),
                           href=dbi[[n]], target="_blank"
                        ) %>% as.character()
                     }else if(n %in% c("maintainer", "description")){
                        vt <- markdown::renderMarkdown(text=dbi[[n]])
                        vt <- gsub("<[/]?p>", "", as.character(vt))
                     }else if(n=="access"){
                        vt <- as.character(shiny::tags$span(
                           dbi[[n]],
                           style=sprintf(
                              "color:%s;",
                              ifelse(dbi[[n]]=="none", "red", "blue")
                           )
                        ))
                     }else if(is.numeric(dbi[[n]])){
                        vt <- format(dbi[[n]], big.mark=",")
                     }else{
                        vt <- as.character(dbi[[n]])
                     }
                     return(shiny::tags$li(
                        shiny::strong(n), ":",
                        shiny::HTML(vt)
                     ))
                  }
               }
            )),
            shiny::uiOutput("dbSize")
         )
      }
   })
   output$dbSize <- shiny::renderUI({
      size <- selStatus$size
      if(is.null(size)){
         shiny::actionButton(
            inputId = "mdbSize", label = "Get the number of records"
         )
      }else{
         shiny::tags$ul(shiny::tags$li(
            shiny::strong("Number of records"), ":",
            shiny::HTML(format(size, big.mark=","))
         ))
      }
   })
   
   shiny::observeEvent(input$mdbSize, {
      mdb <- selStatus$mdb
      shiny::req(!is.null(mdb))
      req(is.MDB(mdb))
      shiny::withProgress(
         message = sprintf(
            "Getting the number of records in %s",
            db_info(mdb)$name
         ),
         expr={
            selStatus$size <- sum(count_records(mdb))
         }
      )
   })
   
   ########################@
   ## Data model ----
   shiny::observe({
      mdb <- selStatus$mdb
      if(is.null(mdb) || inherits(mdb, "try-error")){
         session$sendCustomMessage('hideNavs', 'model')
      }else{
         n <- shiny::isolate(selStatus$resource)
         session$sendCustomMessage('showNavs', 'model')
         session$sendCustomMessage(
            'setTabLabel',
            list(
               name="model",
               label=shiny::HTML(sprintf(
                  paste(
                     '<i class=" fa fa-project-diagram"></i>',
                     'Data model of %s'
                  ),
                  n
               ))
            )
         )
      }
   })
   
   dbdm <- shiny::reactiveValues(
      model=NULL,
      collections=NULL,
      validInput=FALSE
   )
   shiny::observe({
      mdb <- selStatus$mdb
      if(is.MDB(mdb)){
         dbdm$model <- data_model(mdb)
         dbdm$collections <- collection_members(mdb)
      }else{
         if(is.list(mdb) && "dataModel" %in% names(mdb)){
            dbdm$model <- mdb$dataModel
            dbdm$collections <- mdb$collectionMembers
         }else{
            dbdm$model <- NULL
            dbdm$collections <- NULL
            dbdm$validInput <- FALSE
         }
      }
   })
   
   output$dataModel <- visNetwork::renderVisNetwork({
      mdb <- selStatus$mdb
      shiny::req(mdb)
      if(is.MDB(mdb)){
         dm <- data_model(mdb)
      }else{
         dm <- mdb$dataModel
      }
      dbdm$validInput <- TRUE
      nodesIdSelection <- list(enabled=TRUE, useLabels=FALSE)
      sel <- shiny::isolate(selStatus$tables) %>% 
         intersect(names(dm))
      if(length(sel)>0){
         nodesIdSelection$selected <- sel
      }
      plot(dm) %>%
         visNetwork::visOptions(
            nodesIdSelection=nodesIdSelection,
            height="100%"
         ) 
   })
   
   shiny::observe({
      shiny::req(dbdm$validInput)
      n <- input$dataModel_selected
      mdb <- shiny::isolate(selStatus$mdb)
      shiny::req(mdb)
      if(is.MDB(mdb)){
         m <- data_model(mdb)
      }
      if(is.list(mdb) && "dataModel" %in% names(mdb)){
         m <- mdb$dataModel
      }
      if(length(n)==0 || n=="" || !all(n %in% names(m))){
         selStatus$tables <- NULL
      }else{
         selStatus$tables <- n
      }
   })
   
   shiny::observe({
      selTables <- selStatus$tables
      visNetwork::visNetworkProxy("dataModel") %>%
         visNetwork::visSelectNodes(selTables)
   })
   
   ########################@
   ## Collections ----
   output$collectionInfo <- shiny::renderUI({
      mdb <- selStatus$mdb
      shiny::req(mdb)
      list(
         shiny::h3("Collection members"),
         DT::DTOutput("colMembers")
      )
   })
   output$colMembers <- DT::renderDT({
      cm <- dbdm$collections
      shiny::req(cm)
      cm %>%
         dplyr::select(
            "collection", "id"="mid",
            "table", "field",
            "static", "value", "type"
         ) %>%
         DT::datatable(
            rownames=FALSE,
            selection = 'single',
            extensions='Scroller',
            options = list(
               deferRender = TRUE,
               scrollX=TRUE,
               scrollY = 250,
               scroller = TRUE,
               order=list(list(0, 'asc'), list(1, 'asc')),
               dom=c("ti")
            )
         )
   })
   shiny::observe({
      cs <- input$colMembers_rows_selected
      shiny::req(cs)
      cm <- shiny::isolate(dbdm$collections)
      shiny::req(cm)
      cmt <- cm %>% 
         dplyr::slice(cs) %>%
         dplyr::pull(table)
      visNetwork::visNetworkProxy("dataModel") %>%
         visNetwork::visSelectNodes(id=cmt)
   })
   
   ########################@
   ## Table information ----
   
   output$tableInfo <- shiny::renderUI({
      mdb <- selStatus$mdb
      shiny::req(mdb)
      if(!is.MDB(mdb)){
         return(shiny::tagList(
            shiny::tags$strong(
               "You don't have access to these data",
               style='color:red;'
            )
         ))
      }
      sel <- selStatus$tables %>%
         intersect(names(mdb))
      shiny::req(sel)
      shiny::req(length(sel)==1)
      tss <- tabSubSet()
      nr <- ifelse(attr(tss, "mat"), nrow(tss)*ncol(tss), nrow(tss))
      shiny::tagList(
         shiny::h3(sel),
         shiny::tags$ul(
            if(is.fileMDB(mdb)){
               shiny::tags$li(
                  shiny::tags$strong("File size"),
                  ":",
                  data_file_size(mdb) %>% 
                     dplyr::filter(table==!!sel) %>% 
                     dplyr::pull("size") %>% 
                     .format_file_size(),
                  sprintf("(showing %s records)", nr)
               )
            }else{
               if(!is.metaMDB(mdb)){
                  shiny::tags$li(
                     shiny::tags$strong("Number of records"),
                     ":",
                     count_records(mdb, dplyr::all_of(sel)) %>%
                        format(big.mark=","),
                     sprintf("(showing %s records)", nr)
                  )
               }
            }
         ),
         DT::DTOutput("dataSample"),
         shiny::uiOutput("b64Download"),
         shiny::uiOutput("tableDownload")
      )
   })
   
   tabSubSet <- shiny::reactiveVal(NULL)
   shiny::observe({
      tabSubSet(NULL)
      mdb <- selStatus$mdb
      shiny::req(mdb)
      sel <- selStatus$tables %>% 
         intersect(names(mdb))
      shiny::req(sel)
      shiny::req(length(sel)==1)
      tabMod <- data_model(mdb)[[sel]]
      fields <- tabMod$fields$name
      b64_fields <- fields[which(tabMod$fields$type=="base64")]
      if(length(b64_fields)>0){
         toShow <- heads(mdb, dplyr::all_of(sel), n=1)[[1]]
         toTake <- ceiling((2^23)/object.size(toShow))
         if(toTake > 1){
            toShow <- heads(mdb, dplyr::all_of(sel), n=toTake)[[1]]
         }
         tmp <- dplyr::select(
            toShow,
            dplyr::all_of(setdiff(
               colnames(toShow),
               tabMod$fields$name[which(tabMod$fields$type=="base64")]
            ))
         )
      }else{
         toShow <- tmp <- heads(mdb, dplyr::all_of(sel), n=subSetSize)[[1]]
      }
      attr(toShow, "mat") <- ReDaMoR::is.MatrixModel(tabMod)
      if(utils::object.size(tmp) > 2^19){
         toShow <- toShow[
            1:max(c(
               1, ceiling(nrow(toShow)*(2^19/utils::object.size(tmp)))
            )),
         ]
      }
      tabSubSet(toShow)
   })
   output$dataSample <- DT::renderDT({
      toShow <- tabSubSet()
      shiny::req(toShow)
      if(!attr(toShow, "mat")){
         mdb <- shiny::isolate(selStatus$mdb)
         shiny::req(mdb)
         sel <- shiny::isolate(selStatus$tables) %>% 
            intersect(names(mdb))
         shiny::req(sel)
         shiny::req(length(sel)==1)
         tabMod <- data_model(mdb)[[sel]]
         fields <- tabMod$fields$name
         ##
         b64_fields <- fields[which(tabMod$fields$type=="base64")]
         for(b64f in b64_fields){
            toShow[[b64f]] <- paste0(
               '<div width="100%" height="100%" style="color:blue;"',
               ' onmouseover="this.style.color=', "'orange'", ';"',
               ' onmouseout="this.style.color=',"'blue'", ';">',
               "file",
               '</div>'
            )
         }
         toShow <- toShow[, fields]
         ##
         char_fields <- fields[which(tabMod$fields$type=="character")]
         for(charf in char_fields){
            toShow[[charf]] <- ifelse(
               nchar(toShow[[charf]]) > 100,
               sprintf(
                  '<span title="%s" style="%s">%s...</span>',
                  htmltools::htmlEscape(gsub(
                     '"', '&quot;', toShow[[charf]]
                  )),
                  'border-bottom: 1px dashed;',
                  htmltools::htmlEscape(substr(toShow[[charf]], 1, 90))
               ),
               toShow[[charf]]
            )
         }
         ##
         toRet <- DT::datatable(
            toShow,
            rownames=attr(toShow, "mat"),
            selection=if(length(b64_fields)>0){
               list(
                  mode='single', target='cell'
               )
            }else{
               "none"
            },
            extensions='Scroller',
            escape=FALSE,
            options = list(
               deferRender = TRUE,
               scrollX=TRUE,
               scrollY = 250,
               scroller = TRUE,
               dom=c("ti")
            )
         )
         if(length(b64_fields)>0){
            toRet <- toRet %>% 
               DT::formatStyle(
                  columns=b64_fields,
                  "font-style"="italic"
               )
         }
      }else{
         toRet <- DT::datatable(
            as.matrix(toShow),
            rownames=TRUE,
            selection="none",
            extensions='Scroller',
            escape=FALSE,
            options = list(
               deferRender = TRUE,
               scrollX=TRUE,
               scrollY = 250,
               scroller = TRUE,
               dom=c("ti")
            )
         )
      }
      return(toRet)
   })
   
   output$b64Download <- shiny::renderUI({
      tabss <- tabSubSet()
      shiny::req(tabss)
      cs <- input$dataSample_cells_selected
      shiny::req(cs)
      shiny::req(!attr(tabss, "mat"))
      mdb <- shiny::isolate(selStatus$mdb)
      shiny::req(mdb)
      sel <- shiny::isolate(selStatus$tables) %>%
         intersect(names(mdb))
      shiny::req(sel)
      tabMod <- data_model(mdb)[[sel]]
      b64_fields <- tabMod$fields %>%
         dplyr::filter(.data$type=="base64") %>%
         dplyr::pull("name")
      cn <- intersect(colnames(tabss)[cs[2]+1], b64_fields)
      if(length(cn)==1)
         return(shiny::tagList(
            shiny::br(),
            shiny::downloadButton(
               "downloadB64",
               paste("Download", cn)
            ) 
         ))
   })
   
   output$downloadB64 <- shiny::downloadHandler(
      filename = function() {
         tabss <- tabSubSet()
         cs <- input$dataSample_cells_selected
         mdb <- selStatus$mdb
         sel <- selStatus$tables %>%
            intersect(names(mdb))
         f <- colnames(tabss)[cs[2]+1]
         fd <- data_model(mdb)[[sel]]$fields %>%
            dplyr::filter(.data$name==!!f) %>%
            dplyr::pull("comment")
         extm <- regexpr("^ *[{][.]?[[:alnum:]]+[}]", fd)
         if(extm==-1){
            ext <- ""
         }else{
            ext <- substr(fd, extm, extm+attr(extm, "match.length")-1) 
            ext <- sub("[}]", "", sub("^ *[{][.]?", "", ext))
            ext <- paste0(".", ext)
         }
         paste(f, ext, sep="")
      },
      content = function(file) {
         tabss <- tabSubSet()
         cs <- input$dataSample_cells_selected
         d <- tabss[cs[1], cs[2]+1, drop=TRUE]
         writeBin(base64enc::base64decode(d), file)
      }
   )
   
   
   ########################@
   ## Search ----
   output$searchResults <- shiny::renderUI({
      shiny::tagList(
         shiny::fluidRow(
            shiny::column(
               12,
               shiny::uiOutput("searchResources")
            )
         ),
         shiny::fluidRow(
            shiny::column(
               12,
               shiny::uiOutput("searchTables")
            )
         ),
         shiny::fluidRow(
            shiny::column(
               12,
               shiny::uiOutput("searchFields")
            )
         )
      )
   })
   output$searchMessages <- shiny::renderUI({
      st <- input$searchInput
      shiny::req(st)
      shiny::req(c(
         input$searchResRes_rows_selected,
         input$searchTabRes_rows_selected,
         input$searchFieldRes_rows_selected
      ))
      mdb <- selStatus$mdb
      shiny::req(!is.null(mdb))
      if(inherits(mdb, "try-error")){
         n <- shiny::isolate(selStatus$resource)
         shiny::tagList(
            shiny::tags$p(
               "You don't have access to",
               shiny::strong(n),
               '(You can sign in with different credentials)',
               style="color:red;"
            )
         )
      }else{
         NULL
      }
   })
   ##
   searchRes <- shiny::reactiveValues(
      resources=NULL,
      tables=NULL,
      fields=NULL
   )
   ## _+ resources ----
   shiny::observe({
      mdbs <- mdbs$list
      shiny::req(mdbs)
      st <- input$searchInput
      shiny::req(st)
      mdbs <- mdbs %>% 
         dplyr::select("name", "title", "description", "maintainer")
      toTake <- unlist(apply(
         mdbs, 2,
         function(x){
            grep(st, x, ignore.case=T, value=FALSE)
         }
      ))
      toTake <- unique(c(0, toTake))
      toRet <- mdbs %>% dplyr::slice(toTake)
      if(nrow(toRet)>0){
         searchRes$resources <- toRet
      }else{
         searchRes$resources <- NULL
      }
   })
   output$searchResources <- shiny::renderUI({
      st <- input$searchInput
      shiny::req(st)
      shiny::tagList(
         shiny::h3("Resources"),
         DT::DTOutput("searchResRes")
      )
   })
   output$searchResRes <- DT::renderDT({
      st <- input$searchInput
      shiny::req(st)
      toRet <- searchRes$resources
      shiny::req(toRet)
      if(nrow(toRet)>0){
         toRet %>% 
            dplyr::mutate(
               name=.highlightText(.data$name, st),
               title=.highlightText(.data$title, st),
               description=.highlightText(.data$description, st),
               maintainer=.highlightText(.data$maintainer, st)
            ) %>% 
            DT::datatable(
               rownames=FALSE,
               escape=FALSE,
               selection="single",
               options=list(
                  pageLength=5,
                  dom="tip"
               )
            )
      }else{
         NULL
      }
   })
   shiny::observe({
      sel <- input$searchResRes_rows_selected
      shiny::req(sel)
      rt <- shiny::isolate(searchRes$resources)
      shiny::req(rt)
      mdbListProxy %>%
         DT::selectRows(
            which(
               shiny::isolate(mdbs$list$name) %in%
                  dplyr::pull(dplyr::slice(rt, sel), "name")
            )
         )
   })
   ## _+ tables ----
   shiny::observe({
      mdbs <- mdbs$list
      shiny::req(mdbs)
      st <- input$searchInput
      shiny::req(st)
      tkcon <- get_tkcat()
      toRet <- search_MDB_tables(tkcon, st)
      if(nrow(toRet)>0){
         searchRes$tables <- toRet
      }else{
         searchRes$tables <- NULL
      }
   })
   output$searchTables <- shiny::renderUI({
      st <- input$searchInput
      shiny::req(st)
      shiny::tagList(
         shiny::h3("Tables"),
         DT::DTOutput("searchTabRes")
      )
   })
   output$searchTabRes <- DT::renderDT({
      st <- input$searchInput
      shiny::req(st)
      toRet <- searchRes$tables
      shiny::req(toRet)
      if(nrow(toRet)>0){
         toRet %>%
            dplyr::mutate(
               # resource=.highlightText(.data$resource, st),
               name=.highlightText(.data$name, st),
               comment=.highlightText(.data$comment, st)
            ) %>%
            DT::datatable(
               rownames=FALSE,
               escape=FALSE,
               selection="single",
               filter="top",
               options=list(
                  pageLength=5,
                  dom="tip"
               )
            )
      }else{
         NULL
      }
   })
   shiny::observe({
      sel <- input$searchTabRes_rows_selected
      shiny::req(sel)
      rt <- shiny::isolate(searchRes$tables)
      shiny::req(rt)
      mdbListProxy %>%
         DT::selectRows(
            which(
               shiny::isolate(mdbs$list$name) %in%
                  dplyr::pull(dplyr::slice(rt, sel), "resource")
            )
         )
      selStatus$tables <- rt %>% dplyr::slice(sel) %>% dplyr::pull("name")
   })
   ## _+ fields ----
   shiny::observe({
      mdbs <- mdbs$list
      shiny::req(mdbs)
      st <- input$searchInput
      shiny::req(st)
      tkcon <- get_tkcat()
      toRet <- search_MDB_fields(tkcon, st)
      if(nrow(toRet)>0){
         searchRes$fields <- toRet
      }else{
         searchRes$fields <- NULL
      }
   })
   output$searchFields <- shiny::renderUI({
      st <- input$searchInput
      shiny::req(st)
      shiny::tagList(
         shiny::h3("Fields"),
         DT::DTOutput("searchFieldRes")
      )
   })
   output$searchFieldRes <- DT::renderDT({
      st <- input$searchInput
      shiny::req(st)
      toRet <- searchRes$fields
      shiny::req(toRet)
      if(nrow(toRet)>0){
         toRet %>%
            dplyr::mutate(
               name=.highlightText(.data$name, st),
               comment=.highlightText(.data$comment, st)
            ) %>%
            DT::datatable(
               rownames=FALSE,
               escape=FALSE,
               selection="single",
               filter="top",
               options=list(
                  pageLength=5,
                  dom="tip"
               )
            )
      }else{
         NULL
      }
   })
   shiny::observe({
      sel <- input$searchFieldRes_rows_selected
      shiny::req(sel)
      rt <- shiny::isolate(searchRes$fields)
      shiny::req(rt)
      mdbListProxy %>%
         DT::selectRows(
            which(
               shiny::isolate(mdbs$list$name) %in%
                  dplyr::pull(dplyr::slice(rt, sel), "resource")
            )
         )
      selStatus$tables <- rt %>% dplyr::slice(sel) %>% dplyr::pull("table")
   })
   
}

###############################################################################@
## Run the application ----
shiny::shinyApp(
   ui = ui,
   server = server
)
