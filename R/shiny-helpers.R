###############################################################################@
## UI Helpers ####
###############################################################################@


###############################################################################@
.etkc_add_resources <- function(ddir=NULL){
   pckn <- utils::packageName()
   shiny::addResourcePath(
      "www",
      system.file("www", package=pckn)
   )
   shiny::addResourcePath(
      "doc",
      system.file("doc", package=pckn)
   )
   if(!is.null(ddir)){
      shiny::addResourcePath(
         "data",
         ddir
      )
   }
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
.etkc_sd_sidebar <- function(
   sysInterface,
   manList
){
   
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
   if(length(manList)>0){
      mit <- list()
      for(i in 1:length(manList)){
         mit <- c(mit, list(
            shinydashboard::menuSubItem(
               names(manList)[i],
               href=as.character(manList)[i]
            )
         ))
      }
      sbelts <- c(sbelts, list(
         shiny::tags$hr(),
         do.call(shinydashboard::menuItem, c(
            list(
               text="Documentation",
               icon=shiny::icon("question-circle")
            ),
            mit
         ))
      ))
   }
   
   return(shinydashboard::dashboardSidebar(
      ## Logo ----
      div(
         shiny::a(
            shiny::img(
               src="www/TKCat-small.png",
               height="120px",
               id="mainLogo"
            ),
            href="https://github.com/patzaw/TKCat",
            target="_blank"
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
## Server Helpers ####
###############################################################################@


###############################################################################@
.build_etkc_server_default <- function(
   x,
   subSetSize=100,
   xparams=list(),
   ddir=NULL
){
   
   function(input, output, session) {
      
      if(is.TKCat(x)){
         instance <- shiny::reactiveValues(
            tkcat=x
         )
         output$instance <- shiny::renderUI({
            "Local TKCat"
         })
      }
      
      if(is.chTKCat(x)){
         ########################@
         ## TKCat instance ----
         instance <- shiny::reactiveValues(
            tkcat=db_reconnect(x, user="default"),
            valid=DBI::dbIsValid(x$chcon)
         )
         shiny::observe({
            if(!instance$valid){
               instance$tkcat <- db_reconnect(
                  shiny::isolate(instance$tkcat), user="default"
               )
               instance$valid <- TRUE
            }
         })
         output$instance <- shiny::renderUI({
            paste("chTKCat :", instance$tkcat$instance)
         })
         shiny::observe({
            if(instance$tkcat$chcon@user=="default"){
               sc <- "blue"
            }else{
               sc <- "yellow"
            }
            session$sendCustomMessage(
               "change_skin",
               paste0("skin-", sc)
            )
         })
         shiny::onSessionEnded(function(){
            suppressWarnings(db_disconnect(shiny::isolate(instance$tkcat)))
         })
      }
      
      ########################@
      ## Download directory ----
      dd <- !is.null(ddir) && dir.exists(ddir)
      if(dd){
         tddir <- file.path(ddir, session$token)
         dir.create(tddir, showWarnings=TRUE)
         shiny::onStop(function(){
            unlink(tddir, recursive=TRUE, force=TRUE)
         })
      }
      
      ########################@
      ## Selection status ----
      selStatus <- shiny::reactiveValues(
         resource=NULL,
         mdb=NULL,
         tables=NULL
      )
      output$status <- shiny::renderUI({
         mdb <- selStatus$mdb
         shiny::req(mdb)
         dbi <- db_info(mdb)
         shiny::tags$p(
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
         mdbs$list <- list_MDBs(instance$tkcat)
         mdbs$collections <- collection_members(instance$tkcat)
      })
      output$mdbList <- DT::renderDT({
         shiny::req(mdbs$list)
         toShow <- mdbs$list %>%
            dplyr::select("name", "title") %>%
            dplyr::rename("Resource"="name", "Title"="title")
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
         mdbs$validInput <- TRUE
         DT::datatable(
            toShow,
            rownames=FALSE,
            filter="top",
            selection = list(
               mode="single",
               selected=which(
                  toShow$Resource==shiny::isolate(selStatus$resource)
               )
            ),
            extensions='Scroller',
            options = list(
               deferRender = TRUE,
               scrollY = "70vh",
               scroller = TRUE,
               columnDefs=list(
                  list(width='60%', targets=1)
               ),
               order=list(list(0, 'asc')),
               dom=c("ti")
            )
         )
      })
      mdbListProxy <- DT::dataTableProxy("mdbList")
      
      shiny::observe({
         shiny::req(mdbs$validInput)
         s <- input$mdbList_rows_selected
         n <- mdbs$list$name[s]
         if(length(n)==0 || n==""){
            selStatus$resource <- NULL
            selStatus$mdb <- NULL
         }else{
            selStatus$resource <- n
         }
      })
      shiny::observe({
         n <- selStatus$resource
         shiny::req(n)
         mdb <- try(get_MDB(instance$tkcat, n, n_max=0), silent=TRUE)
         selStatus$mdb <- mdb
         if(
            inherits(mdb, "try-error") ||
            !all(isolate(selStatus$tables) %in% names(mdb))
         ){
            selStatus$tables <- NULL
         }
      })
      shiny::observe({
         mdb <- selStatus$mdb
         shiny::req(mdb)
         tn <- isolate(selStatus$tables)
         if(!all(tn %in% names(mdb))){
            selStatus$tables <- NULL
         }
      })
      
      ########################@
      ## DB information ----
      
      output$dbInfo <- shiny::renderUI({
         mdb <- selStatus$mdb
         shiny::req(!is.null(mdb))
         if(inherits(mdb, "try-error")){
            n <- isolate(selStatus$resource)
            shiny::tagList(
               shiny::tags$p(
                  "You don't have access to",
                  shiny::strong(n),
                  '(You can sign in with different credentials)',
                  style="color:red;"
               )
            )
         }else{
            dbi <- db_info(mdb)
            if(is.fileMDB(mdb)){
               dbs <- sum(data_file_size(mdb))
               dbi$size <- .format_file_size(dbs)
            }else{
               if(!is.metaMDB(mdb)){
                  dbi$records <- sum(count_records(mdb))
               }
            }
            shiny::tagList(
               shiny::h3(dbi$name),
               do.call(shiny::tags$ul, lapply(
                  setdiff(names(dbi), c("name")),
                  function(n){
                     if(!is.na(dbi[[n]]) && dbi[[n]]!=""){
                        if(n=="url"){
                           vt <- tags$a(
                              shiny::HTML(dbi[[n]]),
                              href=dbi[[n]], target="_blank"
                           ) %>% as.character()
                        }else if(n=="maintainer"){
                           vt <- markdown::renderMarkdown(text=dbi[[n]])
                           vt <- gsub("<[/]?p>", "", as.character(vt))
                        }else if(is.numeric(dbi[[n]])){
                           vt <- format(dbi[[n]], big.mark=",")
                        }else{
                           vt <- dbi[[n]]
                        }
                        return(shiny::tags$li(
                           shiny::strong(n), ":",
                           shiny::HTML(vt))
                        )
                     }
                  }
               )),
               shiny::uiOutput("dbDownload")
            )
         }
      })
      
      if(dd){
         
         reqDbs <- shiny::reactiveVal(character(0))
         dbdone <- shiny::reactiveVal(character(0))
         
         output$dbDownload <- renderUI({
            dbdone()
            input$refreshDbdown
            mdb <- selStatus$mdb
            shiny::req(mdb)
            n <- shiny::isolate(selStatus$resource)
            
            fname <- paste0(n, ".zip")
            if(!fname %in% reqDbs()){
               return(
                  shiny::p(
                     shiny::actionButton(
                        "prepDbdown",
                        "Request database for download"
                     ),
                     shiny::strong(paste(
                        "(Preparing the data may take time)"
                     ))
                  )
               )
            }
            f <- file.path(tddir, fname)
            if(!file.exists(f)){
               return(
                  p(
                     strong(
                        "The archive is being prepared", style="color:blue;"
                     ),
                     shiny::actionButton("refreshDbdown", "Check availability")
                  )
               )
            }
            return(
               shiny::a(
                  list(icon("download"), sprintf("Download %s", n)),
                  id="downloadTable",
                  class="btn btn-default shiny-download-link shiny-bound-output",
                  href=file.path("data", session$token, fname),
                  target="_blank",
                  download=""
               )
            )
         })
         
         observeEvent(input$prepDbdown, {
            mdb <- isolate(selStatus$mdb)
            shiny::req(mdb)
            n <- shiny::isolate(selStatus$resource)
            fname <- paste0(n, ".zip")
            reqDbs(union(reqDbs(), fname))
            f <- file.path(tddir, fname)
            tf <- tempfile(tmpdir=tddir, fileext=".zip")
            if(!file.exists(f)){
               if(is.chMDB(mdb)){
                  p <- isolate(upwd())
               }
               future::future({
                  if(is.chMDB(mdb)){
                     db_reconnect(mdb, password=p)
                  }
                  
                  dbloc <- tempfile()
                  as_fileMDB(mdb, path=dbloc)
                  cd <- getwd()
                  on.exit({
                     setwd(cd)
                     unlink(dbloc, recursive=TRUE)
                  })
                  setwd(dbloc)
                  suppressMessages(
                     utils::zip(zipfile=tf, files=n, flags="-r9Xq")
                  )
                  
                  file.rename(tf, f)
                  return(fname)
               }, seed=TRUE) %>% dbdone()
            }
         })
         
      }
      
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
                        '%s data model'
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
         if(is.chMDB(mdb)){
            dbdm$model <- data_model(mdb)
            dbdm$collections <- collection_members(mdb)
         }else{
            dbdm$model <- NULL
            dbdm$collections <- NULL
            dbdm$validInput <- FALSE
         }
      })
      
      output$dataModel <- visNetwork::renderVisNetwork({
         mdb <- selStatus$mdb
         shiny::req(mdb)
         dm <- data_model(mdb)
         dbdm$validInput <- TRUE
         nodesIdSelection <- list(enabled=TRUE, useLabels=FALSE)
         sel <- isolate(selStatus$tables) %>% 
            intersect(names(mdb))
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
         mdb <- isolate(selStatus$mdb)
         shiny::req(mdb)
         if(length(n)==0 || n=="" || !all(n %in% names(mdb))){
            selStatus$tables <- NULL
         }else{
            selStatus$tables <- n
         }
      })
      
      shiny::observe({
         selTables <- selStatus$tables
         visNetworkProxy("dataModel") %>%
            visSelectNodes(selTables)
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
         mdb <- selStatus$mdb
         shiny::req(mdb)
         cm <- collection_members(mdb)
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
         mdb <- isolate(selStatus$mdb)
         shiny::req(mdb)
         cmt <- collection_members(mdb) %>%
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
         sel <- selStatus$tables %>% 
            intersect(names(mdb))
         shiny::req(sel)
         shiny::req(length(sel)==1)
         shiny::tagList(
            shiny::h3(sel),
            shiny::tags$ul(
               if(is.fileMDB(mdb)){
                  shiny::tags$li(
                     shiny::tags$strong("File size"),
                     ":",
                     .format_file_size(data_file_size(mdb)[sel]),
                     sprintf("(showing %s records)", nrow(tabSubSet()))
                  )
               }else{
                  if(!is.metaMDB(mdb)){
                     shiny::tags$li(
                        shiny::tags$strong("Number of records"),
                        ":",
                        count_records(mdb, dplyr::all_of(sel)) %>%
                           format(big.mark=","),
                        sprintf("(showing %s records)", nrow(tabSubSet()))
                     )
                  }
               }
            ),
            DT::DTOutput("dataSample"),
            shiny::tags$br(),
            shiny::uiOutput("tableDownload")
         )
      })
      
      tabSubSet <- reactiveVal(NULL)
      shiny::observe({
         tabSubSet(NULL)
         mdb <- selStatus$mdb
         shiny::req(mdb)
         sel <- selStatus$tables %>% 
            intersect(names(mdb))
         shiny::req(sel)
         shiny::req(length(sel)==1)
         toShow <- data_tables(mdb, dplyr::all_of(sel), n_max=subSetSize)[[1]]
         if(utils::object.size(toShow) > 2^19){
            toShow <- toShow[
               1:max(c(
                  1, ceiling(nrow(toShow)*(2^19/utils::object.size(toShow)))
               )),
            ]
         }
         tabSubSet(toShow)
      })
      output$dataSample <- DT::renderDT({
         toShow <- tabSubSet()
         shiny::req(toShow)
         DT::datatable(
            toShow,
            rownames=FALSE,
            selection = 'single',
            extensions='Scroller',
            options = list(
               deferRender = TRUE,
               scrollX=TRUE,
               scrollY = 250,
               scroller = TRUE,
               dom=c("ti")
            )
         )
      })
      
      if(dd){
         
         reqtables <- shiny::reactiveVal(character(0))
         tabledone <- shiny::reactiveVal(character(0))
         
         output$tableDownload <- renderUI({
            tabledone()
            input$refreshTabledown
            mdb <- selStatus$mdb
            shiny::req(mdb)
            sel <- selStatus$tables %>% 
               intersect(names(mdb))
            shiny::req(sel)
            shiny::req(length(sel)==1)
            fname <- file.path(db_info(mdb)$name, paste0(sel, ".txt.gz"))
            if(!fname %in% reqtables()){
               return(
                  shiny::p(
                     shiny::actionButton(
                        "prepTabledown",
                        "Request table for download"
                     ),
                     shiny::strong(paste(
                        "(Preparing the data may take time)"
                     ))
                  )
               )
            }
            f <- file.path(tddir, fname)
            if(!file.exists(f)){
               return(
                  p(
                     strong("The file is being prepared", style="color:blue;"),
                     shiny::actionButton("refreshTabledown", "Check availability")
                  )
               )
            }
            return(
               shiny::a(
                  list(icon("download"), sprintf("Download %s", sel)),
                  id="downloadTable",
                  class="btn btn-default shiny-download-link shiny-bound-output",
                  href=file.path("data", session$token, fname),
                  target="_blank",
                  download=""
               )
            )
         })
         
         observeEvent(input$prepTabledown, {
            mdb <- isolate(selStatus$mdb)
            shiny::req(mdb)
            sel <- isolate(selStatus$tables) %>% 
               intersect(names(mdb))
            shiny::req(sel)
            shiny::req(length(sel)==1)
            fname <- file.path(db_info(mdb)$name, paste0(sel, ".txt.gz"))
            reqtables(union(reqtables(), fname))
            f <- file.path(tddir, fname)
            tf <- tempfile(tmpdir=tddir, fileext=".txt.gz")
            if(!file.exists(f)){
               if(is.chMDB(mdb)){
                  p <- isolate(upwd())
               }
               future::future({
                  if(is.chMDB(mdb)){
                     db_reconnect(mdb, password=p)
                  }
                  if(!dir.exists(dirname(f))){
                     dir.create(dirname(f))
                  }
                  readr::write_tsv(
                     data_tables(mdb, dplyr::all_of(sel))[[1]], tf
                  )
                  file.rename(tf, f)
                  return(fname)
               }, seed=TRUE) %>% tabledone()
            }
         })
         
      }
      
      
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
            n <- isolate(selStatus$resource)
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
         rt <- isolate(searchRes$resources)
         shiny::req(rt)
         mdbListProxy %>%
            DT::selectRows(
               which(isolate(mdbs$list$name) %in% pull(slice(rt, sel), "name"))
            )
      })
      ## _+ tables ----
      shiny::observe({
         mdbs <- mdbs$list
         shiny::req(mdbs)
         st <- input$searchInput
         shiny::req(st)
         toRet <- search_MDB_tables(instance$tkcat, st)
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
         rt <- isolate(searchRes$tables)
         shiny::req(rt)
         mdbListProxy %>%
            DT::selectRows(
               which(
                  isolate(mdbs$list$name) %in% pull(slice(rt, sel), "resource")
               )
            )
         selStatus$tables <- rt %>% slice(sel) %>% pull("name")
      })
      ## _+ fields ----
      shiny::observe({
         mdbs <- mdbs$list
         shiny::req(mdbs)
         st <- input$searchInput
         shiny::req(st)
         toRet <- search_MDB_fields(instance$tkcat, st)
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
         rt <- isolate(searchRes$fields)
         shiny::req(rt)
         mdbListProxy %>%
            DT::selectRows(
               which(
                  isolate(mdbs$list$name) %in% pull(slice(rt, sel), "resource")
               )
            )
         selStatus$tables <- rt %>% slice(sel) %>% pull("table")
      })
      
      ###############################################@
      ## Managing connection of chTKCat objects   ####
      ###############################################@
      if(is.chTKCat(x)){
         
         upwd <- reactiveVal(value="")
            
         ########################@
         ## System information ----
         output$systemInfo <- shiny::renderUI({
            k <- instance$tkcat
            shiny::tagList(
               shiny::tags$ul(
                  shiny::tags$li(
                     shiny::tags$strong("Host"), ":", xparams$host
                  ),
                  shiny::tags$li(
                     shiny::tags$strong("Native port"), ":", k$chcon@port
                  ),
                  shiny::tags$li(
                     shiny::tags$strong("HTTP port"), ":", k$http
                  ),
                  shiny::tags$li(
                     shiny::tags$strong("Current user"), ":", k$chcon@user
                  ),
                  shiny::tags$li(
                     shiny::tags$strong("Instance"), ":", k$instance,
                     sprintf("(Version %s)", k$version)
                  ),
                  shiny::tags$li(
                     shiny::HTML(markdown::renderMarkdown(
                        text=paste(
                           shiny::tags$strong("Administrator"), ":",
                           k$contact
                        )
                     ))
                  )
               )
            )
         })
         
         ########################@
         ## Sign in ----
         output$signin <- shiny::renderUI({
            shiny::actionLink(
               inputId="silink",
               label=shiny::span("Sign in", style="margin-left:6px;"),
               icon=shiny::icon("sign-in-alt"),
               style="margin:0;"
            )
         })
         shiny::observeEvent(input$silink, {
            okConnect(TRUE)
            showModal(modalDialog(
               title="Sign in",
               shiny::div(
                  shiny::fluidRow(
                     shiny::column(
                        8,
                        shiny::textInput(
                           "userName", label=NULL, width="100%",
                           placeholder="User name"
                        ),
                        shiny::passwordInput(
                           "password", label=NULL, width="100%",
                           placeholder="Password"
                        ),
                     ),
                     shiny::column(4, shiny::actionButton("connect", "Connect"))
                  ),
                  shiny::fluidRow(shiny::column(
                     12,
                     shiny::uiOutput("notOkConnect"),
                     paste(
                        "Contact",
                        "if you've forgotten your password",
                        "or if you want to sign up:"
                     ),
                     shiny::HTML(markdown::renderMarkdown(
                        text=shiny::isolate(instance$tkcat$contact)
                     ))
                  ))
               ),
               size="s",
               easyClose=TRUE
            ))
         })
         okConnect <- shiny::reactiveVal(TRUE)
         output$notOkConnect <- shiny::renderUI({
            shiny::req(!okConnect())
            shiny::tagList(
               shiny::strong("Invalid credentials", style="color:red;"),
            )
         })
         shiny::observeEvent(input$connect, {
            u <- shiny::isolate(input$userName)
            p <- shiny::isolate(input$password)
            upwd(p)
            nk <- try(db_reconnect(
               shiny::isolate(instance$tkcat),
               user=u, password=p
            ), silent=TRUE)
            if(!inherits(nk, "try-error")){
               suppressWarnings(db_disconnect(instance$tkcat))
               instance$tkcat <- nk
               okConnect(TRUE)
               shiny::removeModal()
            }else{
               instance$tkcat <- db_reconnect(instance$tkcat, user="default")
               okConnect(FALSE)
            }
         })
         
         ########################@
         ## Sign out ----
         output$currentUser <- shiny::renderUI({
            shiny::actionLink(
               inputId=ifelse(
                  instance$tkcat$chcon@user!="default",
                  "solink",
                  "disabledSoLink"
               ),
               label=shiny::span(
                  shiny::HTML(paste(c(
                     ifelse(
                        instance$tkcat$chcon@user!="default",
                        instance$tkcat$chcon@user,
                        "Public access"
                     ),
                     ifelse(
                        instance$tkcat$chcon@user!="default",
                        as.character(shiny::span(
                           shiny::icon("sign-out-alt"),
                           style="margin-left:6px;"
                        )),
                        ""
                     )
                  ), collapse=" ")),
                  style="margin-left:6px;"
               ),
               icon=shiny::icon(
                  ifelse(
                     instance$tkcat$chcon@user!="default",
                     "user",
                     "user-slash"
                  )
               ),
               style="margin:0;",
               title=ifelse(
                  instance$tkcat$chcon@user!="default",
                  "Sign out",
                  ""
               )
            )
         })
         shiny::observeEvent(input$solink, {
            suppressWarnings(db_disconnect(instance$tkcat))
            instance$valid <- FALSE
         })
         shiny::observe({
            k <- instance$tkcat
            if(k$chcon@user=="default"){
               session$sendCustomMessage('showNavs', 'signinTab')
            }else{
               session$sendCustomMessage('hideNavs', 'signinTab')
            }
         })
      }
      
      ########################@
      ## Bookmarks ----
      shiny::observe({
         # Trigger this observer every time an input changes
         shiny::reactiveValuesToList(input)
         session$doBookmark()
      })
      shiny::onBookmark(function(state) {
         state$values$resource <- selStatus$resource
         state$values$tables <- selStatus$tables
      })
      shiny::onBookmarked(function(url){
         shiny::updateQueryString(url)
      })
      shiny::onRestore(function(state) {
         selStatus$resource <- state$values$resource
         selStatus$tables <- state$values$tables
      })
      
   }
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
