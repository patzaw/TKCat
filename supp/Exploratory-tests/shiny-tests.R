library(devTKCat)

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
buildUi <- function(){
   
   shiny::addResourcePath(
      "www",
      system.file("www", package="devTKCat")# package=utils::packageName())
   )
   shiny::addResourcePath(
      "doc",
      system.file("doc", package="devTKCat")# package=utils::packageName())
   )
   
   function(req){
      shinydashboard::dashboardPage(
         title="TKCat",
         
         ########################@
         ## Dashboard header ----
         header=shinydashboard::dashboardHeader(
            title=shiny::uiOutput("instance"),
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
         ),
         
         ########################@
         ## Sidebar ----
         sidebar=shinydashboard::dashboardSidebar(
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
            shinydashboard::sidebarMenu(
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
               ),
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
               ),
               shiny::tags$hr(),
               shinydashboard::menuItem(
                  "Documentation",
                  icon=shiny::icon("question-circle"),
                  shinydashboard::menuSubItem(
                     "clichouse dm",
                     href="doc/chTKCat-data-models.html"
                  )
               )
            )
         ),
         
         body=shinydashboard::dashboardBody(
            
            ########################@
            ## Page header ----
            shiny::tags$head(
               shiny::tags$link(
                  rel="icon",
                  href='www/TKCat-small.png'
               ),
               shiny::tags$script(src='www/interactions.js')
            ),
            
            shinydashboard::tabItems(
               
               ########################@
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
               
               ########################@
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
               
               ########################@
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
               ),
               
               ########################@
               # System information ----
               shinydashboard::tabItem(
                  tabName="system",
                  shiny::fluidRow(
                     shiny::uiOutput("systemInfo")
                  )
               )
               
            )
            
         )
      )
   }
}


###############################################################################@
buildServer <- function(
   defaultConnection,
   subSetSize=100,
   host=defaultConnection$chcon@host
){
   function(input, output, session) {
      
      ########################@
      ## TKCat instance ----
      instance <- shiny::reactiveValues(
         tkcon=db_reconnect(defaultConnection, user="default"),
         valid=DBI::dbIsValid(defaultConnection$chcon)
      )
      shiny::observe({
         if(!instance$valid){
            instance$tkcon <- db_reconnect(
               shiny::isolate(instance$tkcon), user="default"
            )
            instance$valid <- TRUE
         }
      })
      output$instance <- shiny::renderUI({
         instance$tkcon$instance
      })
      shiny::onSessionEnded(function(){
         suppressWarnings(db_disconnect(shiny::isolate(instance$tkcon)))
      })
      
      ########################@
      ## Selection status ----
      selStatus <- shiny::reactiveValues(
         resource=NULL,
         mdb=NULL,
         tables=NULL
      )
      output$status <- shiny::renderUI({
         mdb <- selStatus$mdb
         req(mdb)
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
         mdbs$list <- list_chMDBs(instance$tkcon)
         mdbs$collections <- collection_members(instance$tkcon)
      })
      output$mdbList <- DT::renderDT({
         shiny::req(mdbs$list)
         toShow <- mdbs$list %>%
            dplyr::select(name, title) %>%
            dplyr::rename("Resource"="name", "Title"="title")
         cm <- mdbs$collections %>%
            dplyr::select("collection", "resource") %>%
            dplyr::distinct() %>%
            dplyr::group_by(.data$resource) %>% 
            dplyr::mutate(collection=c(.data$collection)) %>%
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
               dom=c("ti")
            )
         )
      })
      mdbListProxy <- DT::dataTableProxy("mdbList")
      observe({
         mdbListProxy %>%
            DT::selectRows(
               which(isolate(mdbs$list$name) %in% selStatus$resource)
            )
      })
      
      shiny::observe({
         shiny::req(mdbs$validInput)
         s <- input$mdbList_rows_selected
         n <- mdbs$list$name[s]
         selStatus$resource <- n
         if(length(n)==0 || n==""){
            selStatus$resource <- NULL
            selStatus$mdb <- NULL
         }else{
            selStatus$resource <- n
         }
      })
      observe({
         n <- selStatus$resource
         req(n)
         mdb <- try(get_chMDB(instance$tkcon, n), silent=TRUE)
         selStatus$mdb <- mdb
         if(
            inherits(mdb, "try-error") ||
            !all(isolate(selStatus$tables) %in% names(mdb))
         ){
            selStatus$tables <- NULL
         }
      })
      observe({
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
            dbi$records <- sum(count_records(mdb))
            shiny::tagList(
               shiny::h3(dbi$name),
               do.call(shiny::tags$ul, lapply(
                  setdiff(names(dbi), c("name", "tkcon", "table_records")),
                  function(n){
                     if(!is.na(dbi[[n]]) && dbi[[n]]!=""){
                        if(n=="url"){
                           vt <- tags$a(
                              shiny::HTML(dbi[[n]]),
                              href=dbi[[n]], target="_blank"
                           ) %>% as.character()
                        }else if(n=="maintainer"){
                           vt <- markdown::renderMarkdown(text=dbi[[n]]) %>%
                              as.character() %>%
                              gsub("<[/]?p>", "", .)
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
               shiny::tags$br(),
               shiny::downloadButton(
                  "downloadMDB", sprintf("Download %s", dbi$name)
               )
            )
         }
      })
      
      output$downloadMDB <- shiny::downloadHandler(
         filename = function() {
            n <- selStatus$resource
            shiny::req(n)
            paste0(n, ".zip")
         },
         content = function(file) {
            mdb <- selStatus$mdb
            shiny::req(mdb)
            n <- shiny::isolate(selStatus$resource)
            dbloc <- tempfile()
            as_fileMDB(mdb, path=dbloc)
            cd <- getwd()
            on.exit({
               setwd(cd)
               unlink(dbloc, recursive=TRUE)
            })
            setwd(dbloc)
            suppressMessages(utils::zip(zipfile=file, files=n, flags="-r9Xq"))
         }
      )
      
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
                  label=htmltools::HTML(sprintf(
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
         sel <- isolate(selStatus$tables)
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
      
      observe({
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
      observe({
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
         sel <- selStatus$tables
         shiny::req(sel)
         shiny::req(length(sel)==1)
         shiny::tagList(
            shiny::h3(sel),
            shiny::tags$ul(
               shiny::tags$li(
                  shiny::tags$strong("Number of records"),
                  ":",
                  count_records(mdb, dplyr::all_of(sel)) %>%
                     format(big.mark=","),
                  sprintf("(showing %s)", nrow(tabSubSet()))
               )
            ),
            DT::DTOutput("dataSample"),
            shiny::tags$br(),
            shiny::downloadButton(
               "downloadTable", sprintf("Download %s", sel)
            )
         )
      })
      
      tabSubSet <- reactiveVal(NULL)
      observe({
         tabSubSet(NULL)
         mdb <- selStatus$mdb
         shiny::req(mdb)
         sel <- selStatus$tables
         shiny::req(sel)
         shiny::req(length(sel)==1)
         toShow <- data_tables(mdb, dplyr::all_of(sel), n_max=subSetSize)[[1]]
         if(object.size(toShow) > 2^19){
            toShow <- toShow[
               1:max(c(1, ceiling(nrow(toShow)*(2^19/object.size(toShow))))),
            ]
         }
         tabSubSet(toShow)
      })
      output$dataSample <- DT::renderDT({
         toShow <- tabSubSet()
         req(toShow)
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
      
      output$downloadTable <- shiny::downloadHandler(
         filename = function() {
            sel <- selStatus$tables
            shiny::req(sel)
            shiny::req(length(sel)==1)
            paste0(sel, ".txt.gz")
         },
         content = function(file) {
            mdb <- selStatus$mdb
            shiny::req(mdb)
            sel <- selStatus$tables
            shiny::req(sel)
            shiny::req(length(sel)==1)
            readr::write_tsv(data_tables(mdb, dplyr::all_of(sel))[[1]], file)
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
         req(c(
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
         req(st)
         shiny::tagList(
            shiny::h3("Resources"),
            DT::DTOutput("searchResRes")
         )
      })
      output$searchResRes <- DT::renderDT({
         st <- input$searchInput
         req(st)
         toRet <- searchRes$resources
         req(toRet)
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
      observe({
         sel <- input$searchResRes_rows_selected
         req(sel)
         rt <- isolate(searchRes$resources)
         req(rt)
         selStatus$resource <- rt %>% slice(sel) %>% pull("name")
      })
      ## _+ tables ----
      shiny::observe({
         mdbs <- mdbs$list
         shiny::req(mdbs)
         st <- input$searchInput
         shiny::req(st)
         selQueries <- paste(
            sprintf(
               "SELECT '%s' as resource, name, comment,",
               mdbs$name
            ),
            sprintf(
               "positionCaseInsensitive(name, '%s')>0 as s1,",
               st
            ),
            if(nchar(st)>4){
               sprintf(
                  "ngramSearchCaseInsensitive(name, '%s') as s2,",
                  st
               )
            }else{
               "0 as s2,"
            },
            sprintf(
               "if(isNull(comment), 0, positionCaseInsensitive(comment, '%s')>0) as s3,",
               st
            ),
            if(nchar(st)>4){
               sprintf(
                  "if(isNull(comment), 0, ngramSearchCaseInsensitive(comment, '%s')) as s4,",
                  st
               )
            }else{
               "0 as s4,"
            },
            "greatest(s4, greatest(s3, greatest(s2, s1))) as ms",
            sprintf("FROM `%s`.`___Tables___`", mdbs$name),
            "WHERE ms > 0"
         )
         query <- paste(selQueries, collapse=" UNION ALL ")
         toRet <- DBI::dbGetQuery(instance$tkcon$chcon, query)
         if(nrow(toRet)>0){
            searchRes$tables <- toRet %>% 
               dplyr::as_tibble() %>%
               dplyr::arrange(desc(ms)) %>%
               dplyr::select("resource", "name", "comment")
         }else{
            searchRes$tables <- NULL
         }
      })
      output$searchTables <- shiny::renderUI({
         st <- input$searchInput
         req(st)
         shiny::tagList(
            shiny::h3("Tables"),
            DT::DTOutput("searchTabRes")
         )
      })
      output$searchTabRes <- DT::renderDT({
         st <- input$searchInput
         req(st)
         toRet <- searchRes$tables
         req(toRet)
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
      observe({
         sel <- input$searchTabRes_rows_selected
         req(sel)
         rt <- isolate(searchRes$tables)
         req(rt)
         selStatus$resource <- rt %>% slice(sel) %>% pull("resource")
         selStatus$tables <- rt %>% slice(sel) %>% pull("name")
      })
      ## _+ fields ----
      shiny::observe({
         mdbs <- mdbs$list
         shiny::req(mdbs)
         st <- input$searchInput
         shiny::req(st)
         selQueries <- paste(
            sprintf(
               "SELECT '%s' as resource, table, name, type, nullable, unique, comment,",
               mdbs$name
            ),
            sprintf(
               "positionCaseInsensitive(name, '%s')>0 as s1,",
               st
            ),
            if(nchar(st)>4){
               sprintf(
                  "ngramSearchCaseInsensitive(name, '%s') as s2,",
                  st
               )
            }else{
               "0 as s2,"
            },
            sprintf(
               "if(isNull(comment), 0, positionCaseInsensitive(comment, '%s')>0) as s3,",
               st
            ),
            if(nchar(st)>4){
               sprintf(
                  "if(isNull(comment), 0, ngramSearchCaseInsensitive(comment, '%s')) as s4,",
                  st
               )
            }else{
               "0 as s4,"
            },
            "greatest(s4, greatest(s3, greatest(s2, s1))) as ms",
            sprintf("FROM `%s`.`___Fields___`", mdbs$name),
            "WHERE ms > 0"
         )
         query <- paste(selQueries, collapse=" UNION ALL ")
         toRet <- DBI::dbGetQuery(instance$tkcon$chcon, query)
         if(nrow(toRet)>0){
            searchRes$fields <- toRet %>% 
               dplyr::as_tibble() %>%
               dplyr::arrange(desc(ms)) %>%
               dplyr::select(
                  "resource", "table", "name", "comment",
                  "type", "nullable", "unique"
               ) %>%
               dplyr::mutate(
                  nullable=as.logical(.data$nullable),
                  unique=as.logical(.data$unique)
               )
         }else{
            searchRes$fields <- NULL
         }
      })
      output$searchFields <- shiny::renderUI({
         st <- input$searchInput
         req(st)
         shiny::tagList(
            shiny::h3("Fields"),
            DT::DTOutput("searchFieldRes")
         )
      })
      output$searchFieldRes <- DT::renderDT({
         st <- input$searchInput
         req(st)
         toRet <- searchRes$fields
         req(toRet)
         if(nrow(toRet)>0){
            toRet %>%
               dplyr::mutate(
                  # resource=.highlightText(.data$resource, st),
                  # table=.highlightText(.data$resource, st),
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
      observe({
         sel <- input$searchFieldRes_rows_selected
         req(sel)
         rt <- isolate(searchRes$fields)
         req(rt)
         selStatus$resource <- rt %>% slice(sel) %>% pull("resource")
         selStatus$tables <- rt %>% slice(sel) %>% pull("table")
      })
      
      ########################@
      ## System information ----
      output$systemInfo <- shiny::renderUI({
         k <- instance$tkcon
         shiny::tagList(
            shiny::tags$ul(
               shiny::tags$li(
                  shiny::tags$strong("Host"), ":", host
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
               shiny::fluidRow(shiny::column(12,
                  shiny::uiOutput("notOkConnect"),
                  paste(
                     "Contact",
                     "if you've forgotten your password",
                     "or if you want to sign up:"
                  ),
                  shiny::HTML(markdown::renderMarkdown(
                     text=shiny::isolate(instance$tkcon$contact)
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
         nk <- try(db_reconnect(
            shiny::isolate(instance$tkcon),
            user=u, password=p
         ), silent=TRUE)
         if(!inherits(nk, "try-error")){
            suppressWarnings(db_disconnect(instance$tkcon))
            instance$tkcon <- nk
            okConnect(TRUE)
            shiny::removeModal()
         }else{
            instance$tkcon <- db_reconnect(instance$tkcon, user="default")
            okConnect(FALSE)
         }
      })
      
      ########################@
      ## Sign out ----
      output$currentUser <- shiny::renderUI({
         shiny::actionLink(
            inputId=ifelse(
               instance$tkcon$chcon@user!="default",
               "solink",
               "disabledSoLink"
            ),
            label=shiny::span(
               shiny::HTML(paste(c(
                  ifelse(
                     instance$tkcon$chcon@user!="default",
                     instance$tkcon$chcon@user,
                     "Public access"
                  ),
                  ifelse(
                     instance$tkcon$chcon@user!="default",
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
                  instance$tkcon$chcon@user!="default",
                  "user",
                  "user-slash"
               )
            ),
            style="margin:0;",
            title=ifelse(
               instance$tkcon$chcon@user!="default",
               "Sign out",
               ""
            )
         )
      })
      shiny::observeEvent(input$solink, {
         suppressWarnings(db_disconnect(instance$tkcon))
         instance$valid <- FALSE
      })
      shiny::observe({
         k <- instance$tkcon
         if(k$chcon@user=="default"){
            session$sendCustomMessage('showNavs', 'signinTab')
         }else{
            session$sendCustomMessage('hideNavs', 'signinTab')
         }
      })
      
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

## Run the application ----
shiny::shinyApp(
   ui=buildUi(),
   server=buildServer(
      defaultConnection=chTKCat(
         port=9201, http=9211,
         user="default", password=""
      ),
      subSetSize=100,
      host="bel040344" # default should be defaultConnection$chcon@host
   ),
   enableBookmarking="url"
)

