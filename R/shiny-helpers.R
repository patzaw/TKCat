###############################################################################@
## UI Helpers ####
###############################################################################@

TKCAT_LOGO_DIV <- shiny::div(
   shiny::a(
      shiny::img(
         src="www/TKCat-small.png",
         height="120px",
         id="mainLogo"
      ),
      href="https://patzaw.github.io/TKCat/",
      target="_blank"
   ),
   style=paste(
      "width:150px;",
      "margin-left:auto; margin-right:auto;",
      "margin-top:15px;",
      "margin-bottom:15px;",
      "text-align:center;"
   )
)

###############################################################################@
.etkc_add_resources <- function(ddir=NULL, rDirs=NULL){
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
   if(length(rDirs)>0) for(i in 1:length(rDirs)){
      shiny::addResourcePath(
         names(rDirs)[i],
         as.character(rDirs[i])
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
   userManager=FALSE,
   manList,
   logoDiv=TKCAT_LOGO_DIV,
   totalSize=FALSE
){
   
   ## Resources ----
   sbelts <- list(
      id="sidebarmenu",
      style="white-space:normal;",
      shinydashboard::menuItem(
         "Resources",
         tabName="resources",
         icon=shiny::icon("list-alt", verify_fa = FALSE)
      ),
      shinydashboard::menuItem(
         "Data model",
         tabName="model",
         icon=shiny::icon("project-diagram", verify_fa = FALSE)
      ),
      shinydashboard::menuItem(
         "Search resources",
         tabName="search",
         icon=shiny::icon("search", verify_fa = FALSE)
      )
   )
   
   ## System ----
   if(sysInterface){
      sbelts <- c(sbelts, list(
         shiny::tags$hr(),
         shinydashboard::menuItem(
            "System",
            tabName="system",
            icon=shiny::icon("info-circle", verify_fa = FALSE)
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
      if(userManager){
         sbelts <- c(sbelts, list(
            shinydashboard::menuItem(
               shiny::uiOutput("userManager"),
               tabName="userManagerTab",
               icon=NULL
            )
         ))
      }
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
               icon=shiny::icon("question-circle", verify_fa = FALSE)
            ),
            mit
         ))
      ))
   }
   
   #### Size ----
   if(totalSize){
      sbelts <- c(sbelts, list(
         shiny::tags$hr(),
         shinydashboard::menuItem(
            shiny::uiOutput("totalSize"),
            icon=NULL
         )
      ))
   }
   
   return(shinydashboard::dashboardSidebar(
      ## Logo ----
      logoDiv,
      shiny::tags$hr(),
      ## Menu ----
      do.call(shinydashboard::sidebarMenu, sbelts)
   ))
}

###############################################################################@
.etkc_sd_body <- function(sysInterface, tabIcon='www/TKCat-small.png'){
   
   belts <- list(
      
      ## Resources ----
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
         shiny::tags$style(shiny::HTML(
            paste(
               'table.dataTable tr.selected td a',
               '{background-color: white !important;}'
            )
         )),
         shiny::tags$link(
            rel="icon",
            href=tabIcon
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
   ddir=NULL,
   userManager=NULL,
   skinColors=c("blue", "yellow"),
   title=NULL,
   totalSize=FALSE
){
   
   pckn <- utils::packageName()
   jsFormatBytes <- readLines(system.file("www/format_bytes.js", package=pckn))
   
   
   function(input, output, session) {
      
      
      ########################@
      ## TKCat instance ----
      
      if(length(title)==1 && !is.na(title)){
         customTitle <- TRUE
         output$instance <- shiny::renderUI({
            title
         })
      }else{
         customTitle <- FALSE
      }
      
      if(is.TKCat(x)){
         instance <- shiny::reactiveValues(
            tkcat=x
         )
         if(!customTitle){
            output$instance <- shiny::renderUI({
               "Local TKCat"
            })
         }
      }
      
      if(is.chTKCat(x)){
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
         if(!customTitle){
            output$instance <- shiny::renderUI({
               paste("chTKCat :", instance$tkcat$instance)
            })
         }
         shiny::observe({
            if(instance$tkcat$chcon@user=="default"){
               sc <- skinColors[1]
            }else{
               sc <- skinColors[2]
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
      ## Total size ----
      if(totalSize){
         output$totalSize <- shiny::renderUI({
            tkcon <- instance$tkcat
            req(tkcon)
            totalSize <- sum(list_tables(tkcon)$total_bytes,  na.rm = TRUE)
            shiny::actionLink(
               inputId="totalSize",
               label=shiny::span(
                  paste("Total size:", .format_bytes(totalSize))
               ),
               icon=shiny::icon(
                  "database",
                  verify_fa = FALSE
               ),
               style="margin:0;",
               title="Total size"
            )
         })
      }
      
      ########################@
      ## Selection status ----
      selStatus <- shiny::reactiveValues(
         resource=NULL,
         mdb=NULL,
         access=NULL,
         tables=NULL
      )
      output$status <- shiny::renderUI({
         mdb <- selStatus$mdb
         shiny::req(mdb)
         if(is.MDB(mdb)){
            dbi <- db_info(mdb)
         }else{
            dbi <- mdb$dbInfo
         }
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
         shiny::withProgress(
            message="Getting list of MDBs",
            expr={
               mdbs$list <- list_MDBs(instance$tkcat)
               mdbs$collections <- collection_members(instance$tkcat)
            }
         )
      })
      output$mdbList <- DT::renderDT({
         shiny::req(mdbs$list)
         colToTake <- intersect(
            c(
               "name", "title", "access", "maintainer", "timestamp",
               "total_size"
            ),
            colnames(mdbs$list)
         )
         toShow <- mdbs$list %>%
            dplyr::select(dplyr::all_of(colToTake)) %>%
            dplyr::rename("Resource"="name") %>%  
            dplyr::rename_all(function(x){
               x <- gsub("_", " ", x)
               paste0(
                  toupper(substr(x, 1, 1)),
                  substr(x, 2, nchar(x))
               )
            })
         if("Total size" %in% colnames(toShow)){
            toShow <- toShow %>%  
               dplyr::mutate("Total size" = signif(
                  .data$`Total size`/2^(30), 3)
               ) %>% 
               dplyr::rename("Total size (GB)"="Total size")
         }
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
               # columnDefs = list(list(
               #    targets = which(colnames(toShow)=="Total size")-1,
               #    render = DT::JS(jsFormatBytes)
               # ))
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
         shiny::withProgress(
            message=sprintf("Getting %s metadata", n),
            expr={
               if(!is.null(access) && access=="none"){
                  mdb <- tryCatch(
                     get_chMDB_metadata(instance$tkcat, n),
                     error = function(e) e
                  )
               }else{
                  mdb <- tryCatch(
                     get_MDB(instance$tkcat, n, check=FALSE),
                     error = function(e) e
                  )
               }
               selStatus$mdb <- mdb
               if(is.MDB(mdb)){
                  m <- data_model(mdb)
               }
               if(is.list(mdb) && "dataModel" %in% names(mdb)){
                  m <- mdb$dataModel
               }
               if(
                  inherits(mdb, c("try-error", "error")) ||
                  !all(shiny::isolate(selStatus$tables) %in% names(m))
               ){
                  selStatus$tables <- NULL
               }
         })
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
         if(inherits(mdb, c("try-error", "error")) || !all(tn %in% names(m))){
            selStatus$tables <- NULL
         }
      })
      
      ########################@
      ## DB information ----
      
      output$dbInfo <- shiny::renderUI({
         mdb <- selStatus$mdb
         shiny::req(!is.null(mdb))
         access <- shiny::isolate(selStatus$access)
         tkcon <- shiny::isolate(instance$tkcat)
         if(inherits(mdb, c("try-error", "error"))){
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
               dbi$"Number of tables" <- length(dm)
               dbi$"Number of fields" <- lapply(
                  dm, function(x) nrow(x$fields)
               ) %>% 
                  unlist() %>% 
                  sum()
               if(is.fileMDB(mdb)){
                  dbs <- sum(data_file_size(mdb)$size)
                  dbi$size <- .format_bytes(dbs)
               }else{
                  if(!is.metaMDB(mdb)){
                     dbdims <- dims(mdb)
                     dbi$values <- sum(dbdims$nrow*dbdims$ncol)
                     dbi$size <- .format_bytes(sum(dbdims$bytes))
                  }
               }
               if(is.chMDB(mdb)){
                  ts <- get_chMDB_timestamps(unclass(mdb)$tkcon, dbi$name)
                  if(!is.null(ts)){
                     dbi$"other instances"=nrow(ts)-1
                  }
               }
            }else{
               dbi <- mdb$dbInfo
               dm <- mdb$dataModel
               dbi$"Number of tables" <- length(dm)
               dbi$"Number of fields" <- lapply(
                  dm, function(x) nrow(x$fields)
               ) %>% 
                  unlist() %>% 
                  sum()
               ts <- get_chMDB_timestamps(tkcon, dbi$name)
               if(!is.null(ts)){
                  dbi$"other instances"=nrow(ts)-1
               }
            }
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
         
         output$dbDownload <- shiny::renderUI({
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
                  list(
                     shiny::icon("download", verify_fa = FALSE),
                     sprintf("Download %s", n)
                  ),
                  id="downloadTable",
                  class=paste(
                     "btn btn-default shiny-download-link shiny-bound-output"
                  ),
                  href=file.path("data", session$token, fname),
                  target="_blank",
                  download=""
               )
            )
         })
         
         shiny::observeEvent(input$prepDbdown, {
            mdb <- shiny::isolate(selStatus$mdb)
            shiny::req(mdb)
            n <- shiny::isolate(selStatus$resource)
            fname <- paste0(n, ".zip")
            reqDbs(union(reqDbs(), fname))
            f <- file.path(tddir, fname)
            tf <- tempfile(tmpdir=tddir, fileext=".zip")
            if(!file.exists(f)){
               if(is.chMDB(mdb)){
                  p <- shiny::isolate(upwd())
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
         if(is.null(mdb) || inherits(mdb, c("try-error", "error"))){
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
                        .format_bytes(),
                     sprintf("(showing %s records)", nr)
                  )
               }else{
                  if(!is.metaMDB(mdb)){
                     tdim <- dims(mdb, all_of(sel))
                     list(
                        shiny::tags$li(
                           shiny::tags$strong("Number of columns"),
                           ":",
                           tdim$ncol %>%
                              format(big.mark=",")
                        ),
                        shiny::tags$li(
                           shiny::tags$strong("Number of rows"),
                           ":",
                           tdim$nrow %>%
                              format(big.mark=",")
                        ),
                        shiny::tags$li(
                           shiny::tags$strong("Size"),
                           ":",
                           .format_bytes(tdim$bytes)
                        ),
                        shiny::tags$li(
                           shiny::tags$strong("Number of values"),
                           ":",
                           (tdim$nrow*tdim$ncol) %>%
                              format(big.mark=","),
                           sprintf("(showing %s records)", nr)
                        )
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
      
      
      if(dd){
         
         reqtables <- shiny::reactiveVal(character(0))
         tabledone <- shiny::reactiveVal(character(0))
         
         output$tableDownload <- shiny::renderUI({
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
               return(shiny::tagList(
                  shiny::tags$br(),
                  shiny::p(
                     strong("The file is being prepared", style="color:blue;"),
                     shiny::actionButton(
                        "refreshTabledown", "Check availability"
                     )
                  )
               ))
            }
            return(shiny::tagList(
               shiny::tags$br(),
               shiny::a(
                  list(
                     shiny::icon("download", verify_fa = FALSE),
                     sprintf("Download %s", sel)
                  ),
                  id="downloadTable",
                  class=paste(
                     "btn btn-default shiny-download-link shiny-bound-output"
                  ),
                  href=file.path("data", session$token, fname),
                  target="_blank",
                  download=""
               )
            ))
         })
         
         shiny::observeEvent(input$prepTabledown, {
            mdb <- shiny::isolate(selStatus$mdb)
            shiny::req(mdb)
            sel <- shiny::isolate(selStatus$tables) %>% 
               intersect(names(mdb))
            shiny::req(sel)
            shiny::req(length(sel)==1)
            fname <- file.path(db_info(mdb)$name, paste0(sel, ".txt.gz"))
            reqtables(union(reqtables(), fname))
            f <- file.path(tddir, fname)
            tf <- tempfile(tmpdir=tddir, fileext=".txt.gz")
            if(!file.exists(f)){
               if(is.chMDB(mdb)){
                  p <- shiny::isolate(upwd())
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
         if(inherits(mdb, c("try-error", "error"))){
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
      
      ###############################################@
      ## Managing connection of chTKCat objects   ####
      ###############################################@
      if(is.chTKCat(x)){
         
         upwd <- shiny::reactiveVal(value="")
            
         ########################@
         ## System information ----
         output$systemInfo <- shiny::renderUI({
            k <- instance$tkcat
            shiny::tagList(
               shiny::tags$ul(
                  shiny::tags$li(
                     shiny::tags$strong("Host"), ":", xparams$host
                  ),
                  if(length(k$ports) > 0 ){
                     shiny::tags$li(
                        shiny::tags$strong("Available ports"), ":",
                        do.call(shiny::tags$ul, lapply(names(k$ports), function(n){
                           shiny::tags$li(
                              shiny::tags$strong(n), ":", k$ports[n]
                           )
                        }))
                     )
                  }else{
                     NULL
                  },
                  shiny::tags$li(
                     shiny::tags$strong("Current user"), ":",
                     k$chcon@user
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
               icon=shiny::icon("sign-in-alt", verify_fa = FALSE),
               style="margin:0;"
            )
         })
         shiny::observeEvent(input$silink, {
            okConnect(TRUE)
            shiny::showModal(shiny::modalDialog(
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
            if(!inherits(nk, c("try-error", "error"))){
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
                        ifelse(
                           nchar(instance$tkcat$chcon@user) > 22,
                           paste0(substr(instance$tkcat$chcon@user, 1, 20), "..."),
                           instance$tkcat$chcon@user
                        ),
                        "Public access"
                     ),
                     ifelse(
                        instance$tkcat$chcon@user!="default",
                        as.character(shiny::span(
                           shiny::icon("sign-out-alt", verify_fa = FALSE),
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
                  ),
                  verify_fa = FALSE
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
         
         ########################@
         ## User manager ----
         if(!is.null(userManager)){
            output$userManager <- shiny::renderUI({
               shiny::actionLink(
                  inputId="umlink",
                  label=shiny::span("User settings", style="margin-left:6px;"),
                  icon=shiny::icon("user-cog", verify_fa = FALSE),
                  style="margin:0;"
               )
            })
            shiny::observeEvent(input$umlink, {
               shiny::showModal(shiny::modalDialog(
                  title="User settings",
                  shiny::div(
                     shiny::tags$iframe(
                        src=userManager, height=600, width="100%"
                     )
                  ),
                  size="l",
                  easyClose=TRUE
               ))
            })
         }
      }
      
      ########################@
      ## Bookmarks ----

      dtbkmf <- c(
         "state",
         "search",
         "search_columns",
         "column_clicked",
         "columns_current",
         "columns_selected",
         "columns_all",
         "columns_last_clicked",
         "row_clicked",
         "rows_current",
         "rows_selected",
         "rows_all",
         "row_last_clicked",
         "cell_clicked",
         "cells_current",
         "cells_selected",
         "cells_all",
         "cell_last_clicked"
      )
      shiny::setBookmarkExclude(c(
         "searchInput", "sidebarItemExpanded",
         "silink", "disabledSoLink",
         "solink",
         paste("mdbList", dtbkmf, sep="_"),
         paste("colMembers", dtbkmf, sep="_"),
         paste("dataSample", dtbkmf, sep="_"),
         paste("searchResRes", dtbkmf, sep="_"),
         paste("searchTabRes", dtbkmf, sep="_"),
         paste("searchFieldRes", dtbkmf, sep="_")
      )) 
      
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
