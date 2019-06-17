buildUi <- function(tkcon, Tabix=NA){
   shinyUI(navbarPage(
      title=div(
         img(
            src=paste(
               "data:image/png;base64,",
               base64encode(system.file(
                  "www/TKCat.png",
                  package = packageName()
               ))
            ),
            height="60px", style="margin-right:40px"
         ),
         paste(tkcon$instance, "on", tkcon$chcon@host),
         style="margin-right:40px"
      ),
      windowTitle="TKCat",
      id="MainApp",
      
      header=list(
         tags$head(
            tags$link(
               rel="icon",
               href=paste(
                  "data:image/png;base64,",
                  base64encode(system.file(
                     "www/TKCat.png",
                     package = packageName()
                  ))
               )
               # href="UCB-logo.jpg"
            ),
            tags$script("
     
           var doubleClickTime = 0;
           var threshold = 200;
   
           Shiny.addCustomMessageHandler('hideNavs', function(nav_label) {
             $('a[data-value=\"' + nav_label + '\"]').parent().css('display', 'none');
           });
       
           Shiny.addCustomMessageHandler('showNavs', function(nav_label) {
             $('a[data-value=\"' + nav_label + '\"]').parent().css('display', '');
           });
       
           Shiny.addCustomMessageHandler('setTabColor', function(message){
             $('a[data-value=\"'+message.name+'\"]').css('background-color', message.bg).css('color', message.color);
           });
       
           Shiny.addCustomMessageHandler('setTabLabel', function(message){
             $('a[data-value=\"'+message.name+'\"]').text(message.label);
           });
     
         "),
            includeCSS(system.file(
               "www/cerulean.css",
               package = packageName()
            )),
            includeCSS(system.file(
               "www/defChanges.css",
               package = packageName()
            ))
         )
      ),
      
      ## Resources ----
      tabPanel(
         "Resources",
         value="Resources",
         fluidRow(
            column(
               6,
               dataTableOutput("mdbList")
            ),
            column(
               6,
               uiOutput("dbInfo")
            )
         )
      ),
      
      ## Data model ----
      tabPanel(
         "Data model",
         value="Data model",
         fluidRow(
            column(
               7,
               visNetworkOutput("dataModel", height="80vh"),
               style="border:solid; min-height:85vh;"
            ),
            column(
               5,
               fluidRow(
                  uiOutput("collectionInfo"),
                  style="margin-left:15px;"
               ),
               fluidRow(
                  uiOutput("tableInfo"),
                  style="margin-left:15px;"
               )
            )
         )
      ),
      
      ## Documentation ----
      tabPanel(
         "Documentation",
         value="Documentation",
         fluidRow(
            h1("ClickHouse connection"),
            h2("Instance"),
            tags$ul(
               tags$li(tags$strong("Host: "), tkcon$chcon@host),
               tags$li(tags$strong("Native port: "), tkcon$chcon@port),
               tags$li(tags$strong("HTTP port: "), tkcon$http)
            ),
            h2("Interfaces"),
            p(
               "Many interfaces to ClickHouse databases are available and ",
               "described here: ",
               tags$a(
                  "https://clickhouse.yandex/docs/en/interfaces/",
                  href="https://clickhouse.yandex/docs/en/interfaces/",
                  target="_blank"
               )
            ),
            if(is.integer(Tabix) && length(Tabix)==1 && !is.na(Tabix)){
               p(
                  "For convenience a ",
                  tags$a("Tabix", href="https://tabix.io/", target="_blank"),
                  "  interface to ClickHouse is provided here: ",
                  tags$a(
                     sprintf("http://%s:%s", tkcon$chcon@host, Tabix),
                     href=sprintf("http://%s:%s", tkcon$chcon@host, Tabix),
                     target="_blank"
                  )
               )
            }else{
               NULL
            },
            h1("TKCat: Managing Tailored Knowledge Catalogs in R"),
            p("A link to the TKCat vignette will be provided here"),
            p("The TKCat R library depends on:"),
            do.call(tags$ul, lapply(
               unlist(c(
                  strsplit(packageDescription("TKCat")$Depends, split=",\n"),
                  strsplit(packageDescription("TKCat")$Imports, split=",\n")
               )),
               function(x){
                  tags$li(x)
               }
            )),
            p("The ReDaMoR package is not public yet"),
            h2("Functions for internal MDB (Modelled DataBases)"),
            h3("Reading an internal MDB"),
            tags$ul(
               tags$li(tags$code(
                  'mdb <- readInternalMDB(',
                     'dataModel=readSQLDataModel(SQLFILE),',
                     'descriptionFile=DESCRIPTION.json),',
                     'directory=DATADIRECTORY',
                  ')'
               )),
               tags$li(tags$code(
                  'colMb <- readCollectionMembers(JSONFILE)'
               )),
               tags$li(tags$code(
                  'collectionMembers(mdb) <- colMb'
               ))
            ),
            h3("Exploring"),
            tags$ul(
               tags$li(tags$code('plot(mdb)')),
               tags$li(tags$code('dbInfo(mdb)')),
               tags$li(tags$code('dataModel(mdb)')),
               tags$li(tags$code('dataTables(mdb)')),
               tags$li(tags$code('collectionMembers(mdb)'))
            ),
            h2("Functions for ClickHouse MDB"),
            h3("Connecting to a ClickHouse TKCat instance"),
            tags$ul(
               tags$li(tags$code(
                  'tkcon <- chTKCat(host=HOST, port=NATIVEPORT)'
               )),
               tags$li(tags$code(
                  'mdb <- chMDB(tkcon=tkcon, dbName=DBOFINTEREST)'
               ))
            ),
            h3("Exploring"),
            tags$ul(
               tags$li(tags$code('explore(tkcon)')),
               tags$li(tags$code('listMDBs(tkcon)')),
               tags$li(tags$code('collectionMembers(tkcon)')),
               tags$li(tags$code('plot(mdb)')),
               tags$li(tags$code('dbInfo(mdb)')),
               tags$li(tags$code('dataModel(mdb)')),
               tags$li(tags$code('dataTables(mdb)')),
               tags$li(tags$code('collectionMembers(mdb)'))
            )
         )
      )
      
      
   ))
}

buildServer <- function(tkcon){
   
   tkcon <- tkcon
   
   function(input, output, session) {
      
      ## mdbList ----
      mdbList <- listMDBs(tkcon)
      output$mdbList <- renderDataTable({
         toShow <- mdbList %>%
            select(name, title) %>%
            rename("Database"="name", "Title"="title")
         cm <- collectionMembers(tkcon) %>%
            select(collection, resource) %>%
            unique() %>%
            group_by(resource) %>%
            summarise(collection=paste(sort(collection), collapse=", ")) %>%
            rename("Collections"="collection")
         toShow <- left_join(toShow, cm, by=c("Database"="resource"))
         datatable(
            toShow,
            rownames=FALSE,
            filter="top",
            selection = 'single',
            extensions='Scroller',
            options = list(
               deferRender = TRUE,
               scrollY = "65vh",
               scroller = TRUE,
               dom=c("ti")
            )
         )
      })
      
      observe({
         s <- input$mdbList_rows_selected
         n <- mdbList$name[s]
         if(length(n)==0 || n==""){
            session$sendCustomMessage('hideNavs', 'Data model')
         }else{
            session$sendCustomMessage('showNavs', 'Data model')
            session$sendCustomMessage(
               'setTabLabel',
               list(
                  name="Data model",
                  label=sprintf("%s data model", n)
               )
            )
         }
      })
      
      ## dbInfo ----
      output$dbInfo <- renderUI({
         s <- input$mdbList_rows_selected
         validate(need(s, FALSE))
         dbi <- dbInfo(chMDB(tkcon, mdbList$name[s]))
         list(
            h3(dbi$name),
            do.call(tags$ul, lapply(
               setdiff(names(dbi), "name"),
               function(n){
                  tags$li(tags$span(
                     tags$strong(paste0(n, ":")),
                     if(n=="url"){
                        tags$a(dbi[[n]], href=dbi[[n]], target="_blank")
                     }else if(is.numeric(dbi[[n]])){
                        format(dbi[[n]], big.mark=",")
                     }else{
                        dbi[[n]]
                     }
                  ))
               }
            ))
         )
      })
      
      ## dataModel ----
      output$dataModel <- renderVisNetwork({
         s <- input$mdbList_rows_selected
         validate(need(s, FALSE))
         dm <- dataModel(chMDB(tkcon, mdbList$name[s]))
         plot(dm) %>%
            visOptions(
               nodesIdSelection=list(enabled=TRUE, useLabels=FALSE),
               highlightNearest=TRUE, height="100%"
            ) 
      })
      
      ## collectionInfo ----
      output$collectionInfo <- renderUI({
         s <- input$mdbList_rows_selected
         validate(need(s, FALSE))
         list(
            h3("Collection members"),
            dataTableOutput("colMembers")
         )
      })
      
      ## colMembers ----
      output$colMembers <- renderDataTable({
         s <- input$mdbList_rows_selected
         validate(need(s, FALSE))
         cm <- collectionMembers(chMDB(tkcon, mdbList$name[s]))
         datatable(
            cm %>% select(-resource),
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
      
      observe({
         cs <- input$colMembers_rows_selected
         validate(need(cs, FALSE))
         s <- isolate(input$mdbList_rows_selected)
         validate(need(s, FALSE))
         cmt <- collectionMembers(chMDB(tkcon, mdbList$name[s])) %>%
            slice(cs) %>%
            pull(table)
         visNetworkProxy("dataModel") %>%
            visSelectNodes(id=cmt)
      })
      
      ## tableInfo ----
      output$tableInfo <- renderUI({
         st <- input$dataModel_selected
         s <- isolate(input$mdbList_rows_selected)
         validate(need(st, FALSE))
         validate(need(s, FALSE))
         dm <- dataModel(chMDB(tkcon, mdbList$name[s]))
         list(
            h3(st),
            tags$ul(
               tags$li(
                  tags$strong("Records"),
                  ":",
                  suppressWarnings(dbGetQuery(
                     tkcon$chcon,
                     sprintf(
                        "SELECT count() from `%s`.`%s`",
                        mdbList$name[s],
                        st
                     )
                  ))[,1] %>% format(big.mark=",")
               )
            ),
            dataTableOutput("dataSample"),
            downloadButton("downloadData", "Download")
         )
      })
      
      ## dataSample ----
      output$dataSample <- renderDataTable({
         st <- input$dataModel_selected
         s <- isolate(input$mdbList_rows_selected)
         validate(need(st, FALSE))
         validate(need(s, FALSE))
         dm <- dataModel(chMDB(tkcon, mdbList$name[s]))
         toShow <- suppressWarnings(dbGetQuery(
            tkcon$chcon,
            sprintf(
               "SELECT * from `%s`.`%s` limit 100",
               mdbList$name[s],
               st
            )
         ))
         if(object.size(toShow) > 2^19){
            toShow <- toShow[
               1:max(c(1, ceiling(nrow(toShow)*(2^19/object.size(toShow))))),
            ]
         }
         datatable(
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
      
      ## downloadData ----
      output$downloadData <- downloadHandler(
         filename = function() {
            st <- input$dataModel_selected
            s <- isolate(input$mdbList_rows_selected)
            validate(need(st, FALSE))
            validate(need(s, FALSE))
            paste0(st, ".txt")
         },
         content = function(file) {
            st <- input$dataModel_selected
            s <- isolate(input$mdbList_rows_selected)
            validate(need(st, FALSE))
            validate(need(s, FALSE))
            mdb <- chMDB(tkcon, mdbList$name[s])
            write_tsv(mdb[[st]], file)
         }
      )
      
      
   }
}

###############################################################################@
#' Explore a chTKCat
#' 
#' @importFrom base64enc base64encode
#' @importFrom ReDaMoR explore
#' @export
#' 
explore.chTKCat <- function(x, Tabix=NA, ...){
   
   shinyApp(ui=buildUi(x, Tabix=Tabix), server=buildServer(x))
   # runGadget(
   #    ui, server,
   #    viewer = dialogViewer("Explore chTKCat", height=900, width=1600)
   # )
   
}
