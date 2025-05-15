library(TKCat)

pwdFile <- "~/etc/tkcat_techadmin_pwd.txt"

x <- chTKCat(
   host = "localhost",
   port = 9111,
   user = "techadmin",
   password = readLines(pwdFile)
)
TKCat:::.etkc_add_resources(ddir=NULL)

shiny::shinyApp(
   
   ui=function(req){
      shiny::fluidPage(
         shiny::tags$script("
               Shiny.addCustomMessageHandler(
                  'current_password',
                  function(value) {
                     Shiny.setInputValue('current_password', value);
                  }
               );
               $(document).keyup(function(event) {
                  if(event.key == 'Enter'){
                     if($('#update')[0]) {
                        $('#update').click();
                     }else{
                        if($('#connect')[0]) {
                           $('#connect').click();
                        }
                     }
                  }
               });
            "),
         shiny::fluidRow(
            shiny::column(
               6,
               shiny::textInput(
                  "login", "User name"
               ),
               shiny::passwordInput(
                  "current_password", "Current password"
               ),
               shiny::actionButton(
                  "connect", "Connect to check and modify settings"
               )
            ),
            shiny::column(
               6,
               shiny::uiOutput("userInfo")
            )
         )
      )
   },
   
   server=function(input, output, session){
      
      ## Manage DB connection ----
      if(!is.null(pwdFile)){
         db_reconnect(x, password=readLines(pwdFile))
         shiny::onSessionEnded(function() db_disconnect(x))
      }
      shiny::onStop(function() db_disconnect(x))
      userInstance <- shiny::reactiveVal()
      
      ## Connect as user ----
      shiny::observeEvent(input$connect, {
         l <- shiny::isolate(input$login)
         p <- shiny::isolate(input$current_password)
         shiny::req(l)
         userInstance(try(
            chTKCat(
               host=x$chcon@host,
               port=x$chcon@port,
               host_path=x$chcon@host_path,
               user=l, password=p,
               settings=x$settings,
               https=x$chcon@https,
               ssl_verifypeer=x$chcon@ssl_verifypeer,
               reset_handle=x$chcon@reset_handle
            ),
            silent=TRUE
         ))
         session$sendCustomMessage("current_password", 'null')
      })
      
      ## Display user settings ----
      newpwd <- shiny::reactiveValues(
         pwd=character(0),
         cpwd=character(0),
         valid=FALSE
      )
      output$userInfo <- shiny::renderUI({
         ui <- userInstance()
         shiny::req(!is.null(ui))
         if(inherits(ui, "try-error")){
            return(shiny::fluidRow(shiny::column(
               12,
               shiny::p(shiny::strong("Bad credentials", style="color:red;"))
            )))
         }
         if(ui$chcon@user=="default"){
            return(shiny::fluidRow(shiny::column(
               12,
               shiny::p(shiny::strong(
                  "The default user cannot be modified",
                  style="color:red;"
               ))
            )))
         }
         if(ui$chcon@user==x$chcon@user){
            return(shiny::fluidRow(shiny::column(
               12,
               shiny::p(shiny::strong(
                  sprintf("The %s user cannot be modified", x$chcon@user),
                  style="color:red;"
               ))
            )))
         }
         ut <- list_chTKCat_users(x)
         cur_cont <- ut %>%
            dplyr::filter(.data$login==ui$chcon@user) %>%
            dplyr::pull("contact")
         toRet <- shiny::fluidRow(shiny::column(
            12,
            shiny::textInput(
               "contact", "Contact", placeholder=cur_cont
            ),
            shiny::passwordInput(
               "new_password", "New password (8 charcters or more)"
            ),
            shiny::passwordInput(
               "conf_password", "Confirm new password"
            ),
            shiny::uiOutput("pwdCheck"),
            shiny::actionButton(
               "update", "Update settings"
            ),
            shiny::uiOutput("updInfo")
         ))
         return(toRet)
      })
      output$pwdCheck <- shiny::renderUI({
         nclim <- 8
         pwd <- input$new_password
         cpwd <- input$conf_password
         valid <- nchar(pwd) >= nclim &
            pwd==cpwd
         newpwd$pwd <- pwd
         newpwd$cpwd <- cpwd
         newpwd$valid <- valid
         if(!valid){
            if(nchar(pwd)==0){
               return()
            }else{
               if(nchar(pwd) < nclim){
                  return(shiny::strong(
                     "8 characters or more are required",
                     style="color:red;"
                  ))
               }
               if(pwd != cpwd){
                  return(shiny::strong(
                     "Passwords are differents",
                     style="color:red;"
                  ))
               }
            }
         }else{
            return(shiny::strong(
               "Valid new password, ready for update",
               style="color:green;"
            ))
         }
      })
      
      ## Update settings ----
      output$updInfo <- shiny::renderUI({
         shiny::req(input$update)
         ui <- shiny::isolate(userInstance())
         shiny::req(ui)
         vui <- try(check_chTKCat(ui), silent=TRUE)
         if(inherits(vui, "try-error")){
            userInstance(vui)
            return()
         }
         newCont <- shiny::isolate(input$contact)
         toRet <- shiny::tagList()
         if(!is.na(newCont) & length(newCont) & nchar(newCont)>0){
            update_chTKCat_user(x, login=ui$chcon@user, contact=newCont)
            toRet <- c(
               toRet,
               shiny::tagList(shiny::p(shiny::strong(
                  "- Contact information has been updated",
                  style="color:green;"
               )))
            )
         }
         if(shiny::isolate(newpwd$valid)){
            pwd <- shiny::isolate(newpwd$pwd)
            change_chTKCat_password(x, login=ui$chcon@user, password=pwd)
            db_disconnect(ui)
            newpwd$pwd <- ""
            newpwd$cpwd <- ""
            newpwd$valid <- FALSE
            toRet <- c(
               toRet,
               shiny::tagList(shiny::p(shiny::strong(
                  paste(
                     "- Password has been updated",
                     "(you need to reconnect to make other changes)"
                  ),
                  style="color:green;"
               )))
            )
         }
         return(toRet)
      })
      
   }
)
