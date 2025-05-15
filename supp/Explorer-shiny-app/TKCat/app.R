library(TKCat)

x <- chTKCat(
   host = "localhost",
   port = 9111,
   user = "default",
   password = NA
)

subSetSize <- 100
host <- x$chcon@host
download <- FALSE
workers <- 4
userManager <- "https://192.168.170.198/shiny/pgodard/AP-RD-TKCat-User-management/"
title <- NULL
skinColors <- c("blue", "yellow")
logoDiv <- TKCat:::TKCAT_LOGO_DIV
tabTitle <- "AP TKCat"
tabIcon <- 'www/TKCat-small.png'
rDirs <- NULL

stopifnot(
   is.logical(download), length(download)==1, !is.na(download),
   is.character(skinColors), length(skinColors)>0, all(!is.na(skinColors))
)
if(length(skinColors)==1){
   skinColors <- rep(skinColors, 2)
}
skinColors <- skinColors[1:2]
if(!is.null(userManager)){
   stopifnot(
      is.character(userManager), length(userManager)==1, !is.na(userManager)
   )
}
if(download){
   ddir <- tempfile()
   dir.create(ddir)
   oplan <- future::plan(
      future::multisession, workers=workers
   )
}else{
   ddir <- NULL
}
on.exit({
   if(interactive()){
      warning(
         "Disconnected from clickhouse database. ",
         "Use the db_reconnect(x) function to reconnect x."
      )
   }
}, add=TRUE)
shiny::shinyApp(
   ui=TKCat:::.build_etkc_ui.chTKCat(
      x=x, ddir=ddir, userManager=!is.null(userManager),
      logoDiv=logoDiv, rDirs=rDirs,
      tabTitle=tabTitle, tabIcon=tabIcon
   ),
   server=TKCat:::.build_etkc_server.chTKCat(
      x=x,
      subSetSize=subSetSize,
      host=host,
      ddir=ddir,
      userManager=userManager,
      title=title,
      skinColors=skinColors
   ),
   enableBookmarking="url",
   onStart=function(){
      shiny::onStop(function(){
         unlink(ddir, recursive=TRUE, force=TRUE)
         if(exists("oplan")){
            future::plan(oplan)
         }
      })
   }
)
