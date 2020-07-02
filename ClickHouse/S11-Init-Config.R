conff <- "S01-install-clickhouse-docker.sh"
sconf <- do.call(c, lapply(
   strsplit(
      gsub("[[:blank:];]*", "", sub("^export[[:blank:]]*", "", sub("[#].*$", "",
         grep(
            "^export[[:blank:]]",
            readLines(conff),
            value=TRUE
         )
      ))),
      split="="
   ),
   function(x){
      return(as.list(structure(x[2],.Names=x[1])))
   }
))

library(devTKCat)
k <- chTKCat(
   host="localhost",
   port=as.numeric(sconf$TKCAT_NAT_PORT),
   http=as.numeric(sconf$TKCAT_HTTP_PORT),
   password=NA
)
k <- devTKCat:::init_chTKCat(
   k,
   instance="UCB - TBN",
   version=as.character(Sys.Date()),
   path=sconf$TKCAT_HOME,
   login="pgodard",
   contact="Patrice Godard <patrice.godard@ucb.com>"
)
