library(here)
conff <- here("ClickHouse/S01-install-clickhouse-docker.sh")
sconf <- do.call(c, lapply(
   strsplit(
      sub("[[:blank:];]*", "", sub(
         "^export[[:blank:]]*", "",
         grep(
            "^export[[:blank:]]",
            readLines(conff),
            value=TRUE
         )
      )),
      split="="
   ),
   function(x){
      return(as.list(structure(x[2],.Names=x[1])))
   }
))


library(TKCat)
tkcon <- chTKCat(
   host="localhost",
   port=as.numeric(sconf$TKCAT_NAT_PORT)
)
try(tkcon <- TKCat:::init_chTKCat(
   tkcon,
   instance="UCB - TBN",
   version=as.character(Sys.Date())
))
