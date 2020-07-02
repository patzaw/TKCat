library(DBI)
library(RClickhouse)
library(tidyverse)
packageDescription("RClickhouse")$Version

dbDisconnect(con)
con <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", port=9201)
a <- dbGetQuery(con, "select * from system.settings") %>% 
   as_tibble()
a %>% filter(str_detect(name, "max_query"))

dbGetQuery(con , "show grants")

dbGetQuery(con , "create role admin")
dbGetQuery(con, "GRANT ALL ON *.* TO admin WITH GRANT OPTION")

dbGetQuery(con, "create user pgodard IDENTIFIED BY 'password'")
dbGetQuery(con, "GRANT admin to pgodard")
dbGetQuery(con, "create user default IDENTIFIED WITH no_password")


dbGetQuery(con, "create user padamin IDENTIFIED BY 'password'")
dbGetQuery(con, "alter user padamin IDENTIFIED BY 'T42&24t'")
dbGetQuery(con, "GRANT alter user on *.* to padamin WITH GRANT OPTION")
dbGetQuery(con, "REVOKE all privileges on *.* from default")


dbGetQuery(con, "create database db1")
dbGetQuery(con, "create role db1_w")
dbGetQuery(con, "GRANT ALL ON db1.* TO db1_w")
dbGetQuery(con, "CREATE USER db1_u1 IDENTIFIED WITH no_password")
dbGetQuery(con, "GRANT db1_w TO db1_u1")
##
dbGetQuery(con, "create role db1_r")
dbGetQuery(con, "GRANT SELECT ON db1.* TO db1_r")
dbGetQuery(con, "GRANT SHOW ON db1.* TO db1_r")
dbGetQuery(con, "CREATE USER db1_u2 IDENTIFIED WITH no_password")
dbGetQuery(con, "GRANT db1_r TO db1_u2")

dbGetQuery(con, "GRANT ALL ON db1.* TO db1_r")
dbGetQuery(con , "show grants for db1_r")

dbDisconnect(con)
con <- DBI::dbConnect(RClickhouse::clickhouse(), host="localhost", port=9201, user="user1")
dbGetQuery(con, "select * from system.tables where database<>'system'")

# test <- tibble("abc"=1:3)
# TKCat:::writeMergeTree(con, "db1", "test", test, c("abc"="integer"))

dbDisconnect(con)
con <- DBI::dbConnect(
   RClickhouse::clickhouse(), host="localhost", port=9201,
   user="pgodard", password="admin"
)

library(ReDaMoR)
TKCat:::writeMergeTree(con, "default", "d", d, rtypes=c("a"="integer", "b"="integer"))


dbSendQuery(con, 'create row policy dpgorr on d as restrictive for select to pgodard')
dbSendQuery(con, 'create row policy dpgo on d as permissive using b>=18 to pgodard')
dbSendQuery(con, 'create row policy dpgor on d as permissive for select using 1 to pgodard')
dbSendQuery(con, 'alter table d delete where b=12')


dbDisconnect(con2)
con2 <- DBI::dbConnect(RClickhouse::clickhouse(), "localhost", 9101)
db <- dbGetQuery(con2, "select * from system.tables")
a <- c()
for(x in 1:nrow(db))
   a <- bind_rows(a, dbGetQuery(
      con2, sprintf("select * from system.tables where database='%s' and name='%s'", db[x, 1, drop=T], db[x, 2, drop=T])
   ))
