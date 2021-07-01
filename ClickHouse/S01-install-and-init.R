library(TKCat)

###############################################################################@
## Configuration ----

TKCAT_HOME <- "/data/pgodard/Projects/TKCat_UCB_TBN"
# TKCAT_HOME <- "/data/pgodard/Projects/TKCat_Test"
TKCAT_NAT_PORT <- 9101
# TKCAT_NAT_PORT <- 9301
TKCAT_HTTP_PORT <- 9111
# TKCAT_HTTP_PORT <- 9311

USERFILE <- system.file(
   "ClickHouse/users.xml",
   package="TKCat"
)
CH_VERSION <- "21.4.6"

CONTAINER <- "ucb_tbn_tkcat"
# CONTAINER <- "test_tkcat"
# INSTANCE_NAME <- "UCB - TBN"
INSTANCE_NAME <- "Test"
ADMIN <- "pgodard"
CONTACT <- "Patrice Godard <patrice.godard@ucb.com>"

###############################################################################@
## Check ----
if(!file.exists(USERFILE)){
   stop(sprintf("%s does not exist", USERFILE))
}
if(file.exists(TKCAT_HOME)){
   stop(sprintf("%s already exists", TKCAT_HOME))
}

###############################################################################@
## Prepare infrastructure ----
dir.create(TKCAT_HOME, recursive=TRUE, mode="0777")
Sys.chmod(TKCAT_HOME, mode="777", use_umask=FALSE)
TKCAT_DATA <- file.path(TKCAT_HOME, "data")
dir.create(TKCAT_DATA, recursive=TRUE)
Sys.chmod(TKCAT_DATA, mode="777", use_umask=FALSE)
TKCAT_CONF <- file.path(TKCAT_HOME, "conf")
dir.create(TKCAT_CONF, recursive=TRUE)
Sys.chmod(TKCAT_CONF, mode="777", use_umask=FALSE)
TKCAT_USERS <- file.path(TKCAT_CONF, "users.xml")
file.copy(USERFILE, TKCAT_USERS)
Sys.chmod(TKCAT_USERS, mode="777", use_umask=FALSE)

###############################################################################@
## Run the docker container ----
system(paste(
   'docker run -d',
   sprintf('--name %s', CONTAINER),
   '--ulimit nofile=262144:262144',
   sprintf('--volume %s:/var/lib/clickhouse', TKCAT_DATA),
   sprintf('--publish=%s:9000', TKCAT_NAT_PORT),
   sprintf('--publish=%s:8123', TKCAT_HTTP_PORT),
   sprintf('--volume %s:/etc/clickhouse-server/users.xml', TKCAT_USERS),
   '--restart=always',
   sprintf('yandex/clickhouse-server:%s', CH_VERSION)
))

###############################################################################@
## Initiate TKCat instance ----
k <- chTKCat(
   host="localhost",
   port=TKCAT_NAT_PORT,
   http=TKCAT_HTTP_PORT,
   password=NA
)
k <- TKCat:::init_chTKCat(
   k,
   instance=INSTANCE_NAME,
   version=as.character(Sys.Date()),
   path=TKCAT_HOME,
   login=ADMIN,
   contact=CONTACT
) ## The function will ask to setup a password here.
add_chTKCat_collection(k, "BE")
add_chTKCat_collection(k, "Condition")
