library(devTKCat)

###############################################################################@
## Configuration ----

TKCAT_HOME <- "/data/pgodard/Projects/TKCat_UCB_TBN"
TKCAT_NAT_PORT <- 9201 # mv back to 9101 when tests are finished
TKCAT_HTTP_PORT <- 9211 # mv back to 9111 when tests are finished

SRC_DIR <- here::here("ClickHouse", "ClickHouse-Files")
CH_VERSION <- "20.8.2.3"

CONTAINER <- "ucb_tbn_tkcat"
INSTANCE_NAME <- "UCB - TBN"
ADMIN <- "pgodard"
CONTACT <- "Patrice Godard <patrice.godard@ucb.com>"

###############################################################################@
## Check ----
if(!file.exists(SRC_DIR)){
   stop("The ClickHouse-Files directory does not exist")
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
TKCAT_CONFIG <- file.path(TKCAT_CONF, "config.xml")
TKCAT_USERS <- file.path(TKCAT_CONF, "users.xml")
file.copy(file.path(SRC_DIR, "config.xml"), TKCAT_CONFIG)
Sys.chmod(TKCAT_CONFIG, mode="777", use_umask=FALSE)
file.copy(file.path(SRC_DIR, "users-init.xml"), TKCAT_USERS)
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
   sprintf('--volume %s:/etc/clickhouse-server/config.xml', TKCAT_CONFIG),
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
k <- devTKCat:::init_chTKCat(
   k,
   instance=INSTANCE_NAME,
   version=as.character(Sys.Date()),
   path=TKCAT_HOME,
   login=ADMIN,
   contact=CONTACT
)
add_chTKCat_collection(k, "BE")
add_chTKCat_collection(k, "Condition")
