library(TKCat)

###############################################################################@
## Configuration ----

USERFILE <- system.file(
   "ClickHouse/users.xml",
   package="TKCat"
)
CONFIGFILE <- system.file(
   "ClickHouse/config.xml",
   package="TKCat"
)

CH_VERSION <- "22.2.3.5"

TKCAT_HOME <- "/data/pgodard/Projects/TKCat_Test" # TKCat_UCB_TBN
TKCAT_NAT_PORT <- 9101
TKCAT_HTTP_PORT <- 9111
TKCAT_NATS_PORT <- 9121
TKCAT_HTTPS_PORT <- 9131
CONTAINER <- "tkcat_test"                         # "ucb_tbn_tkcat"
INSTANCE_NAME <- "Test"                           # "UCB - TBN"

ADMIN <- "pgodard"
CONTACT <- "Patrice Godard <patrice.godard@ucb.com>"

###############################################################################@
## Check ----
if(!file.exists(USERFILE)){
   stop(sprintf("%s does not exist", USERFILE))
}
if(!file.exists(CONFIGFILE)){
   stop(sprintf("%s does not exist", CONFIGFILE))
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

TKCAT_LOG <- file.path(TKCAT_HOME, "log")
dir.create(TKCAT_LOG, recursive=TRUE)
Sys.chmod(TKCAT_LOG, mode="777", use_umask=FALSE)

TKCAT_CONF <- file.path(TKCAT_HOME, "conf")
dir.create(TKCAT_CONF, recursive=TRUE)
Sys.chmod(TKCAT_CONF, mode="777", use_umask=FALSE)

TKCAT_USERS <- file.path(TKCAT_CONF, "users.xml")
file.copy(USERFILE, TKCAT_USERS)
Sys.chmod(TKCAT_USERS, mode="777", use_umask=FALSE)

TKCAT_CONFIG <- file.path(TKCAT_CONF, "config.xml")
file.copy(CONFIGFILE, TKCAT_CONFIG)
Sys.chmod(TKCAT_CONFIG, mode="777", use_umask=FALSE)

### SSL certificate : run in a terminal ----
TKCAT_KEY <- file.path(TKCAT_CONF, "server.key")
TKCAT_CRT <- file.path(TKCAT_CONF, "server.crt")
cat(sprintf(
   paste(
      'openssl req -subj "/CN=localhost" -new -newkey rsa:2048 -days 365',
      '-nodes -x509 -keyout %s -out %s'
   ),
   TKCAT_KEY, TKCAT_CRT
))
Sys.chmod(TKCAT_KEY, mode="777", use_umask=FALSE)
Sys.chmod(TKCAT_CRT, mode="777", use_umask=FALSE)

TKCAT_PEM <- file.path(TKCAT_CONF, "dhparam.pem")
cat(sprintf(
   'openssl dhparam -out %s 4096',
   TKCAT_PEM
))
Sys.chmod(TKCAT_PEM, mode="777", use_umask=FALSE)


###############################################################################@
## Run the docker container ----
system(paste(
   'docker run -d',
   sprintf('--name %s', CONTAINER),
   '--ulimit nofile=262144:262144',
   sprintf('--volume %s:/var/lib/clickhouse', TKCAT_DATA),
   sprintf('--publish=%s:9000', TKCAT_NAT_PORT),
   sprintf('--publish=%s:8123', TKCAT_HTTP_PORT),
   sprintf('--publish=%s:9440', TKCAT_NATS_PORT),
   sprintf('--publish=%s:8443', TKCAT_HTTPS_PORT),
   sprintf('--volume %s:/etc/clickhouse-server/users.xml', TKCAT_USERS),
   sprintf('--volume %s:/etc/clickhouse-server/config.xml', TKCAT_CONFIG),
   sprintf('--volume %s:/etc/clickhouse-server/server.key', TKCAT_KEY),
   sprintf('--volume %s:/etc/clickhouse-server/server.crt', TKCAT_CRT),
   sprintf('--volume %s:/etc/clickhouse-server/dhparam.pem', TKCAT_PEM),
   sprintf('--volume %s:/var/log/clickhouse-server', TKCAT_LOG),
   '--restart=always',
   sprintf('clickhouse/clickhouse-server:%s', CH_VERSION)
))


###############################################################################@
## Initiate TKCat instance ----
k <- chTKCat(
   host="localhost",
   port=TKCAT_HTTP_PORT,
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
