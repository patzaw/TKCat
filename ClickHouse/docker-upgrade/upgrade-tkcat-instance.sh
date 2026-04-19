#!/bins/bash

#############################################################
## TURN OFF FORMER CONTAINER BEFORE RUNNING THIS SCRIPT    ##
#############################################################

##################################################################
## Customize parameters
CH_VERSION="26.3.9.8"
TKCAT_VERSION="2026.04.18"
TKCAT_INSTANCE="test_tkcat"

TKCAT_NAT_PORT=9101 #9103
TKCAT_HTTP_PORT=9111 #9113

TKCAT_HOSTDIR=/mnt/data1/pgodard/Services-test

######################
## Automatic parameters
CONTAINER=${TKCAT_INSTANCE}_${TKCAT_VERSION}
TKCAT_HOME=${TKCAT_HOSTDIR}/${CONTAINER}

TKCAT_DATA=${TKCAT_HOME}/data
TKCAT_CONFIG=${TKCAT_HOME}/config
TKCAT_LOG=${TKCAT_HOME}/log

if [ ! -e "$TKCAT_DATA" ]; then
    echo "File $TKCAT_DATA does not exist. Cannot upgrade TKCat ClickHouse."
    exit 1
fi
if [ ! -e "$TKCAT_CONFIG" ]; then
    echo "File $TKCAT_CONFIG does not exist. Cannot upgrade TKCat ClickHouse."
    exit 1
fi
if [ ! -e "$TKCAT_LOG" ]; then
    echo "File $TKCAT_LOG does not exist. Cannot upgrade TKCat ClickHouse."
    exit 1
fi

##################################################################

docker run -d \
	--name ${CONTAINER} \
	--ulimit nofile=262144:262144 \
	--publish=${TKCAT_NAT_PORT}:9000 \
   --publish=${TKCAT_HTTP_PORT}:8123 \
	--volume ${TKCAT_DATA}:/var/lib/clickhouse \
	--volume ${TKCAT_CONFIG}:/etc/clickhouse-server \
	--volume ${TKCAT_LOG}:/var/log/clickhouse-server \
	--restart=always \
	clickhouse/clickhouse-server:${CH_VERSION}
