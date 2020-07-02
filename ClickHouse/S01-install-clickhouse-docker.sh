#!/bin/sh

#####################################################
## Configuration

export TKCAT_HOME=/data/pgodard/Projects/TKCat_UCB_TBN # avoid space characters in the path
mkdir -p $TKCAT_HOME

export TKCAT_NAT_PORT=9201 # mv back to 9101 when tests are finished
export TKCAT_HTTP_PORT=9211 # mv back to 9111 when tests are finished

SRC_DIR=ClickHouse-Files
CH_VERSION=20.4.5.36

TKCAT_DATA=$TKCAT_HOME/data
mkdir -p $TKCAT_DATA
TKCAT_CONF=$TKCAT_HOME/conf
mkdir -p $TKCAT_CONF
TKCAT_CONFIG=$TKCAT_CONF/config.xml
TKCAT_USERS=$TKCAT_CONF/users.xml
cp $SRC_DIR/config.xml $TKCAT_CONFIG
cp $SRC_DIR/users-init.xml $TKCAT_USERS
chmod -R a+rwx $TKCAT_HOME

#####################################################
## Instantiate clickhouse server

docker run -d \
	--name ucb_tbn_tkcat \
	--ulimit nofile=262144:262144 \
	--volume $TKCAT_DATA:/var/lib/clickhouse \
	--publish=$TKCAT_HTTP_PORT:8123 --publish=$TKCAT_NAT_PORT:9000 \
	--volume $TKCAT_CONFIG:/etc/clickhouse-server/config.xml \
	--volume $TKCAT_USERS:/etc/clickhouse-server/users.xml \
	--restart=always \
	yandex/clickhouse-server:$CH_VERSION



	
