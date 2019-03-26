#!/bin/sh

#####################################################
## REQUIREMENTS

# - Must be run as root: NOT ANYMORE because giving write access to everyone ==> to be improved

#####################################################
## Configuration

export TKCAT_HOME=/data/pgodard/Projects/UCB_NM_TKcat
mkdir -p $TKCAT_HOME

export TKCAT_NAT_PORT=9101
export TKCAT_HTTP_PORT=9111

# SRC_DIR=ClickHouse-Files
CH_VERSION=19.1.14

TKCAT_DATA=$TKCAT_HOME/data
mkdir -p $TKCAT_DATA
TKCAT_CONF=$TKCAT_HOME/conf
mkdir -p $TKCAT_CONF
TKCAT_CONFIG=$TKCAT_CONF/config.xml
TKCAT_USERS=$TKCAT_CONF/users.xml
# cp $SRC_DIR/config.xml $DS_CONFIG
# cp $SRC_DIR/users.xml $DS_USERS
chmod -R a+rwx $TKCAT_HOME

#####################################################
## Instantiate clickhouse server

docker run -d \
	--name ucb_nm_tkcat \
	--ulimit nofile=262144:262144 \
	--volume $TKCAT_DATA:/var/lib/clickhouse \
	--publish=$TKCAT_HTTP_PORT:8123 --publish=$TKCAT_NAT_PORT:9000 \
	--restart=always \
	yandex/clickhouse-server:$CH_VERSION

	# --volume $DS_CONFIG:/etc/clickhouse-server/config.xml \
	# --volume $DS_USERS:/etc/clickhouse-server/users.xml \
	
