#!/bins/bash

######################
IMAGE=tkcat:2022.12.04

docker build -t ${IMAGE} .

######################
CONTAINER=tkcat
TKCAT_NAT_PORT=9000
TKCAT_HTTP_PORT=8123

TKCAT_HOME=/data/TKCat
TKCAT_DATA=${TKCAT_HOME}/data
TKCAT_LOG=${TKCAT_HOME}/log

######################
mkdir -p ${TKCAT_HOME}
chmod a+rwx ${TKCAT_HOME}
mkdir -p ${TKCAT_DATA}
chmod a+rwx ${TKCAT_DATA}
mkdir -p ${TKCAT_LOG}
chmod a+rwx ${TKCAT_LOG}

######################
docker run -d \
	--name ${CONTAINER} \
	--ulimit nofile=262144:262144 \
	--network=host \
	--volume ${TKCAT_DATA}:/var/lib/clickhouse \
	--volume ${TKCAT_LOG}:/var/log/clickhouse-server \
	--restart=always \
	${IMAGE}

	

## Filtered by docker somehow ? ==> use of --network=host
#	--publish=${TKCAT_NAT_PORT}:9000 \
#	--publish=${TKCAT_HTTP_PORT}:8123 \
