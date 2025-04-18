#!/bins/bash

##################################################################
## Customize parameters
IMAGE_BASE_NAME="test-tkcat"
TKCAT_INSTANCE="test_tkcat"
TKCAT_VERSION="2025.04.18"
TKCAT_ADMIN="pgodard@domain.com"
TKCAT_CONTACT="Admin User <pgodard@domain.com>"

## Don't forget to create a password for the TKCAT_ADMIN user
## In R: ?TKCat::change_chTKCat_password()

TKCAT_NAT_PORT=9103 #9101
TKCAT_HTTP_PORT=9113 #9111

TKCAT_HOSTDIR=/mnt/data1/pgodard/Services-test

######################
## Automatic parameters
IMAGE=${IMAGE_BASE_NAME}:${TKCAT_VERSION}

CONTAINER=${TKCAT_INSTANCE}_${TKCAT_VERSION}
TKCAT_HOME=${TKCAT_HOSTDIR}/${CONTAINER}

TKCAT_DATA=${TKCAT_HOME}/data
TKCAT_LOG=${TKCAT_HOME}/log

if [ -e "$TKCAT_HOME" ]; then
    echo "File $TKCAT_HOME already exists. Exiting without doing anything."
    exit 1
fi

##################################################################

######################
docker build -t ${IMAGE} .

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
	--publish=${TKCAT_NAT_PORT}:9000 \
   --publish=${TKCAT_HTTP_PORT}:8123 \
	--volume ${TKCAT_DATA}:/var/lib/clickhouse \
	--volume ${TKCAT_LOG}:/var/log/clickhouse-server \
	-e TKCAT_INSTANCE="${TKCAT_INSTANCE}" \
	-e TKCAT_VERSION="${TKCAT_VERSION}" \
	-e TKCAT_ADMIN="${TKCAT_ADMIN}" \
	-e TKCAT_CONTACT="${TKCAT_CONTACT}" \
	--restart=always \
	${IMAGE}
