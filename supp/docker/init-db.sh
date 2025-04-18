#!/bin/bash

set -e

CONTAINER_ALREADY_STARTED="CONTAINER_ALREADY_STARTED_PLACEHOLDER"
if [ ! -e $CONTAINER_ALREADY_STARTED ]; then
    touch $CONTAINER_ALREADY_STARTED
    echo "-- First container startup --"

    cp /users-1.xml /etc/clickhouse-server/users.xml
    cp /default-user-1.xml /etc/clickhouse-server/users.d/default-user.xml
    sleep 2

    clickhouse client -q "CREATE TABLE default.System (name String, instance String, version String, contact String, path String) ENGINE = MergeTree() ORDER BY tuple();"
    clickhouse client -q "CREATE TABLE default.Collections (title String, description String, json String) ENGINE = MergeTree() ORDER BY (title);"
    clickhouse client -q "CREATE TABLE default.Users (login String, contact Nullable(String), admin UInt8, provider UInt8) ENGINE = MergeTree() ORDER BY (login);"
    clickhouse client -q "INSERT INTO default.System (name, instance, version, contact, path) VALUES ('chTKCat', '${TKCAT_INSTANCE}', '${TKCAT_VERSION}', '${TKCAT_CONTACT}', '');"
    clickhouse client -q "CREATE USER '${TKCAT_ADMIN}' IDENTIFIED WITH no_password;"
    clickhouse client -q "GRANT ALL ON *.* TO '${TKCAT_ADMIN}' WITH GRANT OPTION;"
    clickhouse client -q "INSERT INTO default.Users (login, admin, provider) VALUES ('${TKCAT_ADMIN}', 1, 1);"
    clickhouse client -q "INSERT INTO default.Users (login, admin, provider) VALUES ('default', 0, 0) ;"

   if [ -e "/etc/clickhouse-server/users.d/default-user.xml" ]; then
      rm -rf "/etc/clickhouse-server/users.d/default-user.xml"
   fi
   cp /users-2.xml /etc/clickhouse-server/users.xml
   cp /remove_default_user.yaml /etc/clickhouse-server/users.d/remove_default_user.yaml
   sleep 2

   clickhouse client -u ${TKCAT_ADMIN} -n <<-EOSQL
       CREATE USER default IDENTIFIED WITH no_password;
       REVOKE ALL ON *.* FROM default;
       GRANT SHOW DATABASES ON *.* TO default;
       GRANT SHOW TABLES ON *.* TO default;
       GRANT SHOW COLUMNS ON *.* TO default;
       GRANT SELECT(name, instance, version, contact) ON default.System TO default;
       GRANT SELECT ON default.Collections TO default;
       GRANT SELECT(login, admin, provider) ON default.Users TO default;
       GRANT SELECT(value, name) ON system.build_options TO default;
EOSQL
    
else
    echo "-- Not first container startup --"
fi
