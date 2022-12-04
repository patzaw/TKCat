#!/bin/bash

set -e

CONTAINER_ALREADY_STARTED="CONTAINER_ALREADY_STARTED_PLACEHOLDER"
if [ ! -e $CONTAINER_ALREADY_STARTED ]; then
    touch $CONTAINER_ALREADY_STARTED
    echo "-- First container startup --"

    cp /users-1.xml /etc/clickhouse-server/users.xml
    sleep 2

    clickhouse client -n <<-EOSQL
        CREATE TABLE default.System (name String, instance String, version String, contact String, path String) ENGINE = MergeTree() ORDER BY tuple();
        CREATE TABLE default.Collections (title String, description String, json String) ENGINE = MergeTree() ORDER BY (title);
        CREATE TABLE default.Users (login String, contact Nullable(String), admin UInt8, provider UInt8) ENGINE = MergeTree() ORDER BY (login);
        INSERT INTO default.System (name, instance, version, contact, path) VALUES ('chTKCat', 'UCB - IT', '2022-12-04', 'Patrice Godard <patrice.godard@ucb.com>', '');
        CREATE USER 'patrice.godard@ucb.com' IDENTIFIED WITH no_password;
        GRANT ALL ON *.* TO 'patrice.godard@ucb.com' WITH GRANT OPTION;
        INSERT INTO default.Users (login, admin, provider) VALUES ('patrice.godard@ucb.com', 1, 1);
EOSQL

    cp /users-2.xml /etc/clickhouse-server/users.xml
    sleep 2

    clickhouse client -u patrice.godard@ucb.com -n <<-EOSQL
        CREATE USER default IDENTIFIED WITH no_password;
        INSERT INTO default.Users (login, admin, provider) VALUES ('default', 0, 0) ;
        REVOKE ALL ON *.* FROM default;
        GRANT SHOW DATABASES ON *.* TO default;
        GRANT SHOW TABLES ON *.* TO default;
        GRANT SHOW COLUMNS ON *.* TO default;
        GRANT SELECT(name, instance, version, contact) ON default.System TO default;
        GRANT SELECT ON default.Collections TO default;
        GRANT SELECT(login, admin, provider) ON default.Users TO default;
EOSQL
    
else
    echo "-- Not first container startup --"
fi
