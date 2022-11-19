#!/bin/bash

set -e

cp /users-1.xml /etc/clickhouse-server/users.xml
sleep 2

clickhouse client -n <<-EOSQL
	CREATE TABLE default.System (name String, instance String, version String, contact String, path String) ENGINE = MergeTree() ORDER BY tuple();
	CREATE TABLE default.Collections (title String, description String, json String) ENGINE = MergeTree() ORDER BY (title);
	CREATE TABLE default.Users (login String, contact Nullable(String), admin UInt8, provider UInt8) ENGINE = MergeTree() ORDER BY (login);
	INSERT INTO default.System (name, instance, version, contact, path) VALUES ('chTKCat', 'UCB - IT', '2022-11-17', 'Patrice Godard <patrice.godard@ucb.com>', '');
	CREATE USER techadmin IDENTIFIED BY 'a_password_TO_CHANGE';
	GRANT ALL ON *.* TO techadmin WITH GRANT OPTION;
	INSERT INTO default.Users (login, admin, provider) VALUES ('techadmin', 1, 1);
EOSQL

cp /users-2.xml /etc/clickhouse-server/users.xml
sleep 2

clickhouse client -u techadmin --password a_password_TO_CHANGE -n <<-EOSQL
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
