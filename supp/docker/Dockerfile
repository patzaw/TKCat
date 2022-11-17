FROM clickhouse/clickhouse-server:22.10.2.11-alpine
COPY users-1.xml /users-1.xml
COPY users-2.xml /users-2.xml
COPY init-db.sh /docker-entrypoint-initdb.d/init-db.sh
