FROM clickhouse/clickhouse-server:25.3.2.39
COPY users-1.xml /users-1.xml
COPY users-2.xml /users-2.xml
COPY default-user-1.xml /default-user-1.xml
COPY remove_default_user.yaml /remove_default_user.yaml
COPY init-db.sh /docker-entrypoint-initdb.d/init-db.sh
