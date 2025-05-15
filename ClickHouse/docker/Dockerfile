FROM clickhouse/clickhouse-server:25.3.2.39
COPY users.xml /users.xml
COPY default-user.xml /default-user.xml
COPY remove_default_user.yaml /remove_default_user.yaml
COPY init-db.sh /docker-entrypoint-initdb.d/init-db.sh
