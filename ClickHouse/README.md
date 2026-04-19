## Create a new ClickHouse instance for TKCat

1. Go to docker-init
2. Set your version of clickhouse/clickhouse-server in the **Dockerfile**
3. Set the following TKCat parameters in the **launch-tkcat-instance.sh** file:
   - `TKCAT_VERSION`
   - `IMAGE_BASE_NAME`
   - `TKCAT_INSTANCE`
   - `TKCAT_ADMIN`
   - `TKCAT_CONTACT`
   - `TKCAT_NAT_PORT`
   - `TKCAT_HTTP_PORT`
   - `TKCAT_HOSTDIR`
4. Run `/bin/bash launch-tkcat-instance.sh`

## Upgrage ClickHouse instance

1. Go to docker-upgrade
2. Set your version of clickhouse/clickhouse-server in
the **upgrade-tkcat-instance.sh** file:
   - `CH_VERSION`
3. Set the following TKCat parameters in the **upgrade-tkcat-instance.sh** file:
   - `TKCAT_VERSION`
   - `TKCAT_INSTANCE`
   - `TKCAT_NAT_PORT`
   - `TKCAT_HTTP_PORT`
   - `TKCAT_HOSTDIR`
4. Run `/bin/bash upgrade-tkcat-instance.sh`
