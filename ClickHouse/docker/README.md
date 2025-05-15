----------------------------------------
# Install ClickHouse, initialize and configure the TKCat instance

1. Update the *Dockerfile* to select the version of ClickHouse to use.

2. Customize and run the following script.

```{sh, eval=FALSE}

sh launch-tkcat-instance.sh

```

----------------------------------------
# Cleaning and removing a TKCat instance

Stop and remove the docker containers.

```{sh, eval=FALSE}

docker stop test_tkcat
docker rm test_tkcat
docker volume prune -f
# Remove the folder with all the data: `$TKCAT_HOME`.`
sudo rm -rf /mnt/data1/pgodard/Services-test/test_tkcat_2025.04.18

```
