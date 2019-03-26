----------------------------------------
# Installation of underlying databases

The DBMS supporting TKCat are installed using the following shell script:
	
	- **S01-install-clickhouse-docker.sh**

The data are stored in the following folder:
	
	- `$TBKM_HOME`: **/data/pgodard/Projects/UCB_NM_TKcat**

----------------------------------------
# Initialize and configure TKCat instance

The initialization and configuration procedure is described here:
	
	- **S11-Init-Config.R**

----------------------------------------
# Cleaning and removing TKCat instance

Stop and remove the docker containers.

```{sh, eval=FALSE}
docker stop ucb_nm_tkcat
docker rm ucb_nm_tkcat
docker volume prune
```

Remove the folder with all the data: `$TKCAT_HOME`.

```
sudo rm -rf ~/Documents/Projects/UCB_NM_TKcat
```
