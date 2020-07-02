----------------------------------------
# Installation of underlying databases

The DBMS supporting TKCat are installed using the following shell script:
	
	- **S01-install-clickhouse-docker.sh**

The data are stored in the following folder:
	
	- `$TBKM_HOME`: **/data/pgodard/Projects/TKCat_UCB_TBN**

----------------------------------------
# Initialize and configure TKCat instance

The initialization and configuration procedure is described here:
	
	- **S11-Init-Config.R**

----------------------------------------
# Cleaning and removing TKCat instance

Stop and remove the docker containers.

```{sh, eval=FALSE}
docker stop ucb_tbn_tkcat
docker rm ucb_tbn_tkcat
docker volume prune -f
# Remove the folder with all the data: `$TKCAT_HOME`.`
sudo rm -rf ~/Documents/Projects/TKCat_UCB_TBN
```
