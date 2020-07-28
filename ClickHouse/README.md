----------------------------------------
# Install ClickHouse, initialize and configure the TKCat instance

The ClickHouse docker container supporting TKCat,
its initialization and its configuration procedures
are implemented here:
	
	- **S01-install-and-init.R**
	
The data are stored in the `TBKM_HOME` folder.

----------------------------------------
# Cleaning and removing a TKCat instance

Stop and remove the docker containers.

```{sh, eval=FALSE}

docker stop ucb_tbn_tkcat
docker rm ucb_tbn_tkcat
docker volume prune -f
# Remove the folder with all the data: `$TKCAT_HOME`.`
sudo rm -rf ~/Documents/Projects/TKCat_UCB_TBN

```
