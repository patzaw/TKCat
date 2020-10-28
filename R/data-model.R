## Data models ----
DEFAULT_DATA_MODEL <- ReDaMoR::read_json_data_model(
   system.file(
      "ClickHouse", "Data-Model", "chTKCat-default-data-model.json",
      package="TKCat"
   )
)

CHMDB_DATA_MODEL <- ReDaMoR::read_json_data_model(
   system.file(
      "ClickHouse", "Data-Model", "chTKCat-MDB-data-model.json",
      package="TKCat"
   )
)
