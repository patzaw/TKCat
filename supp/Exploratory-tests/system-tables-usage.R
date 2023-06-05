source("~/opt/KMT.R")

last_queries <- get_query(
   .tkcon,
   "
   SELECT user, event_date, event_time, read_rows, current_database,
      databases, tables
   FROM system.query_log ORDER BY event_time DESC
   LIMIT 100000
   "
)
last_queries %>%
   distinct(user, current_database, .keep_all = TRUE) %>% 
   View()

