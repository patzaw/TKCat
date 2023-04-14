source("~/opt/TKCat/tkcat-ucb.R")

b <- get_query(.tkcon, "select user, event_date, event_time, read_rows, current_database, databases, tables from system.query_log")
