################################################################################

###Access data - Create metadata for each tabble

################################################################################


###Load packages

pacman :: p_load(data.table, tidyverse, DBI, duckdb, connections)

###Open connection


con <- dbConnect(duckdb::duckdb())

## httpfs para HTTP/S3

dbExecute(con, "INSTALL httpfs;")
dbExecute(con, "LOAD httpfs;")
dbGetQuery(con, "SELECT * FROM duckdb_extensions() WHERE loaded = TRUE;")

r2_access  <- Sys.getenv("R2_ACCESS_KEY",  unset = Sys.getenv("AWS_ACCESS_KEY_ID"))
r2_secret  <- Sys.getenv("R2_SECRET_KEY",  unset = Sys.getenv("AWS_SECRET_ACCESS_KEY"))
r2_endpoint<- Sys.getenv("R2_ENDPOINT")
r2_bucket  <- Sys.getenv("R2_BUCKET")

# Set credenciais e endpoint (path-style para R2)
dbExecute(con, sprintf("SET s3_access_key_id='%s';", r2_access))
dbExecute(con, sprintf("SET s3_secret_access_key='%s';", r2_secret))
dbExecute(con, sprintf("SET s3_endpoint='%s';", r2_endpoint))
dbExecute(con, "SET s3_url_style='path';")  # R2 costuma requerer path-style

rm(r2_access, r2_secret, r2_endpoint, r2_bucket)

#connection_view(con)



