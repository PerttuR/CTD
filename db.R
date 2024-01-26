library(RPostgres);

set_utf8 <- function(x){
  # Declare UTF-8 encoding on all character strings:
  for(i in 1:ncol(x)){
    if(is.character(x[, i])) {
      Encoding(x[, i]) <- "UTF-8"
    }
  }
  # Same on column names:
  for(name in colnames(x)){
    Encoding(name) <- "UTF-8"
  }
  return(x)
}

validate.params <- function(schema, table) {
  if(length(c(table)) != 1 || length(c(schema)) != 1) {
    stop("missing schema or table name");
  }
  if(!grepl("^[-[:digit:][:alpha:]_]+$", table)) {
    stop(paste("bad table name", table));
  }
  if(!grepl("^[-[:digit:][:alpha:]_]+$", schema)) {
    stop(paste("bad schema name", schema));
  }
}

read.dbTable <- function(schema, table, where = NA, dbname = NULL) {
  validate.params(schema, table)
  where = ifelse(is.na(where),"",paste0(" WHERE ", where))
  tmp <- new.env()
  source("db_params.R", local=tmp)
  drv <- RPostgres::Postgres()
  resolved_dbname <- ifelse(is.null(dbname), tmp$dbname, dbname)
  con <- dbConnect(drv, dbname = resolved_dbname,
                   host = tmp$host, tmp$port,
                   user = tmp$user, password = tmp$password)
  rm(tmp)
  data <- set_utf8(dbGetQuery(con, paste0('SELECT * from "',schema,'".',table,where)))
  dbDisconnect(con)
  rm(con)
  return(data)
}

query.dbTable <- function(schema, table, dbname = NULL, query ) {
  validate.params(schema, table)
  tmp <- new.env()
  source("db_params.R", local=tmp)
  drv <- RPostgres::Postgres()
  resolved_dbname <- ifelse(is.null(dbname), tmp$dbname, dbname)
  con <- dbConnect(drv, dbname = resolved_dbname,
                   host = tmp$host, tmp$port,
                   user = tmp$user, password = tmp$password)
  rm(tmp)
  data <- set_utf8(dbGetQuery(con, query))
  dbDisconnect(con)
  rm(con)
  return(data)
}

write.dbTable <- function(schema, table, data, dbname = NULL) {
  validate.params(schema, table)
  tmp <- new.env()
  source("db_params.R", local=tmp)
  drv <- RPostgres::Postgres()
  resolved_dbname <- ifelse(is.null(dbname), tmp$dbname, dbname)
  con <- dbConnect(drv, dbname = resolved_dbname,
                   host = tmp$host, tmp$port,
                   user = tmp$user, password = tmp$password)
  query.for.ids <- paste0(
    'select nextval(\'suomu.hibernate_sequence\') as id from generate_series(1,'
    ,nrow(data),
    ');'
  )
  rm(tmp)
  data$id <- dbGetQuery(con, query.for.ids)$id
  dbAppendTable(con, Id(schema=schema, table=table), data)
  dbDisconnect(con)
  rm(con)
}
