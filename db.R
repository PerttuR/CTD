library(RPostgres);

read.dbTable <- function(schema, table, where = NA, dbname = NULL) {
  where = ifelse(is.na(where),"",paste0(" WHERE ", where))
  set_utf8 = function(x){
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
  #Some validation
  if(length(c(table)) != 1 || length(c(schema)) != 1) {
    return(NULL);
  }
  if(!grepl("^[-[:digit:][:alpha:]_]+$", table)) {
    return(NULL);
  }
  if(!grepl("^[-[:digit:][:alpha:]_]+$", schema)) {
    return (NULL);
  }
  #End validation
  tmp <- new.env();
  source("db_params.R", local=tmp);
  drv <- RPostgres::Postgres();
  resolved_dbname = ifelse(is.null(dbname), tmp$dbname, dbname);
  con <- dbConnect(drv, dbname = resolved_dbname,
                   host = tmp$host, tmp$port,
                   user = tmp$user, password = tmp$password)
  rm(tmp);
  data <- set_utf8(dbGetQuery(con, paste0('SELECT * from "',schema,'".',table,where)))
  dbDisconnect(con);
  rm(con);
  return(data);
}

query.dbTable <- function(schema, table, dbname = NULL, query ) {
  set_utf8 = function(x){
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
  #Some validation
  if(length(c(table)) != 1 || length(c(schema)) != 1) {
    return(NULL);
  }
  if(!grepl("^[-[:digit:][:alpha:]_]+$", table)) {
    return(NULL);
  }
  if(!grepl("^[-[:digit:][:alpha:]_]+$", schema)) {
    return (NULL);
  }
  #End validation
  tmp <- new.env();
  source("db_params.R", local=tmp);
  drv <- RPostgres::Postgres();
  resolved_dbname = ifelse(is.null(dbname), tmp$dbname, dbname);
  con <- dbConnect(drv, dbname = resolved_dbname,
                   host = tmp$host, tmp$port,
                   user = tmp$user, password = tmp$password);
  rm(tmp);
  data <- set_utf8(dbGetQuery(con, query));
  dbDisconnect(con);
  rm(con);
  return(data);
}
















