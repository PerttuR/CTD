library(data.table)
library(tibble)
library(expss)
library(splitstackshape)
library(rLakeAnalyzer)
library (reshape2)
library(purrr)
library(dplyr)
library(readxl)
library(stringr)
library(sf)

rm(list = ls())

source("./db.R")

ewkb_to_sf <- function(data) {
  return(st_as_sfc(structure(data, class="WKB"), EWKB=T))
}

get.handler <- function() {
  return(read.dbTable("suomu","handler"))
}

get.trip <- function(year) {
  trip <- read.dbTable("suomu", "trip", paste0("year=", year, " AND project_fk=1"))
  if(nrow(trip) != 1) {
    stop(paste0("Year ", year, " matched multiple or no trip records"))
  }
  return(trip)
}

get.haul <- function(trip) {
  if(nrow(trip) != 1) {
    stop("Function argument should be exactly one trip")
  }
  haul <- read.dbTable("suomu", "haul", paste0("trip_fk = ", trip$id))
haul$start_location = ewkb_to_sf(haul$start_location)
haul$stop_location = ewkb_to_sf(haul$stop_location)
haul$geometry <- ifelse(
  st_is_empty(haul$start_location),
    haul$stop_location,
  ifelse(st_is_empty(haul$stop_location), haul$start_location, st_centroid(haul$start_location, haul$stop_location)))
  return(haul)
}

get.metadata <- function(trip, haul) {
  where <- paste0("trip_fk = ", trip$id,
    " or haul_fk in (", paste0(haul$id, collapse=","), ")")
  metadata <- read.dbTable("suomu", "ctd_metadata", where)
  return(metadata)
}

get.data <- function(metadata) {
  where <- paste0("ctd_metadata_fk in (",
    paste0(metadata$id, collapse=","), ")")
  data <- read.dbTable("suomu", "ctd_data", where)
  return(data)
}

wd <- getwd()
handler <- get.handler()

year <- 2022
trip <- get.trip(year)
haul <- get.haul(trip)
metadata <- get.metadata(trip, haul)
data <- get.data(metadata)

haul_metadata <- haul %>% left_join(metadata, by=join_by(id == haul_fk))
haul_metadata$trip_fk <- coalesce(haul_metadata$trip_fk.x, haul_metadata$trip_fk.y)
haul_metadata <- haul_metadata %>% select(-c(trip_fk.x, trip_fk.y))

haul_metadata_trip <- haul_metadata %>% left_join(trip, by=join_by(trip_fk == id))

mega <- haul_metadata_trip %>% left_join(data, by=join_by(id == ctd_metadata_fk))

result <- data.frame(matrix(nrow= nrow(mega), ncol=0))
result$cruise <- paste0(mega$year, mega$sample_number)
result$station <- mega$station_number

result$type <- "C"
result$"yyyy-mm-ddThh:mm:ss.sss" <- coalesce(mega$start_time.x, mega$stop_time)

result$latitude <- lapply(
  mega$geometry,
  function(geom) {return(st_coordinates(geom)[,2])})
result$longitude <- lapply(
  mega$geometry,
  function(geom) {return(st_coordinates(geom)[,1])})