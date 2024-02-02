library(data.table)
library(tibble)
library(expss)
library(reshape2)
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


haul_metadata <- metadata %>% left_join(haul, by=join_by(haul_fk == id))
haul_metadata$trip_fk <- coalesce(haul_metadata$trip_fk.x, haul_metadata$trip_fk.y)
haul_metadata <- haul_metadata %>% select(-c(trip_fk.x, trip_fk.y))

haul_metadata_trip <- haul_metadata %>% left_join(trip, by=join_by(trip_fk == id))

mega <- haul_metadata_trip %>% left_join(data, by=join_by(id == ctd_metadata_fk))

result <- data.frame(matrix(nrow= nrow(mega), ncol=0))
result$cruise <- paste0(mega$year, mega$sample_number)
result$station <- mega$station_number

result$type <- "C"
result$"yyyy-mm-ddThh:mm:ss.sss" <- coalesce(mega$start_time.x, mega$stop_time)

result$latitude <- unlist(lapply(
  mega$geometry,
  function(geom) {return(st_coordinates(geom)[,2])}))
result$longitude <- unlist(lapply(
  mega$geometry,
  function(geom) {return(st_coordinates(geom)[,1])}))

result$"Bot. Depth [m]" <- mega$bottom_depth

result$"Platform Code" <- "34A1" # TODO: get this from shipyear for Dana
result$"Device Category Code" <- mega$device_category_code
result$"Distributor Code" <- 5013 #Natural Resources Institute Finland (Main Office)
result$"Custodian Code" <- 5013

result$"Originator Code" <- 5013
result$"Project Code" <- 11272 #BIAS, TODO: project?
result$"Pressure [dbar] or Depth [m]" <- mega$pressure
result$"QV:ODV:Depth [m]" <- mega$pressure_qv

result$"Temperature [degC]" <- mega$temperature
result$"QV:ODV:Temperature [degC]" <- mega$temperature_qv
result$"Practical Salinity [dmnless]" <- mega$salinity_practical
result$"QV:ODV:Practical Salinity [dmnless]" <- mega$salinity_practical_qv
result$"Dissolved Oxygen [ml/l]" <- mega$oxygen_dissolved
result$"QV:ODV:Dissolved Oxygen [ml/l]" <- mega$oxygen_dissolved_qv

outdir <- paste0(c(wd, "out"), collapse="/")
dir.create(outdir, showWarnings = FALSE)
write.csv2(result, file=paste0(outdir, "/ODS-", year, ".csv"), na="", row.names=FALSE)