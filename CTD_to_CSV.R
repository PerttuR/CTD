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
rm(list = ls())

#Get haul map

source("./db.R")
source("./cnv.R")

########## create a 2023 CTD data file
#PATHS
# data folder = where the textfiles exist
# data <- paste0(getwd(), .Platform$file.sep, "2023 data/") #Location where you store original unmodified data
wd <- getwd()
handler <- get.handler()

#parsittava vuosi / year to be parsed
trip <- get.trip(2023) 
#tarkistaa onko metadata jo olemassa / check if data already exist
check.empty(trip)
#map haul ID from SUOMU DB
map_2023 <- get.haul.map(trip)
#where files exist
wd_2023 <- paste0(wd,"/2023 data")
#
out_2023 <- paste0(wd_2023, .Platform$file.sep, "out/") # folder where outputs are written
cnvFiles <- list.files(wd_2023)
parsed <- lapply(cnvFiles, function(el) {return(read.cnv(paste0(wd_2023, "/",el)))})
parsed <- lapply(parsed, function(el) {
  el$extracted <- extract.metadata(el$metadata, trip, map_2023, handler)
  return(el)
})

parsed_pivot <- list()
parsed_pivot$extracted <- lapply(parsed, function(el) {return(el$extracted)})
parsed_pivot$data <- lapply(parsed, function(el) {return(el$data)})
parsed_pivot$metadata <- lapply(parsed, function(el) {return(el$metadata)})

parsed_pivot$extracted <- bind_rows(parsed_pivot$extracted)

metadata.with.ids <- write.dbTable("suomu", "ctd_metadata", parsed_pivot$extracted)

for(i in 1:length(parsed_pivot$data)) {
  parsed_pivot$data[[i]]$ctd_metadata_fk <- metadata.with.ids[i, "id"]
}

parsed_pivot$data <- bind_rows(parsed_pivot$data)
data.table.columns <- c("ctd_metadata_fk",
    "pressure",
    "pressure_qv",
    "temperature",
    "temperature_qv",
    "salinity_practical",
    "salinity_practical_qv",
    "oxygen_dissolved",
    "oxygen_dissolved_qv",
    "oxygen_saturation",
    "oxygen_saturation_qv",
    "conductivity",
    "conductivity_qv",
    "sound_velocity",
    "depth",
    "density"
)
data.omit.columns <- parsed_pivot$data %>% select(any_of(data.table.columns))
if(any(is.na(data.omit.columns$ctd_metadata_fk))) {
  stop("metadata id reference would be null for some columns")
}
skip.data <- write.dbTable("suomu", "ctd_data", data.omit.columns)

cat(
  "Wrote ",
  nrow(metadata.with.ids),
  "rows of metadata and",
  nrow(skip.data),
  "rows of data.\n")