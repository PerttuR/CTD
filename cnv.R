library(stringr)
library(dplyr)

get.trip <- function(year) {
  trip <- read.dbTable("suomu", "trip", paste0("year=", year, " AND project_fk=1"))
  if(nrow(trip) != 1) {
    stop(paste0("Year ", year, " matched multiple or no trip records"))
  }
  return(trip)
}

# Note: setwd() breaks read.dbTable function
get.haul.map <- function(trip) {
  haul <- read.dbTable("suomu", "haul", paste0("trip_fk=", trip$id))
  return(setNames(haul$id, haul$location_description))
}

get.handler <- function() {
  return(read.dbTable("suomu","handler"))
}

str_match_wrapper <- function(x, pattern) {
  df <- data.frame(str_match(x, pattern))
  return(na.omit(df))
}

coordinate.fix <- function(coordinate) {
  arr <- unlist(strsplit(coordinate, " "))
  return(as.numeric(arr[[1]]) + as.numeric(arr[[2]]) / 60)
}

time.fix <- function(time) {
  arr <- unlist(strsplit(time, "\\.| "))
  result <- paste0(arr[[3]],"-",arr[[2]],"-",arr[[1]])
  if(length(arr) > 3) {
    result <- paste0(result, " ", arr[[4]])
  }
  return(result)
}

extract.metadata <- function(entry, trip, haul.map, handler) {
  metadata <- entry$metadata
  filename <- entry$file
  cols <- c(
    "id",
    "handler_fk",
    "trip_fk",
    "haul_fk",
    "ctd_calculation_time",
    "location",
    "bottom_depth",
    "device_category_code",
    "aranda_index",
    "ctd_device"
  )
  result <- data.frame(matrix(ncol=length(cols), nrow=1))
  names(result) <- cols

  rectangle <- str_match_wrapper(
    metadata,
    "\\*\\* Station name\\s*:\\s*(?<rectangle>.+)")$rectangle
  if(length(rectangle) == 0) {
    stop(paste0(filename, ": Could not find rectangle"))
  }
  if(rectangle %in% names(haul.map)) {
    result$haul_fk <- haul.map[rectangle]
  } else {
    warning(paste0(filename, ": Missing haul for rectangle "), rectangle)
  }

  if(is.na(result$haul_fk)) {
    result$trip_fk <- trip$id
  }
  result$handler_fk <- handler

  timeDf <- str_match_wrapper(metadata,
    "\\*\\* Date and time \\(UTC\\)\\s*:\\s*0*(?<time>.+)")
  result$ctd_calculation_time <- time.fix(timeDf$time)

  longitudeDf <- str_match_wrapper(metadata,
    "\\*\\* Latitude\\s*:\\s*0*(?<longitude>[0-9]{1,2} [0-9]{1,2}(.[0-9]+)?)")
  latitudeDf <- str_match_wrapper(metadata,
    "\\*\\* Longitude\\s*:\\s*0*(?<latitude>[0-9]{1,2} [0-9]{1,2}(.[0-9]+)?)")

  result$location <- paste0("POINT (",
    coordinate.fix(latitudeDf$latitude),
    " ",
    coordinate.fix(longitudeDf$longitude),
    ")"
  )

  depthDf <- str_match_wrapper(metadata, "\\*\\* Depth\\s*:\\s*0*(?<depth>[0-9]+)")
  result$bottom_depth <- depthDf$depth

  #static code
  result$device_category_code <- 130

  indexDf <- str_match_wrapper(metadata, "\\*\\* Index\\s*:\\s*0*(?<index>[0-9]+)")
  result$aranda_index <- indexDf$index

  deviceDf <- str_match_wrapper(metadata, "\\* (?<device>.+) Data File\\s*:\\s*")
  result$ctd_device <- deviceDf$device

  return(result)
}

rename.data <- function(data) {
  name.map <- c(
    "prdM" = "pressure",
    "tv290C" = "temperature",
    "t068C" = "temperature",
    "t168C" = "temperature_qv",
    "c0mS/cm" = "conductivity",
    "c1mS/cm" = "conductivity_qv",
    "sal00" = "salinity_practical",
    "sal11" = "salinity_practical_qv",
    "sigma-t00" = "density",
    "depSM" = "depth",
    "svCM" = "sound_velocity",
    "sbeox0PS" = "oxygen_saturation",
    "sbeox1PS" = "oxygen_saturation_qv",
    "sbeox0ML/L" = "oxygen_dissolved",
    "sbeox1ML/L" = "oxygen_dissolved_qv",
    "sigma-Θ00" = "density"
  )
  data.names <- names(data)
  for(i in 1:length(data.names)) {
    data.names[[i]] <- ifelse(
      is.na(name.map[data.names[[i]]]),
      data.names[[i]],
      name.map[data.names[[i]]]
    )
  }
  names(data) <- data.names
  return(data)
}

read.cnv <- function(path) {
  filename <- basename(path)
  lines = readLines(path)
  for(row in seq_along(lines)) {
    if(lines[row] == "*END*") {
      #TODO: check that this split works
      metadata <- na.omit(lines[1:(row - 1)])
      Encoding(metadata) <- "UTF-8"
      metadata <- str_replace_all(metadata, "\xe9", "Θ")
      data <- na.omit(lines[-(1:row)])
      data <- read.table(textConnection(paste(data, collapse="\n")))
      dataNames <- data.frame(str_match(metadata, "name (?<number>[0-9]+) = (?<name>.+): ?(?<comment>.*)"))
      dataNames <- dataNames %>% filter(!is.na(number)) %>% mutate(number = as.numeric(number) + 1) %>% arrange(number)
      names(data) <- dataNames$name
      data <- rename.data(data)
      return(setNames(list(data, metadata, filename), c("data", "metadata", "file")))
    }
  }
  return(setNames(list(lines, NULL, filename), c("metadata", "data", "file")))
}

check.empty <- function(trip) {
  if(nrow(trip) != 1) {
    stop("trip dataframe must have exactly one row")
  }
  metadata <- read.dbTable("suomu", "ctd_metadata", paste0("trip_fk=", trip$id))
  if(nrow(metadata) != 0) {
    stop(paste0("data exists for year ", trip$year))
  }
}