library(data.table)
library(tibble)
library(expss)
library(splitstackshape)
library(rLakeAnalyzer)
library (reshape2)
library(dplyr)
library(readxl)
library(stringr)

rm(list = ls())

#Get haul map

source("./db.R")
source("./cnv.R")


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

extract.metadata <- function(metadata, trip, haul.map, handler) {
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

  result$trip_fk <- trip$id

  rectangle <- str_match_wrapper(
    metadata,
    "\\*\\* Station name ?:(?<rectangle>.+)")$rectangle
  result$haul_fk <- haul.map[rectangle]

  timeDf <- str_match_wrapper(metadata,
    "\\*\\* Date and time \\(UTC\\):0*(?<time>.+)")
  result$ctd_calculation_time <- time.fix(timeDf$time)

  longitudeDf <- str_match_wrapper(metadata,
    "\\*\\* Latitude:0*(?<longitude>[0-9]{2} [0-9]{2}(.[0-9]+)?)")
  latitudeDf <- str_match_wrapper(metadata,
    "\\*\\* Longitude:0*(?<latitude>[0-9]{2} [0-9]{2}(.[0-9]+)?)")

  result$location <- paste0("POINT(", coordinate.fix(longitudeDf$longitude),
    " ",coordinate.fix(latitudeDf$latitude), ")")

  depthDf <- str_match_wrapper(metadata, "\\*\\* Depth:0*(?<depth>[0-9]+)")
  result$bottom_depth <- depthDf$depth

  #static code
  result$device_category_code <- 130

  indexDf <- str_match_wrapper(metadata, "\\*\\* Index:0*(?<index>[0-9]+)")
  result$aranda_index <- indexDf$index

  deviceDf <- str_match_wrapper(metadata, "\\* (?<device>.+) Data File:")
  result$ctd_device <- deviceDf$device

  return(result)
}

########## create a 2023 CTD data file
#PATHS
# data folder = where the textfiles exist
# data <- paste0(getwd(), .Platform$file.sep, "2023 data/") #Location where you store original unmodified data
wd <- getwd()
handler <- get.handler()

trip <- get.trip(2023)
map_2023 <- get.haul.map(trip)
wd_2023 <- paste0(wd,"/2023 data")
out_2023 <- paste0(wd_2023, .Platform$file.sep, "out/") # folder where outputs are written
cnvFiles <- list.files(wd_2023)
first <- read.cnv(paste0(wd_2023, "/",cnvFiles[[1]]))
extract.metadata(first$metadata, trip, map_2023, handler)





dat<-data.frame(matrix(nrow=43, ncol=9))
rownames(dat)<-243:282
colnames(dat)<-c("rect","lat","lon","date","time","SST","SBT","MLD","TCI")





for(i in 243:282){#i=376
  df <- tibble(lines = readLines (paste ("aranda0",i,"a.cnv", sep="")))
  posit<-df[9:12,]
  o<-cSplit(posit, 'lines', sep=" ", type.convert=FALSE)
  rect<-gsub('name:','',o[1,3])
  lat<-as.numeric(gsub('Latitude:','',o[2,2]))+as.numeric(o[2,3])/60
  lon<-as.numeric(gsub('Longitude:','',o[3,2]))+as.numeric(o[3,3])/60
  date <-gsub('(UTC):','',o[4,5], fixed=TRUE)
  time<-as.character(o[4,6])
  df<-df[243:nrow(df),]
  dfnew<-cSplit(df, 'lines', sep=" ", type.convert=FALSE)
  colnames(dfnew)<-c("Pressure","Temperature","Conductivity","Voltage","Oxygen","OxygenSat",
                     "ScansperBin","Salinity","Density","Depth","dm","sv","PoTemp","flag")
  dfnew$Temperature<-as.numeric(dfnew$Temperature)
  dfnew$Depth<-as.numeric(dfnew$Depth)
  SST<-dfnew$Temperature[1]
  SBT<-dfnew$Temperature[nrow(dfnew)]
  MLD<-thermo.depth(dfnew$Temperature,dfnew$Depth,Smin=0.1,seasonal=TRUE,index=FALSE,mixed.cutoff=1)
  dfnew$Delta<-c(0,diff(dfnew$Temperature))
  TCI<-max(abs(dfnew$Delta))
  dat[i-348+1,]<-c(rect,lat,lon,date,time,SST,SBT,MLD,TCI)
  write.csv(dat, file="CTD2023.csv")
}