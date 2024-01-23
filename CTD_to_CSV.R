library(data.table)
library(tibble)
library(expss)
library(splitstackshape)
library(rLakeAnalyzer)
library (reshape2)
library(dplyr)
library(readxl)

rm(list = ls())

#Get haul map

source("./db.R")

# Note: setwd() breaks read.dbTable function
getHaulMap <- function(year) {
  trip <- read.dbTable("suomu", "trip", paste0("year=", year, " AND project_fk=1"))
  if(nrow(trip) != 1) {
    stop(paste0("Year ", year, " matched multiple or no trip records"))
  }
  haul <- read.dbTable("suomu", "haul", paste0("trip_fk=", trip$id))
  return(setNames(haul$id, haul$location_description))
}

readCnvFile <- function(path) {
  lines = readLines(path)
  for(row in 1:length(lines)) {
    if(lines[row] == '*END*') {
      #TODO: check that this split works
      metadata <- na.omit(lines[1:row])
      data <- na.omit(lines[row+1:length(lines)])
      return(setNames(list(data, metadata), c("data", "metadata")))
    }
  }
  return(setNames(list(lines, NULL), c("metadata", "data")))
}

########## create a 2023 CTD data file
#PATHS
# data folder = where the textfiles exist
# data <- paste0(getwd(), .Platform$file.sep, "2023 data/") #Location where you store original unmodified data
wd <- getwd()
map_2023 <- getHaulMap(2023)
wd_2023 <- paste0(wd,"/2023 data")
out_2023 <- paste0(wd_2023, .Platform$file.sep, "out/") # folder where outputs are written
cnvFiles <- list.files(wd_2023)
first <- readCnvFile(paste0(wd_2023, "/",cnvFiles[[1]]))

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