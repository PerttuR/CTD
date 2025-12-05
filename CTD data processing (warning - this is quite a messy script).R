library(data.table)
library(tibble)
library(expss)
library(splitstackshape)
library(rLakeAnalyzer)
library (reshape2)
library(dplyr)
library(readxl)

rm(list = ls())

wd <- getwd()
########## create a 2017 Temperature data file ## DONE
setwd(paste0(wd,"/2017 data"))

dat<-data.frame(matrix(nrow=36, ncol=8))
rownames(dat)<-seq(2,72,by=2)
colnames(dat)<-c("lat","lon","date","time","SST","SBT","MLD","TCI")

for(i in seq(2,72,by=2)){ #i=36
  df <- tibble(lines = readLines (paste ("26da.2017.12.",i,".cnv", sep="")))
  posit<-df[11:13,]
  o<-cSplit(posit, 'lines', sep=" ", type.convert=FALSE)
  lat<-as.numeric(o[1,5])+as.numeric(o[1,6])/60
  lon<-as.numeric(o[2,5])+as.numeric(o[2,6])/60
  date<-paste(o[3,7],o[3,6],o[3,8])
  time<-as.character(o[3,9])
  df<-df[368:nrow(df),]
  dfnew<-cSplit(df, 'lines', sep=" ", type.convert=FALSE)
  dfnew<-dfnew[,c(3,4,6,8,10,12,16,18,26)]
  colnames(dfnew)<-c("Pressure","Temperature","Conductivity","Salinity","Density","Oxygen",
                     "OxygenSat","Depth","Chl")
  dfnew$Temperature <-as.numeric(dfnew$Temperature)
  dfnew$Salinity    <-as.numeric(dfnew$Salinity)
  dfnew$Depth       <-as.numeric(dfnew$Depth)
  SST<-dfnew$Temperature[1]
  SBT<-dfnew$Temperature[nrow(dfnew)]
  SSS<-dfnew$Salinity[1]
  SBS<-dfnew$Salinity[nrow(dfnew)]
  SSO<-dfnew$Oxygen[1]
  SBO<-dfnew$Oxygen[nrow(dfnew)]
  MLD<-thermo.depth(dfnew$Temperature,dfnew$Depth,Smin=0.1,seasonal=TRUE,index=FALSE,mixed.cutoff=1)
  dfnew$Delta<-c(0,diff(dfnew$Temperature))
  TCI<-max(abs(dfnew$Delta))
  dfnew$DeltaS<-c(0,diff(dfnew$Salinity))
  HCI<-max(abs(dfnew$DeltaS))
  sort_by(dfnew,by=DeltaS)
  HCD<-dfnew$Depth[nrow(dfnew)]
  dat[i/2,]<-c(lat,lon,date,time,SST,SBT,MLD,TCI)
  write.csv(dat, file="CTD2017.csv")
}

########## create a 2018 Temperature data file ## DONE
setwd(paste0(wd,"/2018 data"))
dat<-data.frame(matrix(nrow=81, ncol=9))
rownames(dat)<-1:81
colnames(dat)<-c("rect","lat","lon","date","time","SST","SBT","MLD","TCI")

for(i in 50:81){ #i=76
  df <- tibble(lines = readLines (paste ("a1800",i,"a.cnv", sep="")))
  posit<-df[22:25,]
  o<-cSplit(posit, 'lines', sep=" ", type.convert=FALSE)
  rect<-as.character(o[1,5])
  lat<-as.numeric(o[2,4])+as.numeric(o[2,5])/60
  lon<-as.numeric(o[3,4])+as.numeric(o[3,5])/60
  date<-as.character(o[4,7])
  time<-as.character(o[4,8])
  df<-df[330:nrow(df),]
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
  dat[i,]<-c(rect,lat,lon,date,time,SST,SBT,MLD,TCI)
  write.csv(dat, file="CTD2018.csv")
}

########## create a 2019 Temperature data file ## DONE
setwd(paste0(wd,"/2019 data"))
dat<-data.frame(matrix(nrow=660-617+1, ncol=9))
rownames(dat)<-617:660
colnames(dat)<-c("rect","lat","lon","date","time","SST","SBT","MLD","TCI")

for(i in 617:660){
  df <- tibble(lines = readLines (paste ("aranda0",i,"a.cnv", sep="")))
  posit<-df[9:12,]
  o<-cSplit(posit, 'lines', sep=" ", type.convert=FALSE)
  rect<-gsub(':','',o[1,4])
  lat<-as.numeric(gsub(':','',o[2,3]))+as.numeric(o[2,4])/60
  lon<-as.numeric(gsub(':','',o[3,3]))+as.numeric(o[3,4])/60
  date <-gsub(':','',o[4,6])
  time<-gsub(':','',o[4,7])
  df<-df[261:nrow(df),]
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
  dat[i-617+1,]<-c(rect,lat,lon,date,time,SST,SBT,MLD,TCI)
  write.csv(dat, file="CTD2019.csv")
  }

########## create a 2020 Temperature data file ## DONE
setwd(paste0(wd,"/2020 data"))
dat<-data.frame(matrix(nrow=47, ncol=9))
rownames(dat)<-204:250
colnames(dat)<-c("rect","lat","lon","date","time","SST","SBT","MLD","TCI")

for(i in 204:250){#i=216
  df <- tibble(lines = readLines (paste ("aranda0",i,"a.cnv", sep="")))
  posit<-df[9:12,]
  o<-cSplit(posit, 'lines', sep=" ", type.convert=FALSE)
  rect<-gsub(':','',o[1,4])
  lat<-as.numeric(gsub(':','',o[2,3]))+as.numeric(o[2,4])/60
  lon<-as.numeric(gsub(':','',o[3,3]))+as.numeric(o[3,4])/60
  date <-gsub(':','',o[4,6])
  time<-as.character(o[4,7])
  df<-df[261:nrow(df),]
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
  dat[i-204+1,]<-c(rect,lat,lon,date,time,SST,SBT,MLD,TCI)
  write.csv(dat, file="CTD2020.csv")
}

########## create a 2021 Temperature data file ## DONE
setwd(paste0(wd,"/2021 data"))
dat<-data.frame(matrix(nrow=45, ncol=9))
rownames(dat)<-385:429
colnames(dat)<-c("rect","lat","lon","date","time","SST","SBT","MLD","TCI")

for(i in 385:429){#i=416
  df <- tibble(lines = readLines (paste ("aranda0",i,"a.cnv", sep="")))
  posit<-df[9:12,]
  o<-cSplit(posit, 'lines', sep=" ", type.convert=FALSE)
  rect<-gsub(':','',o[1,4])
  lat<-as.numeric(gsub(':','',o[2,3]))+as.numeric(o[2,4])/60
  lon<-as.numeric(gsub(':','',o[3,3]))+as.numeric(o[3,4])/60
  date <-gsub(':','',o[4,6])
  time<-as.character(o[4,7])
  df<-df[261:nrow(df),]
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
  dat[i-385+1,]<-c(rect,lat,lon,date,time,SST,SBT,MLD,TCI)
  write.csv(dat, file="CTD2021.csv")
}

########## create a 2022 Temperature data file #DONE
setwd(paste0(wd,"/2022 data"))
dat<-data.frame(matrix(nrow=45, ncol=9))
rownames(dat)<-348:392
colnames(dat)<-c("rect","lat","lon","date","time","SST","SBT","MLD","TCI")

for(i in 348:392){#i=376
  df <- tibble(lines = readLines (paste ("aranda0",i,"a.cnv", sep="")))
  posit<-df[9:12,]
  o<-cSplit(posit, 'lines', sep=" ", type.convert=FALSE)
  rect<-gsub('name:','',o[1,3])
  lat<-as.numeric(gsub('Latitude:','',o[2,2]))+as.numeric(o[2,3])/60
  lon<-as.numeric(gsub('Longitude:','',o[3,2]))+as.numeric(o[3,3])/60
  date <-gsub('(UTC):','',o[4,5], fixed=TRUE)
  time<-as.character(o[4,6])
  df<-df[264:nrow(df),]
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
  write.csv(dat, file="CTD2022.csv")
}


########## create a 2013 Temperature data file
setwd(paste0(wd,"/2013 data"))
dat<-data.frame(matrix(nrow=34, ncol=10))
colnames(dat)<-c("lat","lon","lat2","lon2","date","time","SST","SBT","MLD","TCI")

for(i in c(1:3,5:35)){#i=28
  hf <- tibble(lines = readLines (paste ("rbr00",i,".hex", sep="")))
  posit<-hf[c(4,22),]
    o<-cSplit(posit, 'lines', sep=" ", type.convert=FALSE)
  lat<-as.numeric(o[2,3])+as.numeric(o[2,4])/60
  lon<-as.numeric(o[2,6])+as.numeric(o[2,7])/60
  lat2<-as.numeric(gsub('N','',o[2,3]))+as.numeric(gsub(',','',o[2,4]))/60
  lon2<-as.numeric(gsub('E','',o[2,5]))+as.numeric(gsub(',','',o[2,6]))/60
  date <-as.character(o[1,3])
  time<-as.character(o[1,4])
  
df <- read_excel(paste ("rbr00",i,".xls", sep=""))
df<-df[6:nrow(df),]
colnames(df)<-c("Timestamp","Conductivity","Temperature","Pressure",
                "Depth","Salinity","Density","soundspeed","d1","d2")
df<-subset(df,d2>0)
df$Temperature<-as.numeric(df$Temperature)
df$Depth<-as.numeric(df$Depth)
SST<-df$Temperature[1]
#df$dup<-duplicated(df, incomparables=FALSE, fromLast=FALSE, by=seq_along(Depth))
SBT<-subset(df,Depth==max(Depth))$Temperature

#cut head and tail
df<-subset(df,Depth>5)
df<-subset(df,Depth<(max(Depth)-5))
df$Delta<-c(0,diff(df$Temperature))
TCI<-max(abs(df$Delta))
MLD<-thermo.depth(df$Temperature,df$Depth,Smin=0.1,seasonal=TRUE,index=FALSE,mixed.cutoff=1)
  
  dat[i,]<-c(lat,lon,lat2,lon2,date,time,SST,SBT,MLD,TCI)
}
write.csv(dat, file="CTD2013.csv")

########## create a 2014 Temperature data file TO BE FIXED
setwd(paste0(wd,"/2014 data"))
dat<-data.frame(matrix(nrow=25, ncol=12))
rownames(dat)<-c(668:692)
colnames(dat)<-c("rect","lat","lon","lat2","lon2","lon3",# "lat3", 
                 "date","time","SST","SBT","MLD","TCI")

for(i in c(668:692)){#i=679
  hf <- tibble(lines = readLines (paste ("rbr0",i,".hex", sep="")))
  posit<-hf[c(4,22),]
  o<-cSplit(posit, 'lines', sep=" ", type.convert=FALSE)
  rect<-gsub(',','',o[2,2])
  lat<-as.numeric(gsub('N','',o[2,3]))+as.numeric(gsub(',','',o[2,4]))/60
  lon<-as.numeric(gsub('E','',o[2,5]))+as.numeric(gsub(',','',o[2,6]))/60
  lat2<-as.numeric(o[2,4])+as.numeric(o[2,5])/60
  lon2<-as.numeric(o[2,7])+as.numeric(o[2,8])/60
  lon3<-as.numeric(o[2,6])+as.numeric(o[2,7])/60
  #lat3<-paste(as.character(o[2,3]),as.character(o[2,4]), sep="D")
  #lon3<-paste(as.character(o[2,6]),as.character(o[2,7]), sep="D")
  date <-as.character(o[1,3])
  time<-as.character(o[1,4])
  
  df <- read_excel(paste ("rbr0",i,".xls", sep=""))
  df<-df[6:nrow(df),]
  colnames(df)<-c("Timestamp","Conductivity","Temperature","Pressure",
                  "Depth","Salinity","Density","soundspeed","d1","d2")
  df<-subset(df,d2>0)
  df$Temperature<-as.numeric(df$Temperature)
  df$Depth<-as.numeric(df$Depth)
  SST<-df$Temperature[1]
  #df$dup<-duplicated(df, incomparables=FALSE, fromLast=FALSE, by=seq_along(Depth))
  df$nrow<-1:nrow(df)
  maxnr<-subset(df,Depth==max(Depth))$nrow
  df<-df[1:maxnr,]
  SBT<-subset(df,Depth==max(Depth))$Temperature
  
  #cut head and tail
  df<-subset(df,Depth>5)
  df<-subset(df,Depth<(max(Depth)-5))
  df$Delta<-c(0,diff(df$Temperature))
  TCI<-max(abs(df$Delta))
  MLD<-thermo.depth(df$Temperature,df$Depth,Smin=0.1,seasonal=TRUE,index=FALSE,mixed.cutoff=1)
  
dat[i-668+1,]<-c(rect,lat,lon,lat2,lon2,lon3,#lat3,
             date,time,SST,SBT,MLD,TCI)
}
write.csv(dat, file="CTD2014.csv")

########## create a 2016 Temperature data file #DONE
setwd(paste0(wd,"/2016 data"))
dat<-data.frame(matrix(nrow=44, ncol=9))
rownames(dat)<-c(500:543)
colnames(dat)<-c("rect","lat","lon",#"lat2","lon2",
                 "date","time","SST","SBT","MLD","TCI")

for(i in c(500:543)){#i=530
  hf <- tibble(lines = readLines (paste ("rbr",i,".hex", sep="")))
  posit<-hf[c(4,22),]
  o<-cSplit(posit, 'lines', sep=" ", type.convert=FALSE)
  rect<- gsub(',','',o[2,2])
  lat<-as.numeric(o[2,3])+as.numeric(gsub('N/','',o[2,4]))/60
  lon<-as.numeric(o[2,5])+as.numeric(gsub('E','',o[2,6]))/60
  #lat3<-paste(as.character(o[2,3]),as.character(o[2,4]), sep="D")
  #lon3<-paste(as.character(o[2,6]),as.character(o[2,7]), sep="D")
  date <-as.character(o[1,3])
  time<-as.character(o[1,4])
  
  df <- read_excel(paste ("rbr",i,".xls", sep=""))
  df<-df[6:nrow(df),]
  colnames(df)<-c("Timestamp","Conductivity","Temperature","Pressure",
                  "Depth","Salinity","Density","soundspeed","d1","d2")
  df<-subset(df,d2>0)
  df$Temperature<-as.numeric(df$Temperature)
  df$Depth<-as.numeric(df$Depth)
  SST<-df$Temperature[1]
  SBT<-subset(df,Depth==max(Depth))$Temperature

  #cut head and tail
  df<-subset(df,Depth>5)
  df<-subset(df,Depth<(max(Depth)-5))
  df$Delta<-c(0,diff(df$Temperature))
  TCI<-max(abs(df$Delta))
  MLD<-thermo.depth(df$Temperature,df$Depth,Smin=0.1,seasonal=TRUE,index=FALSE,mixed.cutoff=1)
  
  dat[i-500+1,]<-c(rect,lat,lon,#lat2,lon2,lat3,lon3,
             date,time,SST,SBT,MLD,TCI)
}

write.csv(dat, file="CTD2016.csv")


########## create a 2015 Temperature data file #DONE
setwd(paste0(wd,"/2015 data"))
dat<-data.frame(matrix(nrow=19, ncol=9))
rownames(dat)<-c(576:594)
colnames(dat)<-c("rect","lat","lon",#"lat2","lon2",
                 "date","time","SST","SBT","MLD","TCI")

for(i in c(577:594)){#i=576
  hf <- tibble(lines = readLines (paste ("RBR 0",i,".hex", sep="")))
  posit<-hf[c(4,22),]
  o<-cSplit(posit, 'lines', sep=" ", type.convert=FALSE)
  rect<- gsub(',','',o[2,2])
  lat<-as.numeric(gsub('N','',o[2,3]))+as.numeric(gsub(',','',o[2,4]))/60
  lon<-as.numeric(gsub('E','',o[2,5]))+as.numeric(gsub(',','',o[2,6]))/60
  #lat3<-paste(as.character(o[2,3]),as.character(o[2,4]), sep="D")
  #lon3<-paste(as.character(o[2,6]),as.character(o[2,7]), sep="D")
  date <-as.character(o[1,3])
  time<-as.character(o[1,4])
  
  df <- read_excel(paste ("RBR 0",i,".xls", sep=""))
  df<-df[6:nrow(df),]
  colnames(df)<-c("Timestamp","Conductivity","Temperature","Pressure",
                  "Depth","Salinity","Density","soundspeed","d1","d2")
  df<-subset(df,d2>0)
  df$Temperature<-as.numeric(df$Temperature)
  df$Depth<-as.numeric(df$Depth)
  SST<-df$Temperature[1]
  SBT<-subset(df,Depth==max(Depth))$Temperature

  #cut head and tail
  df<-subset(df,Depth>5)
  df<-subset(df,Depth<(max(Depth)-5))
  df$Delta<-c(0,diff(df$Temperature))
  TCI<-max(abs(df$Delta))
  MLD<-thermo.depth(df$Temperature,df$Depth,Smin=0.1,seasonal=TRUE,index=FALSE,mixed.cutoff=1)
  
  dat[i-576+1,]<-c(rect,lat,lon,#lat2,lon2,lat3,lon3,
                   date,time,SST,SBT,MLD,TCI)
}

write.csv(dat, file="CTD2015.csv")

# Reset working directory
setwd(wd)
