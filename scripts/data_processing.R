##removed hummingbird studies: M_PL_063,M_PL_065,  M_PL_067, M_PL_070 -in new folder

install.packages("kgc")
library(kgc)

reference=read.csv("data/references.csv",header=T)
str(reference)

climate_zone=reference[,c("ID","Longitude","Latitude")]
str(climate_zone)
climate_zone <- data.frame(climate_zone,
                   rndCoord.lon = RoundCoordinates(climate_zone$Longitude),
                   rndCoord.lat = RoundCoordinates(climate_zone$Latitude))
climate_zone <- data.frame(climate_zone,ClimateZ=LookupCZ(climate_zone))
table(climate_zone$ClimateZ)

setwd("~/Dropbox/PhD/Rprojects/Geonet/data")

files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE, sep=","))
myfiles[[2]]

myfiles.melt=lapply(myfiles, function(x) melt(x,id.vars=c(1)))

#Read multiple CSVs
path <- "data/"
files <- list.files(path=path, pattern="*.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(sep=",",header=T,paste(path,file,sep="")))
}   
