##removed hummingbird studies: M_PL_063,M_PL_065,  M_PL_067, M_PL_070 -in new folder

library(kgc)
library(tidyverse)

reference=read.csv("data/references.csv",header=T)
str(reference)

climate_zone=reference[,c("ID","Longitude","Latitude")]
str(climate_zone)
climate_zone <- data.frame(climate_zone,
                   rndCoord.lon = RoundCoordinates(climate_zone$Longitude),
                   rndCoord.lat = RoundCoordinates(climate_zone$Latitude))
reference <- data.frame(reference,ClimateZ=LookupCZ(climate_zone))
table(reference$ClimateZ)
reference[reference$ClimateZ=="Climate Zone info missing",]
##GALAPAGOS and MAURITIUS

setwd("~/Dropbox/PhD/Rprojects/Geonet/data")

files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE, sep=","))
myfiles[[2]]

myfiles.melt=lapply(myfiles, function(x) melt(x,id.vars=c(1)))

<<<<<<< HEAD
       tax_name(query = colnames(myfiles[[1]])[2:10], get = "family", db = "ncbi")
=======
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
>>>>>>> cd5db2353ad1aefcddd93043350ff1611a027005
