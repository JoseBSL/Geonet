##removed hummingbird studies: M_PL_063,M_PL_065,  M_PL_067, M_PL_070 -in new folder

library(kgc)
library(tidyverse)
library(reshape)
library(taxize)

reference=read.csv("data/ref/references.csv",header=T)
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




# First apply read.csv, then rbind
path <- "data/"
files = list.files(pattern="*.csv")
myfiles = lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE, sep=","))

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



names(myfiles)=left(files,8)
names(myfiles)


myfiles.melt=lapply(myfiles, function(x) melt(x,id.vars=c(1)))
myfiles.melt=melt(myfiles,id.vars=c(1))
myfiles.melt.agg=aggregate(value ~ X + variable+L1,data=myfiles.melt, FUN=sum)
myfiles.melt.agg$value=ifelse(myfiles.melt.agg$value > 0, 1, 0)
colnames(myfiles.melt.agg)=c("Plant","Pollinator","Network","Int")

myfiles.melt.agg.z=myfiles.melt.agg[!myfiles.melt.agg$Int==0,]


<<<<<<< HEAD
plant_family=tax_name(query=word(myfiles.melt.agg.z$Plant,1),
=======
plant_family=tax_name(query=word(myfiles.melt.agg$Plant,1),
>>>>>>> f5ba93357a7cf93c4836066d2dad3832f622c465
                                       get=c("family"),db="ncbi", 
                                       division_filter = "Plantae",rank_query="genus")[3]

myfiles.melt.agg$Pollinator=gsub("\\."," ",myfiles.melt.agg$Pollinator)

<<<<<<< HEAD
myfiles.melt.agg[,c("Poll.Family","Poll.Order")]=tax_name(query=word(myfiles.melt.agg.z$Pollinator[1:10],1),
=======
myfiles.melt.agg[,c("Poll.Family","Poll.Order")]=tax_name(query=word(myfiles.melt.agg$Pollinator[1:10],1),
>>>>>>> f5ba93357a7cf93c4836066d2dad3832f622c465
                      get=c("family","order"),db="ncbi", 
                      division_filter = "Anthropoda",rank_query="genus")[3:4]



