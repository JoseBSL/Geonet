##removed hummingbird studies: M_PL_063,M_PL_065,  M_PL_067, M_PL_070 -in new folder

library(kgc)
library(tidyverse)
library(reshape)
library(taxize)

options(stringsAsFactors = FALSE)
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

reference[26,11]=c("BWh")
reference[60,11]=c("Af")




# First apply read.csv, then rbind

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


myfiles.melt.agg.z$Pollinator=gsub("\\."," ",myfiles.melt.agg.z$Pollinator)

myfiles.melt.agg.z.4=split(myfiles.melt.agg.z , f = c(rep(1,9555),rep(2,9555),rep(3,9554),rep(4,9554)))


#plant_family_1=tax_name(query=word(myfiles.melt.agg.z.4$`1`$Plant,1),
get=c("family"),db="ncbi", 
division_filter = "Plantae",rank_query="genus")

#plant_family_2=tax_name(query=word(myfiles.melt.agg.z.4$`2`$Plant,1),
                        get=c("family"),db="ncbi", 
                        division_filter = "Plantae",rank_query="genus")


#plant_family_3=tax_name(query=word(myfiles.melt.agg.z.4$`3`$Plant,1),
get=c("family"),db="ncbi", 
division_filter = "Plantae",rank_query="genus")
#plant_family_4=tax_name(query=word(myfiles.melt.agg.z.4$`4`$Plant,1),
                        get=c("family"),db="ncbi", 
                        division_filter = "Plantae",rank_query="genus")





poll_family_1=tax_name(query=word(myfiles.melt.agg.z.4$`1`$Plant,1),
get=c("family"),db="ncbi", 
division_filter = "Plantae",rank_query="genus")

poll_family_2=tax_name(query=word(myfiles.melt.agg.z.4$`2`$Plant,1),
get=c("family"),db="ncbi", 
division_filter = "Plantae",rank_query="genus")


poll_family_3=tax_name(query=word(myfiles.melt.agg.z.4$`3`$Plant,1),
get=c("family"),db="ncbi", 
division_filter = "Plantae",rank_query="genus")
poll_family_4=tax_name(query=word(myfiles.melt.agg.z.4$`4`$Plant,1),
get=c("family"),db="ncbi", 
division_filter = "Plantae",rank_query="genus")

sum(is.na(plant_family_2$family))



plant_family=tax_name(query=word(myfiles.melt.agg.z[myfiles.melt.agg.z$Network==c("M_PL_015"),]$Plant,1),
                                       get=c("family"),db="ncbi", 
                                       division_filter = "Plantae",rank_query="genus")




myfiles.melt.agg.z[,c("Poll.Family","Poll.Order")]=tax_name(query=word(myfiles.melt.agg.z$Pollinator[1:10],1),
                      get=c("family","order"),db="ncbi", 
                      division_filter = "Anthropoda",rank_query="genus")[3:4]



