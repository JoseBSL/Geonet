##removed hummingbird studies: M_PL_063,M_PL_065,  M_PL_067, M_PL_070 -in new folder

##Remove oil flower paper M_PL_059



library(kgc)
library(tidyverse)
library(reshape)
library(taxize)
library(plyr)
library(dplyr)
library(stringi)
library(stringr)
library(tidyr)
library(glmmTMB)

options(stringsAsFactors = FALSE)
reference=read.csv("data/ref/references_update.csv",header=T)
colnames(reference)[1]="Network"

left = function (string,char){
  substr(string,1,char)
}
#reference$ID=left(reference$Network,8)
#reference=reference[!duplicated(reference$ID),]


climate_zone=reference[,c("Network","Longitude","Latitude")]
climate_zone <- data.frame(climate_zone,
                           rndCoord.lon = RoundCoordinates(climate_zone$Longitude),
                           rndCoord.lat = RoundCoordinates(climate_zone$Latitude))
reference <- data.frame(reference,ClimateZ=LookupCZ(climate_zone))
reference[reference$ClimateZ=="Climate Zone info missing",]
reference$clim=left(reference$ClimateZ,1)
table(reference$clim)

##ELEVATION DATA
require(geonames)

#Two methods
options(geonamesUsername="liamkendall")

ele=by(reference, 1:nrow(reference), function(x) GNsrtm3(lat=x$Latitude,lng=x$Longitude))
ele=do.call("rbind", ele)

ele$Network=reference$Network

reference$ele=ele2$srtm3
colnames(ele)[1]="ele"
#http://www.gpsvisualizer.com/elevation
ele[ele$ele<0,]

ele[ele$Network %in% "M_PL_009",c("ele")] = 1009
ele[ele$Network %in%"M_PL_010",c("ele")] =  261
ele[ele$Network %in%"M_PL_014",c("ele")] = 250
ele[ele$Network %in%"M_PL_020",c("ele")] = 250
ele[ele$Network %in%"M_PL_024",c("ele")] = 92 
ele[ele$Network %in%"M_PL_026",c("ele")] = 1 #iffy
ele[ele$Network %in%"M_PL_045",c("ele")] = 1 #iffy
ele[ele$Network %in%"M_PL_052",c("ele")] = 139 
ele[ele$Network %in%"Robinson etal 2018",c("ele")] = 31 

reference$ele=ele$ele

# First apply read.csv, then rbind
setwd("~/Dropbox/PhD/Rprojects/Geonet/data")
files = list.files(pattern="*.csv")
myfiles = lapply(files, function(x) read.csv(x,stringsAsFactors = FALSE, sep=","))
setwd("~/Dropbox/PhD/Rprojects/Geonet")

reference$Network


#NAme list objects/networks
names(myfiles)=reference$Network[1:178]

##ADd zeros to Traveset
myfiles$`Traveset 2013`
myfiles$`Traveset 2013`[is.na(myfiles$`Traveset 2013`)] <- 0

#Melt
myfiles.melt=melt(myfiles,id.vars=c(1))
head(myfiles.melt)

#Aggregate - merges #not needed
#myfiles.melt.agg=aggregate(value ~ X + variable+L1,data=myfiles.melt, FUN=sum)
myfiles.melt$value=ifelse(myfiles.melt$value > 0, 1, 0)
head(myfiles.melt)
myfiles.melt=myfiles.melt[,-5]
colnames(myfiles.melt)=c("Plant","Pollinator","Int","Network")

#remove zeros
myfiles.melt.z=myfiles.melt[!myfiles.melt$Int==0,]
myfiles.melt.z$Pollinator=gsub("\\."," ",myfiles.melt.z$Pollinator)


##Merge with poll families
myfiles.melt.z$PGenus=word(myfiles.melt.z$Plant,1)
myfiles.melt.z$IGenus=word(myfiles.melt.z$Pollinator,1)

plant_famord=read.csv("data/processing/plant_famord.csv")
poll_famord=read.csv("data/processing/poll_famord_5.csv")

#check all accounted for
setdiff(myfiles.melt.z$PGenus,plant_famord$PGenus)
setdiff(myfiles.melt.z$IGenus,poll_famord$IGenus)

plant_famord=plant_famord[,c("family","PGenus","order")]
colnames(plant_famord)=c("PFamily","PGenus","POrder")

colnames(poll_famord)=c("IGenus","PolFamily","PolOrder")


setdiff(myfiles.melt.z$PGenus,plant_famord$PGenus)
geonet=merge(myfiles.melt.z,plant_famord, by = "PGenus") 
geonet=merge(geonet,poll_famord, by = "IGenus") 


geonet <- geonet %>% mutate_if(is.factor,as.character)

carvalheiro=read.csv("~/Dropbox/PhD/Rprojects/Geonet/data/newdata/formatted/special/carvalheiro_2_2008.csv")
str(geonet)
str(carvalheiro)
geonet=rbind(geonet,carvalheiro)

geonet[geonet$PolFamily%in%c("Stenotritidae","Apidae","Andrenidae","Colletidae","Megachilidae","Melittidae","Halictidae"),c("PolOrder")]="Bee"
geonet[geonet$PolFamily%in%c("Syrphidae"),c("PolOrder")]="Syrphidae"


g=geonet%>%
  group_by(Network, PolOrder) %>%
  summarise(order_links=sum(Int))

g2=g%>%
  group_by(Network) %>%
  summarise(int_tot=sum(order_links))

g3=merge(g,g2)
g3$prop_links=g3$order_links/g3$int_tot

g4=merge(g3,reference,by="Network")

#subset data by insect order
g.sub <- subset(g4, PolOrder %in% c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera"))
str(g.sub)

