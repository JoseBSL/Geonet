##re-imagining geonet 2021

library(kgc)
library(tidyverse)
library(reshape2)
library(taxize)
library(plyr)
library(dplyr)
library(stringi)
library(stringr)
library(tidyr)
library(glmmTMB)

options(stringsAsFactors = FALSE)

reference=read.csv("data/Networks_coordinates.csv",header=T)
colnames(reference)[1]="Network"


left = function (string,char){
  substr(string,1,char)
}

#reference$ID=left(reference$Network,8)
#reference=reference[!duplicated(reference$ID),]

climate_zone=reference[,c("Network","Long","Lat")]

climate_zone <- data.frame(climate_zone,
                   rndCoord.lon = RoundCoordinates(climate_zone$Long),
                   rndCoord.lat = RoundCoordinates(climate_zone$Lat))

reference <- data.frame(reference,ClimateZ=LookupCZ(climate_zone))

reference$clim <- substr(reference$ClimateZ,1,1)
reference$Network <- gsub("_"," ",reference$Network)
reference$Reference <- word(reference$Network,1)

# First apply read.csv, then rbind
setwd("~/Dropbox/Rprojects/Geonet/data/Networks")
files = list.files(pattern="*.csv")
myfiles = lapply(files, function(x) read.csv(x,stringsAsFactors = FALSE,
                                             #row.names=1,
                                             sep=","))

setwd("~/Dropbox/Rprojects/Geonet")

#Name list objects/networks
files.names=gsub(".csv","",files)
names(myfiles)=files.names
names(myfiles)

View(myfiles[[1]])
#Melt
myfiles.melt=melt(myfiles,id.vars=c(1))

myfiles.melt$value=ifelse(myfiles.melt$value > 0, 1, 0)
colnames(myfiles.melt)=c("Plant","Pollinator","Int","Network")
myfiles.melt$Network <- gsub("_"," ",myfiles.melt$Network)

myfiles.melt.z=myfiles.melt[!myfiles.melt$Int==0,]
myfiles.melt.z$Pollinator=gsub("\\."," ",myfiles.melt.z$Pollinator)
myfiles.melt.z$Pollinator=gsub("_"," ",myfiles.melt.z$Pollinator)

myfiles.melt.z$Plant=gsub("\\."," ",myfiles.melt.z$Plant)
myfiles.melt.z$Plant=gsub("_"," ",myfiles.melt.z$Plant)

#get plant and animal families
plant.family <- read.csv("data/processing/plant_famord.csv",sep=";")
poll.family <- read.csv("data/processing/poll_famord_5.csv",sep=";")

### Plant / Pollinator genera columns
myfiles.melt.z$plant.genus=word(myfiles.melt.z$Plant,1)
myfiles.melt.z$animal.genus=word(myfiles.melt.z$Pollinator,1)


colnames(plant.family)[2]="plant.genus"
colnames(poll.family)[1]="animal.genus"

#missing plants and animals
setdiff(myfiles.melt.z$animal.genus,poll.family$animal.genus)
setdiff(myfiles.melt.z$plant.genus,plant.family$plant.genus)

#add plant families
geonet=myfiles.melt.z%>%
  left_join(plant.family, by = "plant.genus")%>%
  left_join(poll.family,by="animal.genus")%>%
  left_join(reference, by = "Network")

geonet <- geonet %>% mutate_if(is.factor,as.character)
geonet[geonet$animal.family%in%c("Stenotritidae","Apidae","Andrenidae","Colletidae","Megachilidae","Melittidae","Halictidae"),c("animal.order")]="Bee"
geonet[geonet$animal.family%in%c("Syrphidae"),c("animal.order")]="Syrphidae"

#remove non-insect pollinators

phyla <- read.csv("data/processing/animal phyla.csv",row.names = 1)
colnames(phyla)[2:3] = c("animal.order","animal.phylum")

geonet2 <- geonet%>%
           left_join(phyla,by="animal.order")%>%
           filter(animal.phylum%in%c("Arthropoda",
                                     "Chordata"))

g=geonet2%>%
  mutate_if(is.character,as.factor)%>%
group_by(Network, animal.order) %>%
  summarise(order_links=sum(Int))

g2=g%>%
  group_by(Network) %>%
  summarise(int_tot=sum(order_links))

g3=merge(g,g2)

g3$prop_links=g3$order_links/g3$int_tot

g4=g3%>%
  left_join(reference,by="Network")

#subset data by insect order
g.sub <- subset(g4, animal.order %in% c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera"))
