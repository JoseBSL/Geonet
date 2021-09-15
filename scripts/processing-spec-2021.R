#################################################
#correct number of links for sampling effort with Bascompte null model
#################################################
library(brms)
library(reshape2)
library(vegan)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

sp.links.melt <- geonet2 %>%
  group_by(Network, animal.order,Pollinator) %>%
  summarise(value=sum(Int))

sp.links.melt.2=sp.links.melt%>%
  group_by(Network) %>%
  summarise(int_tot=sum(value))

sp.links.melt.3=merge(sp.links.melt,sp.links.melt.2)

sp.links.melt.4=merge(sp.links.melt.3,reference,by="Network")

ord <- c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera")
sp.links.melt.5 <- dplyr::filter(sp.links.melt.4, animal.order %in% ord)%>%
droplevels()

#################################################
#END
#################################################