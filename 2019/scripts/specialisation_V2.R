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
library(glmmTMB)
library(emmeans)

#cast long format dataframe
geonet$bin <- geonet$Int[geonet$Int > 0] <- 1
sp.links.melt <- geonet %>%
  group_by(Network, PolOrder,Pollinator) %>%
  summarise(value=sum(bin))
sp.links.melt.2=sp.links.melt%>%
  group_by(Network) %>%
  summarise(int_tot=sum(value))

sp.links.melt.3=merge(sp.links.melt,sp.links.melt.2)

sp.links.melt.4=merge(sp.links.melt.3,reference,by="Network")

ord <- c("Hymenoptera", "Bee","Diptera", "Lepidoptera","Syrphidae","Coleoptera")
sp.links.melt.5 <- dplyr::filter(sp.links.melt.4, PolOrder %in% ord)


#M_PL_062_00 has very large number of links for all species...456 - seems suspicious 

#################################################
#END
#################################################